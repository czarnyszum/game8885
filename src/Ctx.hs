{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | Game context: the current game state, the WebSocket command handler and
--   the automatic stepping ticker.
module Ctx where

import           Control.Concurrent
import qualified Data.List             as L
import           Control.Exception  (SomeException, finally, try)
import           Control.Lens
import           Control.Monad

import qualified Data.Aeson            as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Network.WebSockets   as WS

import           System.Directory     (doesFileExist, listDirectory)
import           System.FilePath      (takeExtension)
import           System.Random        (mkStdGen, randomIO)

import           Cmd
import           Decl
import           Parser
import           Pattern
import           Sim
import           Species

-- ---------------------------------------------------------------------------
-- Game state

data GameState = GameState {
      _gsRuleSet  :: FilePath,
      _gsTables   :: Tables T.Text,
      _gsStep     :: Int,
      _gsHistory  :: [M.Map (Species T.Text) Int],   -- newest first
      _gsRunning  :: Bool,
      _gsFinished :: Bool,
      _gsResult   :: Maybe T.Text
    }
makeLenses ''GameState

-- | Shared context.
data Ctx = Ctx {
      _ctxGame      :: MVar GameState,
      _ctxClients   :: MVar [(Int, WS.Connection)],
      _ctxTickMs    :: Int,
      _ctxMaxHist   :: Int,
      _ctxRules     :: [FilePath],
      _ctxDefault   :: FilePath,
      _ctxConfig    :: FilePath   -- ^ persistent config file (last rule set)
    }

-- ---------------------------------------------------------------------------
-- Persistent config: remembers the last rule set chosen by the user

-- | Path of the persistent server config file (relative to the working
--   directory, i.e. the project root when started via run.sh).
configPath :: FilePath
configPath = ".game8885.conf"

-- | The persistent server config: the last rule set chosen by the user.
data Config = Config { _cfgLastRule :: Maybe FilePath } deriving Show

instance A.FromJSON Config where
    parseJSON = A.withObject "Config" $ \o -> Config <$> o A..:? "lastRule"

instance A.ToJSON Config where
    toJSON (Config r) = A.object [ "lastRule" A..= r ]

-- | Read the persisted last rule set (Nothing when absent or unreadable).
readConfig :: FilePath -> IO (Maybe FilePath)
readConfig path = do
    exists <- doesFileExist path
    if not exists then return Nothing
    else do
        bs <- BL.readFile path
        return (case A.decode bs :: Maybe Config of
                  Just (Config r) -> r
                  Nothing         -> Nothing)

-- | Persist the last rule set chosen by the user.
writeConfig :: FilePath -> FilePath -> IO ()
writeConfig path rule = BL.writeFile path (A.encode (Config (Just rule)))

-- ---------------------------------------------------------------------------
-- Initialization

-- | Load a rule file and create a fresh game.
initGame :: FilePath -> IO (Either String GameState)
initGame ruleFile = do
    content <- TIO.readFile ruleFile
    case parseRuleFile ruleFile content of
      Left err -> return (Left err)
      Right decls -> do
        seed <- randomIO :: IO Int
        case compileDecls decls (mkStdGen seed) of
          Left e   -> return (Left (show e))
          Right t0 -> do
              let tbl = set space (initialSpace t0) t0
                  hist = [snapshotOf tbl]
              return (Right (GameState ruleFile tbl 0 hist False False Nothing))

-- | Initial population snapshot (counts per species, for the history).
snapshotOf :: Tables T.Text -> M.Map (Species T.Text) Int
snapshotOf = fmap length . view (space.population)

-- | Find rule files in the rules/ directory (sorted for determinism).
listRuleFiles :: IO [FilePath]
listRuleFiles = do
    names <- listDirectory "rules"
    return (map ("rules/" ++) (L.sort (filter ((== ".rule") . takeExtension) names)))

-- ---------------------------------------------------------------------------
-- Stepping

-- | Perform one step; returns the updated state and the messages to broadcast.
stepGame :: GameState -> IO (GameState, [ServerMsg])
stepGame gs = do
    (mres, tables') <- runSim (view gsTables gs) stepTurn
    let stepN = view gsStep gs + 1
        sp = view space tables'
        e = view env tables'
        (finished, result) =
            case mres of
              Just r  -> (True, Just r)
              Nothing ->
                  let maxS = view envMaxSteps e
                  in if maxS > 0 && stepN >= maxS
                     then (True, Just ("Достигнут лимит шагов: " <> T.pack (show maxS)))
                     else (False, Nothing)
        hist = take (maxHistOf gs) (snapshotOf tables' : view gsHistory gs)
        gs' = GameState (view gsRuleSet gs) tables' stepN hist
                        (view gsRunning gs) finished result
        msg = SMState stepN (view freeSpace sp) (view gsRunning gs')
                        finished result (M.fromList (speciesCounts tables'))
                        (M.fromList (lifespanArrays tables'))
    return (gs', [msg])
  where
    maxHistOf _ = 20000

-- | Population of every species, in a stable order, as (name, count).
speciesCounts :: Tables T.Text -> [(T.Text, Int)]
speciesCounts tbl =
    let sp = view space tbl
        pop = view population sp
    in [ (speciesName tbl s, length (M.findWithDefault [] s pop)) | s <- view spAll tbl ]

-- | Lifespan histogram of every species as age-indexed arrays, in a stable
--   order: @[c0, c1, ...]@ where @ck@ is the number of chibiks of that
--   species that died at age @k@.
lifespanArrays :: Tables T.Text -> [(T.Text, [Int])]
lifespanArrays tbl =
    let h = view (space.histogram) tbl
    in [ (speciesName tbl s, ageArray (M.findWithDefault M.empty s h))
       | s <- view spAll tbl ]
  where
    ageArray m = case M.keys m of
        [] -> []
        ks -> [ M.findWithDefault 0 a m | a <- [0 .. maximum ks] ]

-- | (name, color) of every species, in a stable order.
speciesColors :: Tables T.Text -> [(T.Text, T.Text)]
speciesColors tbl =
    let col = view spColor tbl
        def = "#8f8f8f"
    in [ (speciesName tbl s, M.findWithDefault def s col) | s <- view spAll tbl ]

-- | Build the full init message (species, colors, history, lifespans).
initMsg :: GameState -> ServerMsg
initMsg gs =
    let tbl = view gsTables gs
        sp = view space tbl
        hist = reverse (view gsHistory gs)          -- chronological
        steps = [0 .. view gsStep gs]
        series = [ (name, [M.findWithDefault 0 s snap | snap <- hist])
                 | (name, s) <- namedSpecies tbl ]
    in SMInit (T.pack (view gsRuleSet gs))
              (view gsStep gs) (view freeSpace sp)
              (view gsRunning gs)
              (view gsFinished gs) (view gsResult gs)
              (speciesColors tbl) steps series
              (M.fromList (lifespanArrays tbl))
  where
    namedSpecies tbl' = [ (speciesName tbl' s, s) | s <- view spAll tbl' ]

-- | Build the current state message.
stateMsg :: GameState -> ServerMsg
stateMsg gs =
    let tbl = view gsTables gs
        sp = view space tbl
    in SMState (view gsStep gs) (view freeSpace sp)
               (view gsRunning gs)
               (view gsFinished gs) (view gsResult gs)
               (M.fromList (speciesCounts tbl))
               (M.fromList (lifespanArrays tbl))

-- ---------------------------------------------------------------------------
-- Broadcasting

-- | Send messages to all connected clients; drop dead connections.
broadcast :: Ctx -> [ServerMsg] -> IO ()
broadcast ctx msgs = do
    conns <- readMVar (_ctxClients ctx)
    let payloads = [ WS.Text (encodeMsg m) Nothing | m <- msgs ]
    results <- forM conns $ \(i, c) -> do
        r <- try (mapM_ (WS.sendDataMessage c) payloads)
        return (i, c, r :: Either SomeException ())
    let dead = [ i | (i, _, Left _) <- results ]
    unless (null dead) $
        modifyMVar_ (_ctxClients ctx) (return . filter ((`notElem` dead) . fst))

-- ---------------------------------------------------------------------------
-- Ticker: automatic stepping while running

ticker :: Ctx -> IO ()
ticker ctx = forever $ do
    threadDelay (fromIntegral (_ctxTickMs ctx) * 1000)
    msgs <- modifyMVar (_ctxGame ctx) $ \gs ->
        if view gsRunning gs && not (view gsFinished gs)
        then stepGame gs
        else return (gs, [])
    broadcast ctx msgs

-- ---------------------------------------------------------------------------
-- WebSocket handling

wsHandler :: Ctx -> WS.ServerApp
wsHandler ctx pending = do
    conn <- WS.acceptRequest pending
    myId <- modifyMVar (_ctxClients ctx) $ \cs ->
        let i = 1 + foldl (\m (j, _) -> max m j) 0 cs
        in return ((i, conn) : cs, i)
    flip finally (modifyMVar_ (_ctxClients ctx) (return . filter ((/= myId) . fst))) $ do
        gs <- readMVar (_ctxGame ctx)
        let hello = SMHello (map T.pack (_ctxRules ctx)) (T.pack (_ctxDefault ctx))
        WS.sendDataMessage conn (WS.Text (encodeMsg hello) Nothing)
        WS.sendDataMessage conn (WS.Text (encodeMsg (initMsg gs)) Nothing)
        forever $ do
            msg <- WS.receiveDataMessage conn
            case msg of
              WS.Text bs _ -> handleMsg ctx conn bs
              _ -> return ()

handleMsg :: Ctx -> WS.Connection -> BL.ByteString -> IO ()
handleMsg ctx conn bs =
    case parseClientMsg bs of
      Left err -> send conn (SMError (T.pack err))
      Right cmd -> case cmd of
        CMListRules -> send conn (SMHello (map T.pack (_ctxRules ctx)) (T.pack (_ctxDefault ctx)))
        CMInit -> readMVar (_ctxGame ctx) >>= send conn . initMsg
        CMStart -> do
            modifyMVar_ (_ctxGame ctx) (return . set gsRunning True)
            readMVar (_ctxGame ctx) >>= send conn . stateMsg
        CMPause -> modifyMVar_ (_ctxGame ctx) (return . set gsRunning False)
        CMStep -> do
            gs <- takeMVar (_ctxGame ctx)
            if view gsFinished gs
            then do
                putMVar (_ctxGame ctx) gs
                send conn (stateMsg gs)
            else do
                (gs', msgs) <- stepGame gs
                putMVar (_ctxGame ctx) gs'
                broadcast ctx msgs
        CMRestart -> do
            cur <- view gsRuleSet <$> readMVar (_ctxGame ctx)
            _ <- restartWith ctx conn cur
            return ()
        CMUpdate -> do
            -- re-read the current rule file from disk and restart with it
            cur <- view gsRuleSet <$> readMVar (_ctxGame ctx)
            _ <- restartWith ctx conn cur
            return ()
        CMSelectRules file
            | T.unpack file `elem` _ctxRules ctx -> do
                ok <- restartWith ctx conn (T.unpack file)
                -- remember the chosen rule set for the next server start
                when ok (writeConfig (_ctxConfig ctx) (T.unpack file))
            | otherwise -> send conn (SMError ("Неизвестный файл правил: " <> file))
  where
    send c m = WS.sendDataMessage c (WS.Text (encodeMsg m) Nothing)

-- | Restart the game with the given rule file and inform all clients.
--   The running/paused state is preserved. Returns True when the restart
--   succeeded (the rule file was parsed and compiled).
restartWith :: Ctx -> WS.Connection -> FilePath -> IO Bool
restartWith ctx conn ruleFile = do
    wasRunning <- view gsRunning <$> readMVar (_ctxGame ctx)
    r <- initGame ruleFile
    case r of
      Left e -> do
          send conn (SMError (T.pack e))
          return False
      Right gs -> do
          let gs' = set gsRunning wasRunning gs
          modifyMVar_ (_ctxGame ctx) (const (return gs'))
          broadcast ctx [initMsg gs']
          return True
  where
    send c m = WS.sendDataMessage c (WS.Text (encodeMsg m) Nothing)
