{-# LANGUAGE OverloadedStrings #-}

-- | WebSocket protocol between the web client and the server.
--
--   Client -> Server (JSON object with a "type" field):
--
--   > {"type":"init"}                — initialize the game (or request state)
--   > {"type":"start"}               — start automatic stepping
--   > {"type":"pause"}               — pause automatic stepping
--   > {"type":"restart"}             — reset the game to its initial state
--   > {"type":"step"}                — advance one step manually
--   > {"type":"list"}                — list available rule files
--   > {"type":"select","file":...}   — load a rule file and restart
--
--   Legacy string events "Init" and "Step" are also accepted.
--
--   Server -> Client:
--
--   > {"type":"hello","rules":[...],"default":"rules/8885.rule"}
--   > {"type":"init","ruleSet":...,"step":n,"free":n,"finished":bool,
--      "result":...,"species":[["Имя","#цвет"],...],
--      "steps":[0..n],"series":{"Имя":[c0,c1,...],...},
--      "lifespans":{"Имя":[h0,h1,...],...}}
--   > {"type":"state","step":n,"free":n,"finished":bool,"result":...,
--      "population":{"Имя":n,...},"lifespans":{"Имя":[h0,h1,...],...}}
--   > {"type":"error","message":...}
--
--   @lifespans@ is the lifespan histogram of every species: @hk@ is the
--   number of chibiks of that species that died at age @k@ (accumulated over
--   the game, refreshed after every step).
module Cmd where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map             as M
import qualified Data.Text            as T

-- ---------------------------------------------------------------------------
-- Client messages

data ClientMsg =
      CMInit
    | CMStart
    | CMPause
    | CMRestart
    | CMStep
    | CMListRules
    | CMSelectRules T.Text
  deriving (Show, Eq)

instance FromJSON ClientMsg where
    parseJSON (String s) =
        case s of
          "Init" -> return CMInit
          "Step" -> return CMStep
          _      -> fail ("unknown legacy event: " ++ T.unpack s)
    parseJSON v = withObject "ClientMsg" (\o -> do
        t <- o .: "type"
        case t of
          "init"    -> return CMInit
          "start"   -> return CMStart
          "pause"   -> return CMPause
          "restart" -> return CMRestart
          "step"    -> return CMStep
          "list"    -> return CMListRules
          "select"  -> CMSelectRules <$> o .: "file"
          _         -> fail ("unknown command: " ++ T.unpack t)) v

parseClientMsg :: BL.ByteString -> Either String ClientMsg
parseClientMsg = eitherDecode'

-- ---------------------------------------------------------------------------
-- Server messages

data ServerMsg =
      SMHello { smRules :: [T.Text], smDefault :: T.Text }
    | SMInit  { smRuleSet  :: T.Text,
                smStep     :: Int,
                smFree     :: Int,
                smRunning  :: Bool,
                smFinished :: Bool,
                smResult   :: Maybe T.Text,
                smSpecies  :: [(T.Text, T.Text)],      -- (name, color)
                smSteps    :: [Int],                   -- step numbers
                smSeries   :: [(T.Text, [Int])],       -- per-species counts
                smLifespans :: M.Map T.Text [Int] }    -- age -> deaths per species
    | SMState { smStep       :: Int,
                smFree       :: Int,
                smRunning    :: Bool,
                smFinished   :: Bool,
                smResult     :: Maybe T.Text,
                smPopulation :: M.Map T.Text Int,      -- name -> count
                smLifespans  :: M.Map T.Text [Int] }   -- age -> deaths per species
    | SMError { smMessage :: T.Text }

instance ToJSON ServerMsg where
    toJSON (SMHello rules def) = object
        [ "type" .= ("hello" :: T.Text)
        , "rules" .= rules
        , "default" .= def
        ]
    toJSON (SMInit rs step free run fin res sps steps series lifespans) = object
        [ "type" .= ("init" :: T.Text)
        , "ruleSet" .= rs
        , "step" .= step
        , "free" .= free
        , "running" .= run
        , "finished" .= fin
        , "result" .= res
        , "species" .= sps
        , "steps" .= steps
        , "series" .= M.fromList series
        , "lifespans" .= lifespans
        ]
    toJSON (SMState step free run fin res pop lifespans) = object
        [ "type" .= ("state" :: T.Text)
        , "step" .= step
        , "free" .= free
        , "running" .= run
        , "finished" .= fin
        , "result" .= res
        , "population" .= pop
        , "lifespans" .= lifespans
        ]
    toJSON (SMError m) = object
        [ "type" .= ("error" :: T.Text)
        , "message" .= m
        ]

encodeMsg :: ServerMsg -> BL.ByteString
encodeMsg = encode
