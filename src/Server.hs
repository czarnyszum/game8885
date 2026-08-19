{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Control.Concurrent

import           Network.WebSockets.Snap as WS

import qualified Snap.Blaze              as Snap
import           Snap.Core               (Snap)
import qualified Snap.Core               as Snap
import qualified Snap.Http.Server        as Snap
import qualified Snap.Util.FileServe     as Snap

import           Ctx
import           MainPage

httpPort :: Int
httpPort = 8000

app :: Ctx -> Snap ()
app ctx = rou ctx

rou :: Ctx -> Snap ()
rou ctx = Snap.route
      [ ("",               Snap.ifTop $ pageResponse ctx)
      , ("ws",             launchWS ctx)
      , ("js",             Snap.serveDirectory "public/js")
      , ("css",            Snap.serveDirectory "public/css/")
      , ("icon",           Snap.serveDirectory "public/icon/")
      , ("font",           Snap.serveDirectory "public/font")
    ]

pageResponse :: Ctx -> Snap ()
pageResponse _ctx = Snap.blaze mainPage

launchWS :: Ctx -> Snap ()
launchWS ctx = WS.runWebSocketsSnap (wsHandler ctx)

launch :: IO ()
launch = do
    rules <- listRuleFiles
    let defaultRule = if "rules/8885.rule" `elem` rules then "rules/8885.rule"
                      else if null rules then "rules/8885.rule" else head rules
    r <- initGame defaultRule
    gs <- case r of
            Left err -> error err
            Right g  -> return g
    gameVar <- newMVar gs
    clientsVar <- newMVar []
    let ctx = Ctx gameVar clientsVar 100 20000 rules defaultRule
    _ <- forkIO (ticker ctx)
    Snap.httpServe config (app ctx)
  where
    config =
        Snap.setBind "127.0.0.1" $
        Snap.setPort httpPort $
        Snap.defaultConfig
