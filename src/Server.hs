
module Server where

import           Control.Concurrent
-- import           Control.Exception
import           Control.Monad.IO.Class

import qualified Data.ByteString.Conversion as BS

import           Network.Info               (ipv4)
import           Network.WebSockets.Snap    as WS

import           System.Directory

-- import           Text.Blaze.Html.Renderer.Utf8
-- import           Text.Blaze.Html5              as Hx

import qualified Snap.Blaze                 as Snap
import           Snap.Core                  (Snap)
import qualified Snap.Core                  as Snap
import qualified Snap.Http.Server           as Snap
import qualified Snap.Util.FileServe        as Snap

-- import           Html.Css
import           MainPage
import           Prepare

httpPort :: Int
httpPort = 8000

app :: MVar Ctx -> Snap ()
app ctx =
    do
      rou ctx

rou :: MVar Ctx -> Snap ()
rou ctx = Snap.route
      [ ("",               Snap.ifTop $ pageResponse ctx)
      , ("ws",             launchWS ctx)
      , ("js",             Snap.serveDirectory "public/js")
      , ("css",            Snap.serveDirectory "public/css/")
      , ("icon",           Snap.serveDirectory "public/icon/")
      , ("font",           Snap.serveDirectory "public/font")
    ]

pageResponse :: MVar Ctx -> Snap ()
pageResponse ctx =
    do
      p <- liftIO $ buildPage ctx
      Snap.blaze . renderPage $ p

launchWS :: MVar Ctx -> Snap ()
launchWS ctx = WS.runWebSocketsSnap (wsHandler ctx)

launch :: IO ()
launch =
    do
      ctx <- initCtx
      Snap.httpServe config (app ctx)
  where
    config  =
        Snap.setBind "127.0.0.1" $
--        Snap.setBind addr $
        Snap.setPort httpPort $
--        Snap.setErrorLog  Snap.ConfigNoLog $
--        Snap.setAccessLog Snap.ConfigNoLog $
        Snap.defaultConfig
