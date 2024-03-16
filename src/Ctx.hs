
module Ctx where

import           Control.Concurrent
import qualified Network.WebSockets as WS


data Ctx = Ctx

initCtx :: IO (MVar Ctx)
initCtx =
    do
      ctx <- newMVar Ctx
      return ctx

wsHandler :: MVar Ctx -> WS.ServerApp
wsHandler _ctx _pending = return ()

{-

    do
      let
          uname = view (userInfo . userName) u
      conn <- WS.acceptRequest pending
      flip finally (return ()) $ forever $ do
        (WS.Text msg _) <- WS.receiveDataMessage conn
        let
            maybe_event = parseEvent msg
        case maybe_event of
          Left err    -> print err
          Right event -> handleEvent ctx uname conn event

-}
