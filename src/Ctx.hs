{-# LANGUAGE TemplateHaskell #-}

module Ctx where

import           Control.Concurrent
import           Control.Exception  (finally)
import           Control.Lens
import           Control.Monad

import qualified Network.WebSockets as WS

import           Cmd

data CtxRender = CtxRender {
      _timeLength  :: Int,
      _timeStep    :: Double,
      _currentTime :: Double,
      _line0       :: [(Double, Double)],
      _line1       :: [(Double, Double)]
    }
makeLenses ''CtxRender

initRender :: Int -> CtxRender
initRender n =
    let
        dt = 1.0 / fromIntegral n
        xs = [0 .. n - 1]
        f g x = let t = dt * (fromIntegral x) in (t, g t)
        gr t = sin (2 * pi * t)
        gb t = cos (2 * pi * t)
    in
      CtxRender n dt (dt * fromIntegral n) (map (f gr) xs) (map (f gb) xs)

renderStep :: CtxRender -> CtxRender
renderStep ctx =
    let
        t = view currentTime ctx
        dt = view timeStep ctx
        gr = (t, sin (2 * pi * t))
        gb = (t, cos (2 * pi * t))
        ur xs = tail xs ++ [gr]
        ub xs = tail xs ++ [gb]
    in
      over currentTime (+ dt) . over line0 ur . over line1 ub $ ctx


render :: CtxRender -> Render
render ctx =
    let
        r = view line0 ctx
        b = view line1 ctx
        sr = ChSeries r "#D34747"
        sb = ChSeries b "#2B7E7E"
        ss = [sr, sb]
    in
      Render ss

data Ctx = Ctx {
      _ctxRender :: CtxRender
    }
makeLenses ''Ctx

step :: Ctx -> Ctx
step = over ctxRender renderStep

initCtx :: IO (MVar Ctx)
initCtx =
    do
      let
          n = 32
          ren = initRender n
      ctx <- newMVar (Ctx ren)
      return ctx

wsHandler :: MVar Ctx -> WS.ServerApp
wsHandler ctx pending =
    do
      conn <- WS.acceptRequest pending
      flip finally (return ()) $ forever $ do
        (WS.Text msg _) <- WS.receiveDataMessage conn
        let
            maybe_event = parseEvent msg
        case maybe_event of
          Left err    -> print err
          Right event -> handleEvent ctx conn event

handleEvent :: MVar Ctx -> WS.Connection -> WebEvent -> IO ()
handleEvent mctx conn event =
    do
      ctx <- takeMVar mctx
      let
          ren = render (view ctxRender ctx)
          resp = if event == Init then WebInit ren else WebRender ren
          ctx' = step ctx
      putMVar mctx ctx'
      WS.sendDataMessage conn (WS.Text (encodeCmd resp) Nothing)
