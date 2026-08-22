{-# LANGUAGE OverloadedStrings #-}
-- | Дымовой тест протокола: подключается к серверу, печатает hello/init,
--   делает несколько шагов (state), переключает правила и перезапускает.
--   Проверять глазами: в init/state должно присутствовать поле "lifespans".
--   Сборка как build.sh; запуск: .build/wscheck
module Main where
import qualified Data.ByteString.Lazy as BL
import Network.WebSockets

main :: IO ()
main = runClient "127.0.0.1" 8000 "/ws" $ \conn -> do
    m0 <- recv conn
    putStrLn ("[hello] " ++ showFirst m0)
    m1 <- recv conn
    putStrLn ("[init]  " ++ showFirst m1)
    mapM_ (\_ -> do
              sendTextData conn ("{\"type\":\"step\"}" :: BL.ByteString)
              s <- recv conn
              putStrLn ("[state] " ++ showFirst s))
          [1 .. 4]
    sendTextData conn ("{\"type\":\"select\",\"file\":\"rules/triplet.rule\"}" :: BL.ByteString)
    m2 <- recv conn
    putStrLn ("[select] " ++ showFirst m2)
    sendTextData conn ("{\"type\":\"restart\"}" :: BL.ByteString)
    m3 <- recv conn
    putStrLn ("[restart] " ++ showFirst m3)
    putStrLn "WS-CHECK OK"

recv :: Connection -> IO BL.ByteString
recv conn = do
    m <- receiveDataMessage conn
    case m of
      Text bs _ -> return bs
      _         -> return BL.empty

-- | first 160 characters of a JSON message, single line
showFirst :: BL.ByteString -> String
showFirst bs = take 160 (map (\c -> if c == '\n' then ' ' else c) (show bs))
