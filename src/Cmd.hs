{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Cmd where

import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import           GHC.Generics


data WebEvent = Init | Step deriving (Generic, Show, Eq)

$(deriveJSON defaultOptions ''WebEvent)

parseEvent :: BL.ByteString -> Either String WebEvent
parseEvent = eitherDecode'

data ChSeries = ChSeries {
      seriesPoints :: [(Double, Double)],
      seriesColor  :: String
    } deriving (Generic)
$(deriveJSON defaultOptions ''ChSeries)

data Render = Render {
      series :: [ChSeries]
    } deriving (Generic)
$(deriveJSON defaultOptions ''Render)

data WebCmd = WebInit Render | WebRender Render
$(deriveJSON defaultOptions ''WebCmd)

--data WebCmd = ReloadPage | NoGig | ResponseM0 [CellSt0] | ResponseM1 Model3Render | SetValue Int

encodeCmd :: WebCmd -> BL.ByteString
encodeCmd = encode


