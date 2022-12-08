{-# LANGUAGE DeriveGeneric #-}
module I.Candle where
import GHC.Generics
import qualified Data.Aeson as A
import Data.Default.Class (Default (def))

data Candle = Candle
  { open :: !Double
  , close :: !Double
  , high :: !Double
  , low :: !Double
  , time :: !Integer
  } deriving (Generic,Eq,Show)

instance A.ToJSON Candle
instance A.FromJSON Candle

instance Default Candle where
  def = Candle 0 0 0 0 0

type PIFifthData = (Candle,Candle,Candle,Candle,Candle)



