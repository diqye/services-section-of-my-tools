{-# LANGUAGE OverloadedStrings,DeriveGeneric,TemplateHaskell #-}
module Finance.Candle where
import Control.Applicative((<|>),empty)
import Control.Monad(forM_,forM,foldM,guard)
import Control.Monad.IO.Class(liftIO,MonadIO)
import Control.Monad.Trans.Maybe(MaybeT(MaybeT),runMaybeT)
import qualified Control.Monad.Trans.State as State

import qualified Data.Aeson as A
import qualified Data.Time as Time
import qualified Data.Time.Clock.POSIX as TimeP
import Text.Printf(printf)

import Module.TemplateGetter
import GHC.Generics
import Data.Default.Class(Default)


data Candle = Candle
  { time :: Integer
  , open :: Double
  , high :: Double
  , low :: Double
  , close :: Double
  , volume :: Integer
  , ext :: String
  } deriving (Show,Generic)

instance A.ToJSON Candle
instance Default Candle
-- | 生成set_time over_time
mkSetAndOver ''Candle
