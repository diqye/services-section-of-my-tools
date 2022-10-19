{-# LANGUAGE TemplateHaskell,OverloadedStrings,DeriveDataTypeable #-}
module Module.Log where

import System.IO
import Control.Monad.IO.Class(liftIO,MonadIO)
import Data.Time.Format
import Data.Monoid((<>))
import qualified Data.Time as Time
import Language.Haskell.TH
import System.IO
import Debug.Trace(traceM)
import Data.Data(Data)
import Language.Haskell.TH.Syntax(liftData,liftString)
-- import Data.IORef

data LogLevel = LogErr | LogInfo | LogTrace
  deriving (Show,Data)

-- China standard time
cstZone = Time.TimeZone {Time.timeZoneMinutes=480,Time.timeZoneSummerOnly=False,Time.timeZoneName="CST"}

stdBuffer = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

hInfo :: (MonadIO m) => Maybe Loc -> LogLevel -> String -> m ()
hInfo loc level str = liftIO $ do
  utc <- Time.getCurrentTime
  let zone = cstZone
  let time = Time.utcToLocalTime zone utc
  let (prefix,handle) = case level of
        LogErr -> ("err",stderr)
        LogTrace -> ("trace",stdout)
        LogInfo -> ("info",stdout)
  hPutStrLn handle $ (prefn loc prefix (formatTime defaultTimeLocale ("[%Y-%m-%d %H:%M:%S]") time) <> str)
  where prefn Nothing prefix time = "[" <> prefix <> "]" <> time
        prefn (Just (Loc{loc_module=mod,loc_start=start})) prefix time = "[" <> prefix <> "]"<>time<>"[" <> mod <> show start <> "]"

info :: (MonadIO m) => String -> m ()
info = hInfo Nothing LogInfo

trace :: (MonadIO m) => String -> m ()
trace = hInfo Nothing LogTrace


err :: (MonadIO m) => String -> m ()
err = hInfo Nothing LogErr

hInfo' :: LogLevel -> Q Exp
hInfo' logLevel = do
  loc <- location
  let locStr = concat [loc_module loc,show $ loc_start loc]
  let hInfoName = mkName "hInfo"
  logLevelExp <- liftData logLevel
  loc <- liftData $ Just loc
  -- pure $ AppE (AppE (VarE hInfo) (VarE handle)) (VarE logLevel)
  pure $ var hInfoName
    <-> loc
    <-> logLevelExp
  where var = VarE
        con = ConE
        a <-> b = AppE a b

info' = hInfo' LogInfo
err' = hInfo' LogErr
trace' = hInfo' LogTrace
