module Kit where

import Data.Time as T
import Data.Time.Clock.POSIX as P
import qualified Web.AppM as W
import qualified Web.WebSocket.WebSocket as W
import qualified Data.Aeson as A
import Control.Concurrent.Async(withAsync)
import Control.Exception(throwIO,handle)
import Control.Monad(forever)
import Data.Text(Text)
-- import Control.Monad.State.Lazy(StateT,runStateT)
-- UTC转时间戳，精确到毫秒
mkTimestamp :: UTCTime -> Integer
mkTimestamp time = 
  truncate 
  $ toRational
  $ utcTimeToPOSIXSeconds time * 1000

-- 时间戳转UTC时间
mkTimeFromTimestamp :: Integer -> UTCTime
mkTimeFromTimestamp timestamp =
  posixSecondsToUTCTime
  $ fromRational
  $ toRational timestamp / 1000

-- 当前UTC时间
getCurrentTime :: IO UTCTime
getCurrentTime = T.getCurrentTime

-- 转换为中国标准时间
toLocalTime = utcToLocalTime $ hoursToTimeZone 8


respJSON :: (A.ToJSON a,Monad m) => a -> W.AppT m W.Application
respJSON jsonV = do
  W.putHeader W.hContentType "application/json;utf-8"
  W.respLBS W.status200 $ A.encode jsonV

-- | type AppT m = MaybeT (StateT AppState m)
-- MaybeT (StateT AppState (StateT s m)) Application
evalInnerState 
  :: (Monad m) => s
  -> W.AppT (W.StateT s m) a
  -> W.AppT m a
evalInnerState s app = do
  state <- W.lift $ W.get
  let stateApp = W.runMaybeT app
  let sapp = W.runStateT stateApp state
  ((maybeA,s'),s) <- W.lift $ W.lift $ W.runStateT sapp s
  W.MaybeT $ pure maybeA

getInnerState :: Monad m => W.AppT (W.StateT s m) s
getInnerState = W.lift $ W.lift $ W.get

forkInRight :: IO a -> IO b -> IO b
forkInRight action1 action2 = withAsync action1 (const action2)


keepWebsocket 
  :: W.Connection
  -> IO Bool
  -> IO ()
  -> IO ()
  -> IO ()
keepWebsocket 
  conn
  firstAction
  heartAction
  repeatAction
  = do
  doContinue <- firstAction
  if doContinue
  then do
    W.withPingThread conn 30 heartAction $ do -- ping/30s,并在repeatAction结束(异常)后友好的杀死Ping线程。
      ignore `handle` forever repeatAction
  else do
    W.sendClose conn ("Unauthorized"::Text)
    -- 忽略飞行消息
    pure ()
  where ignore :: W.ConnectionException -> IO ()
        ignore (W.CloseRequest code reson) = pure ()
        ignore e = throwIO e