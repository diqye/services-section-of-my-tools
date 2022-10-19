{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
module Endpoint.Finance.Input where
-- | Space + 0 静音 - 减音 + 增音  
import qualified Web.AppM as W
import qualified Data.Aeson as A
import Data.Word
import Kit(oneSec,respJSON,getInnerState)
import Control.Monad.IO.Class(liftIO,MonadIO)
import qualified Web.WebSocket.WebSocket as W
import Module.Log(hInfo,info',trace',err')
import Control.Applicative((<|>),empty)
import Control.Monad(forM_,guard,forever,when,Monad)
import Data.Text(Text,unpack)
import qualified Data.Text.IO as TI
import Control.Concurrent(threadDelay,forkIO)
import Control.Exception(SomeException,Exception,throwIO,handle)
import qualified I.FinanceMessage as IF
import Control.Concurrent.Chan(Chan,writeChan,readChan,dupChan)
import Data.UUID.Types(UUID)
import Control.Concurrent.Async
import System.Random(randomIO)
import Data.IORef
import qualified Module.FreeJSON as FJ
import qualified Data.HashMap.Strict as M
import Data.List(find)
import qualified Module.Request as MR
import qualified Data.ByteString.Lazy as BL
import Module.TemplateGetter

headquarters :: Chan IF.Message -> IO ()
headquarters channel = do
  ref <- newIORef IF.emptyQuatas
  eventRef <- newIORef []
  handException $ forever $ do
    a <- readChan channel
    messageHandle a channel (ref,eventRef::IORef [(Word8,Text,Float,IF.Direction)])
  where handException action = do
          handle' `handle` action
        handle' e = do
          $err' $ show (e::SomeException)

messageHandle (uuid,IF.SVerify token) chan (ref,eRef) = do
  if token == "098765"
  then do
    quotas <- readIORef ref
    events <- readIORef eRef
    writeChan chan $ (uuid,IF.SVerifyResp (Just (quotas,events)))
  else do
    $trace' $ show (uuid,token)
    writeChan chan $ (uuid,IF.SVerifyResp Nothing)

messageHandle (uuid,IF.SNewTicks quotas) chan (ref,eRef) = do
  squotas <- readIORef ref
  case IF.merge quotas squotas of
    Nothing -> pure ()
    Just quotas' -> do
      writeIORef ref quotas'
      writeChan chan $ (uuid,IF.SSendTicks quotas')
      -- 判断是否触发价格提醒
      triggerRemid uuid quotas chan eRef
messageHandle (uuid,IF.SDelQuotaEvent id) chan (ref,eRef) = do
  events <- readIORef eRef
  W.runMaybeT $ do
    (_,code,_,_) <- W.MaybeT $ pure $ find (\(id',_,_,_)->id' == id) $ events
    let nEvents = filter (\(id',_,_,_)->id' /= id) $ events
    liftIO $ do
      writeIORef eRef $ nEvents
      writeChan chan (uuid,IF.SSendQuotaEvent id code (-1))
  pure ()
messageHandle (uuid,IF.SNewQuotaEvent code price) chan (ref,eRef) = do
  squotas <- readIORef ref
  events <- readIORef eRef
  W.runMaybeT $ do
    (realPrice,_) <- W.MaybeT $ pure $  M.lookup code squotas
    let dir = if realPrice > price then IF.Shrink else IF.Grow
    let id = (+1) $ foldl (\b a -> max b (_1 a)) (0::Word8) events
    liftIO $ writeIORef eRef $ (id,code,price,dir):events
    liftIO $ writeChan chan (uuid,IF.SSendQuotaEvent id code price)
  pure ()

messageHandle _ _ _ = pure ()

triggerRemid uuid quotas chan eRef = do
  let keys = M.keys quotas
  forM_ keys $ \ key -> W.runMaybeT $ do
    events <- liftIO $ readIORef eRef
    (id,code,targetPrice,dir) <- W.MaybeT $ pure $ find (\(_,a,_,_)->a==key) events
    let (price,percentage) = quotas M.! key
    let isTri = if dir == IF.Grow then price > targetPrice else price < targetPrice
    guard isTri
    liftIO $ do
      writeIORef eRef $ filter (\(id',_,_,_)->id' /= id) events
      writeChan chan (uuid,IF.SSendQuotaEvent id code 0)
      async $ remidAction $ IF.remidStr code targetPrice price dir 

remidAction :: Text -> IO ()
remidAction text = do
  let param = A.object 
        [ "channel" A..= ("Time" :: Text)
        , "content" A..= text
        ]
  let req = MR.bhUtf8json param 
        $ MR.hAuthorization "1qaz2wsx"
        $ MR.mpost "http://hk.diqye.com/speaker/c"
  MR.httpActionBody req
  pure ()

inputRoutes :: W.AppT (W.StateT (Chan IF.Message) IO) W.Application
inputRoutes = W.appmsum
  [ W.consum "history" >> accpetHistory
  , W.consum "live" >> liveData
  ]
tm :: Monad m => Text -> m ()
tm _ = pure ()
liveData = do
  channelHead <- getInnerState
  channel <- liftIO $ dupChan channelHead
  nFlag <- W.consumV
  W.respSocket' $ \pendingConn -> do
    conn <- W.acceptRequest pendingConn
    token <- W.receiveData conn
    uuid <- randomIO
    writeChan channel $ (uuid,IF.SVerify token)
    vr <- verify uuid channel
    let clientlogic' = clientlogic uuid conn channel
    let pushLogic' = pushLogic uuid conn channel
    case vr of Nothing -> W.sendClose conn ("verifing fail" :: Text)
               Just r -> if nFlag == (0::Int) then pushLogic' else clientlogic' r
  where
    clientlogic uuid conn chan (quotas,events) = W.withPingThread conn 30 (pure ()) $ do
      listenOtherAsync <- async $ forever $ readChan chan >>= listenOther uuid conn
      W.sendBinaryData conn $ IF.encodeQuotas quotas <> IF.encodeEvents events
      handleException `handle` forever (W.receiveData conn >>= listenClient uuid chan)
      cancel listenOtherAsync

    listenOther uuid conn (uuid',IF.SSendTicks quotas) = do
      W.sendBinaryData conn $ IF.encodeQuotas quotas
    listenOther uuid conn (uuid',IF.SSendQuotaEvent id code price) = do
      W.sendBinaryData conn $ IF.encodeEvents [(id,code,price,IF.Grow)]
    listenOther uuid conn _ = pure ()

    listenClient uuid chan text = do
      let msg = parseT $ FJ.decodeJSON text
      case msg of
        Left a -> $info' $ show a
        Right msg -> writeChan chan (uuid,msg)
    parseT :: Either (Int,String) A.Value -> Either (Int,String) IF.MessageServer
    parseT val' = do
      val <- val'
      rType <- FJ.getField "type" val
      let a = id (rType :: Text)
      case rType of 
        "del-remid" -> parseTDel val
        "remid" -> parseTCreate val
        _ -> Left (1000,"Unknow type")
    
    parseTCreate val = do
      code <- FJ.getField "code" val
      price <- FJ.getField "price" val
      pure $ IF.SNewQuotaEvent code price
    parseTDel val = do
      id <- FJ.getField "id" val
      pure $ IF.SDelQuotaEvent id

    pushLogic uuid conn chan = W.withPingThread conn 30 (pure ()) $ forever $ do
      bs <- W.receiveData conn
      let (Just quotas) = A.decode bs
      writeChan chan $ (uuid,IF.SNewTicks quotas)

    handleException (W.CloseRequest code reson) = pure ()
    handleException W.ConnectionClosed = pure ()
    handleException e = $err' $ show e



verify uuid chan = do
  a <- async $ verify' uuid chan
  b <- async $ threadDelay (oneSec * 10)
  c <- waitEitherCancel a b
  $trace' $ show c
  case c of
    Right _ -> pure Nothing
    Left r -> pure r

verify' uuid chan = readChan chan >>= fn
  where fn (uuid',IF.SVerifyResp quotasAndEvents) | uuid == uuid' = pure quotasAndEvents
        fn _ = verify' uuid chan

accpetHistory = do
  code <- W.consumV
  period <- W.consumV
  datas <- W.bodyJSONV
  liftIO $ print (code::String,period::String)
  respJSON (100::Int,datas::A.Value)