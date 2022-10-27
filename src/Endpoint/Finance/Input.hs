{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module Endpoint.Finance.Input where
import qualified Web.AppM as W
import System.Directory(doesFileExist)
import qualified Data.Aeson as A
import Data.Word ( Word8 )
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
import Control.Concurrent.Async ( waitEitherCancel, cancel, async )
import System.Random(randomIO)
import Data.IORef ( writeIORef, readIORef, newIORef, IORef )
import qualified Module.FreeJSON as FJ
import qualified Data.HashMap.Strict as M
import Data.List(find)
import qualified Module.Request as MR
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Module.TemplateGetter
import qualified Data.Binary as Bin
import Data.String.Conversions(cs)
import qualified I.FinanceMessage as A
import Control.Monad.Trans.Except (runExceptT,ExceptT(ExceptT), throwE)
import Data.List(intercalate)

headquarters :: Chan IF.Message -> IO ()
headquarters channel = do
  ref <- newIORef IF.emptyQuatas
  eventRef <- newIORef ([],[])
  readEvents eventRef
  handException $ forever $ do
    a <- readChan channel
    messageHandle a channel (ref,eventRef::IORef ([IF.Event],[IF.OrderOpened]))
  where handException action = do
          handle' `handle` action
        handle' e = do
          let id' = id (e::SomeException)
          pure ()

messageHandle ::(UUID, IF.MessageServer)
  -> Chan (UUID, IF.MessageServer)
  -> (IORef IF.Quotas, IORef ([IF.Event],[IF.OrderOpened]))
  -> IO ()
messageHandle (uuid,IF.SVerify token) chan (ref,eRef) = do
  if token == "098765"
  then do
    quotas <- readIORef ref
    (events,orders) <- readIORef eRef
    writeChan chan $ (uuid,IF.SVerifyResp (Just (quotas,events,orders)))
  else do
    $trace' $ show (uuid,token)
    writeChan chan $ (uuid,IF.SVerifyResp Nothing)

messageHandle (uuid,IF.SNewTicks quotas) chan (ref,eRef) = do
  squotas <- readIORef ref
  case IF.merge quotas squotas of
    Nothing -> pure ()
    Just quotas' -> do
      writeIORef ref quotas'
      writeChan chan $ (uuid,IF.SSendTicks quotas)
      -- 判断是否触发价格提醒
      triggerRemid uuid quotas chan eRef
messageHandle (uuid,IF.SDelQuotaEvent id) chan (ref,eRef) = do
  (events,orders) <- readIORef eRef
  W.runMaybeT $ do
    (_,code,_,_) <- W.MaybeT $ pure $ find (\(id',_,_,_)->id' == id) $ events
    let nEvents = filter (\(id',_,_,_)->id' /= id) $ events
    liftIO $ do
      writeEvents eRef $ (nEvents,orders)
      writeChan chan (uuid,IF.SSendQuotaEvent id code (-1))
  pure ()
messageHandle (uuid,IF.SNewQuotaEvent code price) chan (ref,eRef) = do
  squotas <- readIORef ref
  (events,orders) <- readIORef eRef
  W.runMaybeT $ do
    (realPrice,_) <- W.MaybeT $ pure $  M.lookup code squotas
    let dir = if realPrice > price then IF.Shrink else IF.Grow
    let id = (+1) $ foldl (\b a -> max b (_1 a)) (0::Word8) events
    liftIO $ writeEvents eRef $ ((id,code,price,dir):events,orders)
    liftIO $ writeChan chan (uuid,IF.SSendQuotaEvent id code price)
  pure ()
messageHandle (uuid,IF.SOpenOrder order) chan (ref,eRef) = do
  (events,orders) <- readIORef eRef
  --  id = _1 order , 100为系统开单
  let piFlag = 100
  let noSameKindOrder = null $ filter (\o->all id [_1 o > piFlag,_2 o == _2 order,_3 o == _3 order]) orders
  let getId filterFn inital= (+1) $ foldl (\ b a -> max b (_1 a)) inital $ filter (filterFn . _1) orders
  let piFifth = when noSameKindOrder $ do
        let id = getId (>piFlag) piFlag
        let newOrder = set_1 id order
        writeEvents eRef $ (events,newOrder:orders)
        writeChan chan (uuid,IF.SSendOpenOrder newOrder)
        async $ remidAction $ IF.remidOrderOpening newOrder
        pure ()

  if _1 order == piFlag then piFifth else do
    let id = getId (<piFlag) 0
    let newOrder = set_1 id order
    writeEvents eRef $ (events,newOrder:orders)
    writeChan chan (uuid,IF.SSendOpenOrder newOrder)
messageHandle (uuid,IF.SDelOrder id) chan (ref,eRef) = do
  (events,orders) <- readIORef eRef
  let newOrders = filter ((/=id)._1) orders
  writeEvents eRef $ (events,newOrders)

messageHandle _ _ _ = pure ()


triggerRemid uuid quotas chan eRef = do
  (events,orders) <- readIORef eRef
  forM_ events $ \ (id,code,targetPrice,dir) -> W.runMaybeT $ do
    (price,percentage) <- W.MaybeT $ pure $  M.lookup code quotas
    let isTri = if dir == IF.Grow then price > targetPrice else price < targetPrice
    guard isTri
    liftIO  $ do
      let newEvents = filter ((/= id)._1) events
      writeChan chan (uuid,IF.SSendQuotaEvent id code 0)
      writeEvents eRef $ (newEvents,orders)
      async $ remidAction $ IF.remidStr code targetPrice price dir 
      pure ()
  forM_ orders $ \ (id,code,dir,open,stop,target) -> W.runMaybeT $ do
    (price,percentage) <- W.MaybeT $ pure $  M.lookup code quotas
    let isStop = if dir == IF.Grow then price < stop else price > stop
    let isTarget = if dir == IF.Grow then price > target else price < target
    guard $ isStop || isTarget
    let profit = if dir == IF.Grow then price - open else open - price
    liftIO  $ do
      writeChan chan (uuid,IF.SDelOrder id)
      async $ remidAction $ IF.remidOrder dir code open isTarget price profit
      pure ()

binfilepath = "event1.bindata"
readEvents eRef = do
  existed <- doesFileExist binfilepath
  when existed $ do
    binary <- B.readFile binfilepath
    writeIORef eRef $ Bin.decode $ cs binary
writeEvents eRef events = do
  writeIORef eRef events
  let binary = Bin.encode events
  BL.writeFile binfilepath binary

remidAction :: Text -> IO ()
remidAction text = do
  let param = A.object 
        [ "channel" A..= ("XAU" :: Text)
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
    clientlogic uuid conn chan inits = W.withPingThread conn 30 (pure ()) $ do
      listenOtherAsync <- async $ forever $ readChan chan >>= listenOther uuid conn
      W.sendBinaryData conn $ IF.encodeInits inits
      handleException `handle` forever (W.receiveData conn >>= listenClient uuid chan)
      cancel listenOtherAsync

    listenOther uuid conn (uuid',IF.SSendTicks quotas) = do
      W.sendBinaryData conn $ IF.encodeQuotas quotas
    listenOther uuid conn (uuid',IF.SSendQuotaEvent id code price) = do
      W.sendBinaryData conn $ IF.encodeEvents [(id,code,price,IF.Grow)]
    listenOther uuid conn (uuid',IF.SSendOpenOrder order) = do
      W.sendBinaryData conn $ IF.encodeOrderOpened [order]
    listenOther uuid conn (uuid',IF.SDelOrder id) = do
      W.sendBinaryData conn $ BL.pack [201,id]
    listenOther uuid conn _ = pure ()

    listenClient uuid chan text = do
      either <- runExceptT $ do
        msg <- parseT $ FJ.decodeJSON text
        liftIO $ writeChan chan (uuid,msg)
      case either of 
        Left errs -> $info' $ intercalate "," errs
        Right () -> pure ()

    parseT val' = do
      val <- val'
      rType <- FJ.getField "type" val
      let a = id (rType :: Text)
      case rType of 
        "del-remid" -> parseTDel val
        "del-order" -> parseTDelOrder val
        "remid" -> parseTCreate val
        "order" -> parseTOrder val
        _ -> throwE ["Unknow type"]
    
    parseTOrder val = do
      code <- FJ.getField "code" val
      id <- FJ.getField "id" val
      dir' <- FJ.getField "dir" val
      let dir = if (dir'::Word8) == 1 then IF.Grow else IF.Shrink
      open <- FJ.getField "open" val
      stop <- FJ.getField "stop" val
      target <- FJ.getField "target" val
      pure $ IF.SOpenOrder (id,code,dir,open,stop,target)
    parseTCreate val = do
      code <- FJ.getField "code" val
      price <- FJ.getField "price" val
      pure $ IF.SNewQuotaEvent code price
    parseTDelOrder val = do
      id <- FJ.getField "id" val
      pure $ IF.SDelOrder id
    parseTDel val = do
      id <- FJ.getField "id" val
      pure $ IF.SDelQuotaEvent id

    pushLogic uuid conn chan = W.withPingThread conn 30 (pure ()) $ forever $ do
      bs <- W.receiveData conn
      let firstT = parseExpectPushQuota uuid chan bs
      let secondT = parseExpectPushOrder uuid chan bs
      either <- runExceptT $ (firstT <|> secondT)
      case either of (Left errs) -> $info' $ intercalate "," errs
                     (Right ()) -> pure ()
      pure ()
      where parseExpectPushQuota uuid chan bs = do
              quotas <- FJ.decodeJSON bs
              liftIO $ writeChan chan $ (uuid,IF.SNewTicks quotas)
            parseExpectPushOrder uuid chan bs = do
              val <- FJ.decodeJSON bs
              dir <- FJ.getField "dir" val
              let direction = if dir == (1::Word8) then IF.Grow else IF.Shrink
              code <- FJ.getField "code" val
              open <- FJ.getField "open" val
              stop <- FJ.getField "stop" val
              target <- FJ.getField "target" val
              liftIO $ do
                writeChan chan $ (uuid,IF.SOpenOrder (100,code,direction,open,stop,target))





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