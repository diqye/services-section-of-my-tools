{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
module Endpoint.Message where
import qualified Web.AppM as W
import qualified Data.Aeson as A
import Kit(oneSec,respJSON,getInnerState)
import qualified Kit as Kit
import Control.Monad.IO.Class(liftIO,MonadIO)
import qualified Web.WebSocket.WebSocket as W
import Module.Log(hInfo,trace',err',info')
import Control.Applicative((<|>),empty)
import Control.Monad(guard,forever,when,Monad,forM,forM_)
import Data.Text(Text,unpack)
import Control.Concurrent(threadDelay,forkIO)
import Control.Exception(Exception,throwIO,handle,SomeException)
import Control.Concurrent.Chan(Chan,writeChan,readChan,dupChan)
import qualified I.ChatInfo as Chat
import Control.Concurrent.Async
import Data.IORef
import qualified Data.List as List
import qualified Data.ByteString.Lazy as BL
import System.Directory(doesFileExist)


routes :: W.AppT (W.StateT (Chan Chat.SystemMessage) IO) W.Application
routes = W.appmsum
  [ W.consum "channel" >> channelApp
  ]

filePath = "./chatInfos.json"

initChatInfos ref = do
  existed <- doesFileExist filePath
  when existed $ do
    $info' $ "Reading file form path " ++ filePath
    json <- BL.readFile filePath
    let infos = maybe [] id $ A.decode json
    writeIORef ref infos

alarmAfter12hours ref = do
  threadDelay (1000000 * 60 * 60 * 12)
  $info' $ "Writing file to " ++ filePath
  infos <- readIORef ref 
  BL.writeFile filePath $ A.encode infos

chatHeadquarters chatChannel = do
  chatInfosRef <- newIORef []
  -- 初始化数据
  initChatInfos chatInfosRef
  -- 定时保存数据
  timer <- async $ forever $ alarmAfter12hours chatInfosRef
  liveCheck <- async $ forever $ do
    threadDelay (oneSec * 60)
    time <- Kit.getCurrentTime
    let crstamp = Kit.mkTimestamp time
    chatInfos <- readIORef chatInfosRef
    forM_ chatInfos $ \ info -> do
      forM_ (Chat.onlineMember info)  $ \ (id,timestamp) -> do
        if crstamp - timestamp > 40*1000 then do
          writeChan chatChannel $ Chat.Offline (Chat.chatName info,id) "Timeout"
        else pure ()
  let handException action = do
        handle' `handle` action
        where handle' :: SomeException -> IO ()
              handle' e = do
                cancel liveCheck
                cancel timer
  handException $ forever $ do
    a <- readChan chatChannel
    case a of
      (Chat.Online clientId@(chatName,id)) -> do
        chatInfos <- readIORef chatInfosRef
        r <- modifyOrCreateChat chatName chatInfos $ \ info -> do
          time <- Kit.getCurrentTime
          let timestamp = Kit.mkTimestamp time
          let info' = Chat.over_onlineMember (((id,timestamp):) . filter ((/=id) . fst)) info
          writeChan chatChannel $ Chat.InitialInfo clientId info'
          pure info'
        writeIORef chatInfosRef r
      (Chat.Ping (chatName,id)) -> do
        chatInfos <- readIORef chatInfosRef
        r <- modifyOrCreateChat chatName chatInfos $ \ info -> do
          time <- Kit.getCurrentTime
          let timestamp = Kit.mkTimestamp time
          let member = List.find ((==id) . fst) $ Chat.onlineMember info
          if member == Nothing then do
            --此处为处于某种原因而踢出在线列表，但连接仍然有效并且在Ping.
            --同一个ID多个端登录，其中一端下线也会出现这种情况
            let info' = Chat.over_onlineMember ((id,timestamp):) info
            writeChan chatChannel $ Chat.Online (chatName,id)
            pure info'
          else do
            let updateTime (id',_) | id' == id = (id,timestamp)
                updateTime a = a
            let info' = Chat.over_onlineMember (map updateTime) info
            pure info'
        writeIORef chatInfosRef r
      (Chat.Send chatName message) -> do
        chatInfos <- readIORef chatInfosRef
        r <- modifyOrCreateChat chatName chatInfos $ \ info -> do
          let receiver = Chat.getReceiver message
          if receiver == Nothing then do
            pure $ Chat.over_recentMessages (take 100 . (message:)) info
          else do 
            pure info
        writeIORef chatInfosRef r
      (Chat.Offline (chatName,id) reson) -> do
        chatInfos <- readIORef chatInfosRef
        let ignore (id',_) =  id' /= id
        r <- modifyOrCreateChat chatName chatInfos $ \ info -> do
          pure $ Chat.over_onlineMember (filter ignore) $ info
        writeIORef chatInfosRef r
      _ -> pure ()



modifyOrCreateChat :: Text -> [Chat.ChatInfo] -> (Chat.ChatInfo -> IO Chat.ChatInfo) -> IO [Chat.ChatInfo]
modifyOrCreateChat chatName infos fn = do
  let info = List.find ((==chatName). Chat.chatName) infos
  if info == Nothing then do
    let newInfo = Chat.ChatInfo
          { Chat.chatName = chatName
          , Chat.recentMessages = []
          , Chat.onlineMember = []
          }
    modifiedInfo <- fn newInfo
    pure $ modifiedInfo:infos
  else do
    forM infos $ \ info -> do
      if Chat.chatName info == chatName then do
        fn info
      else pure info

channelApp = do
  channelHead <- getInnerState
  channel <- liftIO $ dupChan channelHead
  chatName <- W.consumV
  W.respSocket' $ \ pendingConn -> do
    conn <- W.acceptRequest pendingConn
    clientName <- W.receiveData conn
    let clientId = (chatName,clientName)
    --判断名字是否合法
    writeChan channel $ Chat.Online clientId
    a <- async $ waitInitialInfo clientId channel
    -- 10s超时
    b <- async $ threadDelay (oneSec * 10)
    r <- waitEitherCancel a b
    case r of
      Right _ -> W.sendClose conn ("Timeout"::Text)
      Left (Left _) -> W.sendClose conn ("Illege name"::Text)
      Left (Right info) -> do
        $trace' $ "Connected by " ++ unpack (fst clientId) ++ "/" ++ unpack (snd clientId)
        W.sendTextData conn $ A.encode (Chat.CInitialInfo info)
        let heart = pingSysem channel clientId
        -- Ping/30s,浏览器Websocket会自动回应Pong消息，客户端无需code
        W.withPingThread conn 30 heart $ do
          let controlException (W.CloseRequest code reson) = do
                writeChan channel $ Chat.Offline clientId "CloseRequest Exception"
              controlException W.ConnectionClosed = do
                writeChan channel $ Chat.Offline clientId "ConnectionClosed"
              controlException e = do
                $err' $ show e

          a <- async $ forever $ listenBroadcast channel conn clientId
          controlException `handle` (forever $ listenClient channel conn clientId)
          cancel a

listenClient channel conn clientId@(chatName',clientName) = do
  text <- W.receiveData conn
  let clientMessage = A.decode text
  case clientMessage of
    (Just (Chat.CMessage msg)) -> do
      currentTime <- Kit.getCurrentTime
      let timestamp = Kit.mkTimestamp currentTime
      let sMsg = Chat.Send chatName'
            $ Chat.set_time timestamp
            $ Chat.set_sender clientName
            $ Chat.set_c_sender clientName
            $ msg
      writeChan channel sMsg
    (Just (Chat.CPing text)) -> do
      W.sendTextData conn $ A.encode $ Chat.CPing text
    _ -> W.sendTextData conn $ A.encode Chat.CIllegeData

listenBroadcast channel conn clientId@(chatName',clientName) = do
  message <- readChan channel
  case message of 
    Chat.Offline id@(name,_) text | name == chatName' -> do
      W.sendTextData conn $ A.encode $ Chat.COffLine id text
    Chat.InitialInfo id@(name,_) _ | name == chatName' -> do
      W.sendTextData conn $ A.encode $ Chat.COnline id
    Chat.Send chatName chatMessage | chatName == chatName' -> do
      let receiver = Chat.getReceiver chatMessage
      if receiver == Nothing then do
        W.sendTextData conn $ A.encode $ Chat.CMessage chatMessage
      else if receiver == Just clientName then do 
        W.sendTextData conn $ A.encode $ Chat.CMessage chatMessage
      else do 
        pure ()
    _ -> pure ()

-- 内存中以心跳方式维护client在线情况
pingSysem channel clientId = do
  writeChan channel $ Chat.Ping clientId

waitInitialInfo clientId channel = do
  message <- readChan channel
  case message of
    Chat.Illege id | id == clientId -> pure $ Left id
    Chat.InitialInfo id info | id == clientId -> pure $ Right info
    _ -> waitInitialInfo clientId channel