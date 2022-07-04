{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
module Endpoint.Finance.Message where
import qualified Web.AppM as W
import qualified Data.Aeson as A
import Kit(respJSON,getInnerState,forkInRight,keepWebsocket)
import Control.Monad.IO.Class(liftIO,MonadIO)
import qualified Web.WebSocket.WebSocket as W
import Module.Log(hInfo,trace')
import Control.Applicative((<|>),empty)
import Control.Monad(guard,forever,when,Monad)
import Data.Text(Text,unpack)
import Control.Concurrent(threadDelay,forkIO)
import Control.Exception(Exception,throwIO,handle)
import I.ChannelMessage(ChannelMessage(Live,Message))
import Control.Concurrent.Chan(Chan,writeChan,readChan,dupChan)

client :: W.AppT (W.StateT (Chan ChannelMessage) IO) W.Application
client = do
  channel <- getInnerState
  name <- W.consumV
  client' channel name

client' channel' name = respSocket' $ \ pendingConn -> do
  conn <- W.acceptRequest
  channel <- dupChan channel'
  let pushAction = do
        mesage <- readChan channel
        case message of
          (Live text) -> W.sendTextData text
          (Message to from text) -> W.sendTextData 
          _ -> $trace' "Unknow message"
  let authAction = do
        text <- receiveData
        pure $ text == ("&*90_+"::Text)
  let heartAction = pure ()
  let repeatAction = do
        text <- receiveData
        writeChan channel $ Message "0" name text
  forkInRight
    pushAction 
    (keepWebsocket conn authAction heartAction repeatAction)
