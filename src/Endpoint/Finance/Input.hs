{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
module Endpoint.Finance.Input where
-- | Space + 0 静音 - 减音 + 增音  
import qualified Web.AppM as W
import qualified Data.Aeson as A
import Kit(respJSON,getInnerState,forkInRight,keepWebsocket)
import Control.Monad.IO.Class(liftIO,MonadIO)
import qualified Web.WebSocket.WebSocket as WS
import Module.Log(hInfo,trace')
import Control.Applicative((<|>),empty)
import Control.Monad(guard,forever,when,Monad)
import Data.Text(Text,unpack)
import Control.Concurrent(threadDelay,forkIO)
import Control.Exception(Exception,throwIO,handle)
import I.ChannelMessage(ChannelMessage(Live))
import Control.Concurrent.Chan(Chan,writeChan,readChan,dupChan)

inputRoutes :: W.AppT (W.StateT (Chan ChannelMessage) IO) W.Application
inputRoutes = W.appmsum
  [ W.consum "history" >> accpetHistory
  , W.consum "live" >> getInnerState >>= liveData
  ]

liveData channel' = WS.respSocket' $ \ pendingConn -> do
  conn <- WS.acceptRequest pendingConn
  channel <- dupChan channel'
  let doAuth = do
        text <- WS.receiveData conn
        pure $ text == ("-=_+"::Text)
  let heartAction = pure ()
  let repeatAction = do
        text <- WS.receiveData conn
        $trace' $ unpack text
        writeChan channel $ Live text

  forkInRight
    (forever $ readChan channel)
    (keepWebsocket conn doAuth heartAction repeatAction)



accpetHistory = do
  code <- W.consumV
  period <- W.consumV
  datas <- W.bodyJSONV
  liftIO $ print (code::String,period::String)
  respJSON (100::Int,datas::A.Value)