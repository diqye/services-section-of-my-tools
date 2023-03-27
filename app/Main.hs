{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
import qualified Web.AppM as W
import Endpoint.Static.MyFile(myFiles)
import System.IO
  ( hSetBuffering
  , BufferMode(LineBuffering)
  , stdout
  , stdin
  , stderr
  )
import Module.Log(hInfo,info',err')
import Control.Exception(displayException)
import Control.Monad(when,forever)
import Control.Applicative(empty)
import Control.Monad.IO.Class(liftIO)
import Kit(evalInnerState)
import Control.Concurrent.Chan(Chan,newChan,dupChan,readChan)
import I.FinanceMessage(Message)
import I.ChatInfo(SystemMessage)
import Control.Concurrent.Async(async,cancel,wait)
import Control.Exception(catch,SomeException,try)
import qualified Endpoint.Message as EM
import qualified Endpoint.Finance.Input as FI

version = "Version 2.0"
port = 8899

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  $info' version
  httpServe


httpServe = do
  chatChannel <- newChan
  channel <- newChan
  chatAsync <- async $ EM.chatHeadquarters chatChannel
  inputAsync <- async $ FI.headquarters channel
  r <- try $ W.runSettings setting $ W.toApplication $ webapp (channel,chatChannel)
  case r of
    (Left e) -> $err' $ "httpServe" ++ displayException (e :: SomeException)
    (Right _) -> pure ()
  -- 释放线程资源
  cancel chatAsync
  cancel inputAsync


webapp :: (Chan Message,Chan SystemMessage) -> W.AppIO
webapp (channel,chatChannel) = do
  W.appmsum 
    [ logForDebug
    , W.consum "files" >>  myFiles
    , W.consum "input" >> evalInnerState channel FI.inputRoutes
    , W.consum "chat" >> evalInnerState chatChannel EM.routes
    , W.respLBS W.status200 "ok"
    ]

-- 调试之
logForDebug = do
  req <- W.getRequest
  -- liftIO $ putStrLn $ show $ req
  empty

-- 非错误不与输出
logException req someException = do
  when (W.defaultShouldDisplayException someException == False) $ do
    $err' $ displayException someException

setting = W.setPort port
  --支持 IPv6 与 IPv4
  $ W.setHost "*"
  $ W.setOnException logException
  -- defaultOnExceptionResponse
  -- exceptionResponseForDebug 
  $ W.setOnExceptionResponse W.defaultOnExceptionResponse
  -- $ W.setTimeout (30*60*60) -- Seconds
  $ W.defaultSettings
