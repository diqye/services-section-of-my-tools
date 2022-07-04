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
import Endpoint.Finance.Input(inputRoutes)
import Kit(evalInnerState,forkInRight)
import Control.Concurrent.Chan(Chan,newChan,dupChan,readChan)
import I.ChannelMessage(ChannelMessage)

version = "Version 1.0"
port = 8899

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  $info' version


httpServe = do
  channel <- newChan
  forkInRight
    (forever $ readChan channel)
    (W.runSettings setting $ W.toApplication $ webapp channel)

webapp :: Chan ChannelMessage -> W.AppIO
webapp channel = do
  W.appmsum 
    [ logForDebug
    , W.consum "files" >>  myFiles
    , W.consum "input" >> evalInnerState channel inputRoutes
    , W.respLBS W.status200 "ok"
    ]

-- 调试之
logForDebug = do
  req <- W.getRequest
  -- liftIO $ putStrLn $ show $ req
  empty

-- 非错误不与输出
logException req someException = do
  when (W.defaultShouldDisplayException someException) $ do
    $err' $ displayException someException

setting = W.setPort port
  --支持 IPv6 与 IPv4
  $ W.setHost "*"
  $ W.setOnException logException
  -- defaultOnExceptionResponse
  $ W.setOnExceptionResponse W.exceptionResponseForDebug
  -- $ W.setTimeout (30*60*60) -- Seconds
  $ W.defaultSettings
