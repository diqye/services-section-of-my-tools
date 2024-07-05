{-# LANGUAGE OverloadedStrings #-}
module Module.Request
  ( module HTTP
  , module HTTPS
  , mmethod
  , mpost
  , mget
  , mhead
  , mput
  , mdelete
  , header
  , hUtf8json
  , hAuthorization
  , bjson
  , bhUtf8json
  , httpAction
  , httpActionBody
  ) where
import Network.HTTP.Client as HTTP
import Network.HTTP.Client.TLS as HTTPS
import qualified Network.HTTP.Types.Method as Method --http-types
import qualified Data.Aeson as A
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Types as T
import Control.Monad.IO.Class(liftIO,MonadIO)
import Data.ByteString.Lazy




mmethod :: Method.Method -> HTTP.Request -> HTTP.Request
mmethod httpMethod req = req {HTTP.method = httpMethod}

{-- 
GET	 
POST	 
HEAD	 
PUT	 
DELETE	 
TRACE	 
CONNECT	 
OPTIONS	 
PATCH
--}
mpost = mmethod Method.methodPost
mget = mmethod Method.methodGet
mhead = mmethod Method.methodHead
mput = mmethod Method.methodPut
mdelete = mmethod Method.methodDelete

header :: Header.Header -> HTTP.Request -> HTTP.Request
header oneHeader req = req {HTTP.requestHeaders=oneHeader:HTTP.requestHeaders req}

hUtf8json = header (Header.hContentType,"application/json; charset=utf-8")

hAuthorization auth = header (Header.hAuthorization,auth)

bjson :: A.ToJSON a => a -> HTTP.Request -> HTTP.Request
bjson value req = req {HTTP.requestBody = HTTP.RequestBodyLBS $ A.encode value}

bhUtf8json :: A.ToJSON a => a -> HTTP.Request -> HTTP.Request
bhUtf8json a = bjson a . hUtf8json

bjson8 :: A.ToJSON a => a -> HTTP.Request -> HTTP.Request
bjson8 = bhUtf8json

httpAction :: MonadIO m => HTTP.Request -> m (T.Status,ByteString)
httpAction req = do
  resp <- liftIO $ do 
    manager <- HTTPS.newTlsManager
    HTTP.httpLbs req manager
  pure (HTTP.responseStatus resp,HTTP.responseBody resp)

httpActionBody req = do
  (_,r) <- httpAction req
  pure r

