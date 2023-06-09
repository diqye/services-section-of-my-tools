{-# LANGUAGE OverloadedStrings #-}
module Module.FreeJSON
  ( module A
  , decodeJSON
  , getField
  , getValue
  ) where

import qualified Data.ByteString.Lazy as L
import Data.Monoid((<>))
import Data.Aeson as A
import qualified Data.Aeson.KeyMap as A
import qualified Data.Aeson.Key as A
import Data.Text
import Data.String.Conversions(cs)
import Control.Monad.Trans.Except (ExceptT(ExceptT),except,throwE)

decodeJSON :: (A.FromJSON a,Monad m) => L.ByteString -> ExceptT [String] m a
decodeJSON json = except $ either (Left.(:[])) Right $ A.eitherDecode json 


getField :: (A.FromJSON a,Monad m) => Text -> A.Value -> ExceptT [String] m a
getField text value =  case value of
  (A.Object o) -> do
    let val = A.lookup (A.fromText text) o
    case val of (Just val') -> getValue val'
                Nothing -> throwE ["the key " <> cs text <> " doesn't exist."]
  _ -> throwE ["To except an Object with a key  " <>cs text <> " but it is not a Object"]

getValue ::(A.FromJSON a,Monad m) => A.Value -> ExceptT [String] m a
getValue val = case A.fromJSON val of
  (A.Success a) -> except $ Right a
  (A.Error err) -> throwE [err]