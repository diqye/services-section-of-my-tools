{-# LANGUAGE OverloadedStrings #-}
module Module.FreeJSON
  ( module A
  , module HMap
  , decodeJSON
  , getField
  , getValue
  ) where

import Data.HashMap.Strict as HMap
import qualified Data.ByteString.Lazy as L
import Data.Monoid((<>))
import Data.Aeson as A
import Data.Text
import Data.String.Conversions(cs)
import Control.Monad.Trans.Except (ExceptT(ExceptT),except,throwE)

decodeJSON :: (A.FromJSON a,Monad m) => L.ByteString -> ExceptT [String] m a
decodeJSON json = except $ either (Left.(:[])) Right $ A.eitherDecode json 


getField :: (A.FromJSON a,Monad m) => Text -> A.Value -> ExceptT [String] m a
getField text value =  case value of
  (A.Object o) -> do
    let val = HMap.lookup text o
    case val of (Just val') -> getValue val'
                Nothing -> throwE ["To haven't this filed" <> cs text]
  _ -> throwE ["To except an Object but it is not on getField method and it will get " <>cs text <> " field"]

getValue ::(A.FromJSON a,Monad m) => A.Value -> ExceptT [String] m a
getValue val = case A.fromJSON val of
  (A.Success a) -> except $ Right a
  (A.Error err) -> throwE [err]