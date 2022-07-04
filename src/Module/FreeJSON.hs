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

decodeJSON :: A.FromJSON a => L.ByteString -> Either (Int,String) a
decodeJSON json = case A.eitherDecode json of
  (Left err) -> Left (100,err)
  (Right a) -> Right a


getField :: A.FromJSON a => Text -> A.Value -> Either (Int,String) a
getField text value =  case value of
  (A.Object o) -> maybe (Left (101,"No key : " <> cs text)) getValue $ HMap.lookup text o
  _ -> Left (102, "Value is not object")

getValue :: A.FromJSON a => A.Value -> Either (Int,String) a
getValue val = case A.fromJSON val of
  (A.Success a) -> Right a
  (A.Error err) -> Left (103,err)