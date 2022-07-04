{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
module I.ChannelMessage where

import Data.Text(Text)
import qualified Data.Aeson as A
import GHC.Generics

data ChannelMessage = Live Text
  | Finance
  | Message Text Text Text -- Message to from message
  deriving (Show,Eq)

t :: Text -> Text
t = id
instance A.ToJSON ChannelMessage where 
  toJSON (Live text) = A.object [
    "tag" A..= t "Live" ,
    "text" A..= text ]
  toJSON Finance = A.object [
    "tag" A..= t "Finance" ]
  toJSON (Message to from text) = A.object [
    "tag" A..= t "Message",
    "from" A..= from,
    "to" A..= to,
    "text" A..= text ]

instance A.FromJSON ChannelMessage where
   parseJSON = A.withObject "ChannelMessage" $ \ v -> do
    tag <- v A..: t "tag"
    let msg | tag == t "Finance" = pure Finance
            | tag == t "Live" = do
          text <- v A..: t "text"
          pure $ Live text
            | tag == t "Message" = do
          text <- v A..: t "text"
          from <- v A..: t "from"
          to <- v A..: t "to"
          pure $ Message to from text
    msg