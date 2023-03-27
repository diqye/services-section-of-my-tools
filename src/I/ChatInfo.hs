{-# LANGUAGE OverloadedStrings,DeriveGeneric,TemplateHaskell #-}
module I.ChatInfo where
import Data.Text(Text)
import qualified Data.Aeson as A
import GHC.Generics
import Module.TemplateGetter
import Data.List(find)
import qualified Network.WebSockets as WS


type ChatInfos = [ChatInfo]
type JSTimestamp = Integer
data ChatInfo =
  ChatInfo
  { chatName :: Text
  , recentMessages :: [ChatMessage]
  , onlineMember :: [(Text,JSTimestamp)]
  } deriving (Show,Eq,Generic)

data ChatMessage =
  ChatMessage
  { content :: !Text
  , sender :: !Text
  , receiver :: Maybe Text
  , time :: !Integer
  } | ChatCustom
  { c_type :: !Text
  , c_sender :: !Text
  , c_receiver :: Maybe Text
  , c_content :: !A.Value
  } deriving (Show,Eq,Generic)

getReceiver :: ChatMessage -> Maybe Text
getReceiver (ChatMessage {receiver=receviver'}) = receviver'
getReceiver (ChatCustom {c_receiver=receviver'}) = receviver'

-- (chatName,sender)
type ClientId = (Text,Text)

data SystemMessage =
  Offline !ClientId !Text
  | Online !ClientId
  | Illege !ClientId
  | InitialInfo !ClientId !ChatInfo
  | Ping !ClientId
  | Send !Text !ChatMessage
  deriving (Show,Generic)

data ClientMessage =
  CMessage !ChatMessage
  | COffLine !ClientId !Text
  | COnline !ClientId 
  | CInitialInfo !ChatInfo
  | CIllegeData
  | CPing !Text
  deriving (Show,Generic)

instance A.ToJSON ClientMessage
instance A.FromJSON ClientMessage

myCustomOptions = A.defaultOptions {A.sumEncoding = A.UntaggedValue}
instance A.ToJSON ChatMessage where
  toJSON  = A.genericToJSON myCustomOptions
instance A.ToJSON SystemMessage
instance A.ToJSON ChatInfo

instance A.FromJSON ChatMessage where
   parseJSON = A.genericParseJSON myCustomOptions
instance A.FromJSON SystemMessage
instance A.FromJSON ChatInfo



mkSetAndOver ''ChatInfo
mkSetAndOver ''ChatMessage
mkSetAndOver ''SystemMessage

