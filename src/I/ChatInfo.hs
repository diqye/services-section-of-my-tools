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
data ChatInfo = ChatInfo
  { chatName :: Text
  , recentMessages :: [ChatMessage]
  , onlineMember :: [(Text,JSTimestamp)]
  } deriving (Show,Eq,Generic)

data ChatMessage = ChatMessage
  { content :: !Text
  , sender :: !Text
  , time :: !Integer
  } deriving (Show,Eq,Generic)

-- (chatName,sender)
type ClientId = (Text,Text)

data SystemMessage = Offline !ClientId !Text
  | Online !ClientId
  | Illege !ClientId
  | InitialInfo !ClientId !ChatInfo
  | Ping !ClientId
  | Send !Text !ChatMessage
  deriving (Show,Generic)

data ClientMessage = CMessage !ChatMessage
  | COffLine !ClientId !Text
  | COnline !ClientId 
  | CInitialInfo !ChatInfo
  | CIllegeData
  | CPing !Text
  deriving (Show,Generic)

instance A.ToJSON ClientMessage
instance A.FromJSON ClientMessage

instance A.ToJSON ChatMessage
instance A.ToJSON SystemMessage
instance A.ToJSON ChatInfo

instance A.FromJSON ChatMessage
instance A.FromJSON SystemMessage
instance A.FromJSON ChatInfo



mkSetAndOver ''ChatInfo
mkSetAndOver ''ChatMessage
mkSetAndOver ''SystemMessage


