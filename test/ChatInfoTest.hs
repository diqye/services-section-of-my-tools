import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Except (runExceptT)
import qualified Module.FreeJSON as F
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad (guard, forM_, when)
import qualified Data.Aeson as A
import Data.Default.Class (Default(def))
import Test.QuickCheck (quickCheck)
import I.ChatInfo
import Debug.Trace

quickCheckChatMessage = testChatMessage && rtestChatMessage && testChatCustom && rtestChatCustom where
    testChatMessage = A.encode a  == a'
    rtestChatMessage = A.decode a' == Just a
    testChatCustom = A.encode b == b'
    rtestChatCustom = A.decode b' == Just b
    a =
        ChatMessage
        { content = "content"
        , sender = "sender"
        , time = 1
        }
    a' = "{\"content\":\"content\",\"sender\":\"sender\",\"time\":1}"
    b = 
        ChatCustom
        { c_type = "c_type"
        , c_sender = "c_sender"
        , c_content = A.toJSON a
        }
    b' = "{\"c_content\":{\"content\":\"content\",\"sender\":\"sender\",\"time\":1},\"c_sender\":\"c_sender\",\"c_type\":\"c_type\"}"
main = do
    quickCheck quickCheckChatMessage