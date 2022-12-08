import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Except (runExceptT)
import qualified Module.FreeJSON as F
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad (guard, forM_, when)
import qualified Data.Aeson as A
import I.Strategies
import I.Candle
import qualified I.FinanceMessage as IF
import Data.Default.Class (Default(def))
import Test.QuickCheck (quickCheck)

test :: IO PIFifthData
test = do
  txt <- BL.readFile "./test/xauusd.txt"
  a <- runExceptT $ do
    values <- F.decodeJSON txt
    guard $ length values >= 5
    let [a,b,c,d,e] = drop (length values - 5) values
    pure (a,b,c,d,e)
  case a of
    Left _ -> pure (def,def,def,def,def)
    Right a -> pure a

testCandle1 = A.encode $ Candle {open=1,close=2,high=3,low=1}
testCandle2 :: Bool
testCandle2 = a == b where
  a = A.decode "{\"low\":1,\"close\":2,\"open\":1,\"high\":3,\"d\":8}"
  b = Just $ Candle 1 2 3 1

testPiFifth = do
  fiveData <- test
  let actions = piFifth "XAUUSD" def fiveData [(0,"XAUUSD",IF.Grow,0,0,0)]
  quickCheck $ actions == [Open (100,"XAUUSD",IF.Shrink,1645.33,1657.04,1623.081),Close 0]

main = do
  quickCheck testCandle2
  testPiFifth
  quickCheck $ (Nothing :: Maybe ())