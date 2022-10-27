{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
module I.FinanceMessage where

import Data.Text(Text)
import qualified Data.Aeson as A
import GHC.Generics ( Generic )
import Data.UUID.Types(UUID)
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as Bin
import qualified Data.Binary as Bin
import Control.Monad(forM_,guard)
import Data.Word ( Word8 )
import Text.Printf(printf)
import Data.List(find)
import Data.String.Conversions(cs)
import Data.Bits (shift, (.|.))

type Quotas = M.HashMap Text (Float,Float)
data ClientMessage = Init !Quotas
  | Ticks !Quotas
  deriving (Show,Eq,Generic)

t :: Text -> Text
t = id
instance A.ToJSON ClientMessage
instance A.FromJSON ClientMessage
type Event = (Word8,Text,Float,Direction)
-- | (id,code,dir,open price,stop price,target price)
--  id range = 0 - 127
type OrderOpened = (Word8,Text,Direction,Float,Float,Float)
type Token = Text
-- | The message to do with server
data MessageServer = SVerify !Token
  | SVerifyResp !(Maybe (Quotas,[Event],[OrderOpened]))
  | SNewTicks !Quotas -- Head监听的
  | SSendTicks !Quotas -- 会话监听的
  | SNewQuotaEvent !Text !Float
  | SSendQuotaEvent !Word8 !Text !Float
  | SDelQuotaEvent !Word8
  | SOpenOrder !OrderOpened --Head监听
  | SSendOpenOrder !OrderOpened --会话监听
  | SDelOrder !Word8
  deriving (Show,Eq)

type Message = (UUID,MessageServer)

data Direction = Grow | Shrink deriving (Generic,Eq,Show)
instance Bin.Binary Direction
emptyQuatas = M.empty :: M.HashMap Text (Float,Float)
-- | 合并两个Map，如果没有差异返回Nothing
merge :: Quotas -> Quotas -> Maybe Quotas
merge new base = do
  let keys = M.keys new
  let baseV = map (\key->M.lookup key base) keys
  let newV = map (\key->M.lookup key new) keys
  guard $ baseV /= newV
  pure $ M.union new base

codeMap :: [(Text,Word8)]
codeMap =
  [ ("XAUUSD",1)
  , ("BTCUSD",2)
  , ("EURUSD",3)
  ]

word8Code :: Text -> Word8
word8Code code = maybe 100 id $ lookup code codeMap

codeCode :: Word8 -> Text
codeCode w = maybe "" fst $ find (\(_,b)->b==w) codeMap

encodeOrderOpened :: [OrderOpened] -> BL.ByteString
encodeOrderOpened  xs = Bin.runPut $ do
  forM_ xs $ \ (id,code,dir,openPrice,stopPrice,targetPrice) -> do
    let id7bit = shift id 1
    let idAndDir = if dir == Grow then id7bit .|. (0x01::Word8) else id7bit
    Bin.putWord8 $ word8Code code + 100
    Bin.put idAndDir
    Bin.putFloatle openPrice
    Bin.putFloatle stopPrice
    Bin.putFloatle targetPrice

encodeEvents :: [Event] -> BL.ByteString
encodeEvents xs = Bin.runPut $ do
  forM_ xs $ \ (id,code,price,dir) -> do
    Bin.putWord8 $ word8Code code + 50
    Bin.putWord8 id
    Bin.putFloatle price

encodeQuotas :: Quotas -> BL.ByteString
encodeQuotas quotas = Bin.runPut $ do
  let keys = M.keys quotas
  forM_ keys $ \ key -> do
    let (price,percentage) = quotas M.! key
    Bin.putWord8 $ word8Code key
    Bin.putInt16le $ truncate $ percentage * 100
    Bin.putFloatle price

encodeInits (quotas,events,orderOpendes) = 
  encodeQuotas quotas <> encodeEvents events <> encodeOrderOpened orderOpendes

fixed :: Text -> String
fixed "XAUUSD" = "2"
fixed "EURUSD" = "5"
fixed _ = "0"

remidOrder dir code open True target profit = "开仓价为" <> openT <> "的" <> code 
  <> typeT <> "在" <> targetT <> "的价位上止盈了，盈利" <> profitT
  where priceF :: Float -> Text
        priceF a = cs $ (""::String) <>  printf ("%."<>fixed code <>"f") a
        openT = priceF open
        targetT = priceF target
        profitT = priceF profit
        typeT = if dir == Grow then "多单" else "空单"
remidOrder dir code open False target profit = "开仓价为" <> openT <> "的" <> code 
  <> typeT <> "在" <> targetT <> "的价位上止损了，损失" <> profitT
  where priceF :: Float -> Text
        priceF a = cs $ (""::String) <>  printf ("%."<>fixed code <>"f") a
        openT = priceF open
        targetT = priceF target
        profitT = priceF profit
        typeT = if dir == Grow then "多单" else "空单"

-- | (id,code,dir,open price,stop price,target price)
remidOrderOpening :: OrderOpened -> Text
remidOrderOpening (id,code,dir,open,stop,target) =
  "在" <> openT <> "上【" <> dirT <> code <> "】,止损" <> stopT <>"(-" <>stopDiffT<> ")止盈" <> targetT <>"(+"<>targetDiffT<>")"
  where openT = priceT open
        stopT = priceT stop
        stopDiffT | dir == Grow = priceT $ open - stop
                  | otherwise = priceT $ stop - open
        targetDiffT | dir == Grow = priceT $ target - open
                | otherwise = priceT $ open - target 
        targetT = priceT target
        priceT val = cs $ (""::String) <>  printf ("%."<>fixed code<>"f") val :: Text
        dirT = if dir == Grow then "做多" else "做空"


remidStr code tprice price Grow =  code <> "当前价" <> priceT <> " 涨超目标价" <> tpriceT
  where tpriceT = cs $ (""::String) <>  printf ("%."<>fixed code<>"f") tprice
        priceT = cs $ (""::String) <> printf ("%."<>fixed code<>"f") price
remidStr code tprice price Shrink = code <> "当前价" <> priceT <> " 跌超目标价" <> tpriceT
  where tpriceT = cs $ (""::String) <> printf ("%."<>fixed code<>"f") tprice
        priceT = cs $ (""::String) <>  printf ("%."<>fixed code<>"f") price

