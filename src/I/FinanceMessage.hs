{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
module I.FinanceMessage where

import Data.Text(Text)
import qualified Data.Aeson as A
import GHC.Generics
import Data.UUID.Types(UUID)
import qualified Data.HashMap.Strict as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as Bin
import Control.Monad(forM_,guard)
import Data.Word
import Text.Printf(printf)
import Data.List(find)
import Data.String.Conversions(cs)

type Quotas = M.HashMap Text (Float,Float)
data ClientMessage = Init Quotas
  | Ticks Quotas
  deriving (Show,Eq,Generic)

t :: Text -> Text
t = id
instance A.ToJSON ClientMessage
instance A.FromJSON ClientMessage

type Token = Text
data MessageServer = SVerify Token
  | SVerifyResp (Maybe (Quotas,[(Word8,Text,Float,Direction)]))
  | SNewTicks Quotas -- Head监听的
  | SSendTicks Quotas -- 客户端监听的
  | SNewQuotaEvent Text Float
  | SSendQuotaEvent Word8 Text Float
  | SDelQuotaEvent Word8
  deriving (Show,Eq)

type Message = (UUID,MessageServer)

data Direction = Grow | Shrink deriving (Eq,Show)
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


encodeEvents :: [(Word8,Text,Float,Direction)] -> BL.ByteString
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

fixed :: Text -> String
fixed "XAUUSD" = "2"
fixed "EURUSD" = "5"
fixed _ = "0"
remidStr code tprice price Grow =  code <> "当前价" <> priceT <> " 涨超目标价" <> tpriceT
  where tpriceT = cs $ (""::String) <>  printf ("%."<>fixed code<>"f") tprice
        priceT = cs $ (""::String) <> printf ("%."<>fixed code<>"f") price
remidStr code tprice price Shrink = code <> "当前价" <> priceT <> " 跌超目标价" <> tpriceT
  where tpriceT = cs $ (""::String) <> printf ("%."<>fixed code<>"f") tprice
        priceT = cs $ (""::String) <>  printf ("%."<>fixed code<>"f") price

