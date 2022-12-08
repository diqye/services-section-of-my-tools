{-# LANGUAGE OverloadedStrings,DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
module I.Strategies where
import qualified I.Candle as C
import qualified I.FinanceMessage as IF
import Module.TemplateGetter
import Data.Text (Text)
import Data.String.Conversions (cs)
import Text.Printf (printf)
import Data.Binary (Word8)
import Data.Default.Class (Default(def))
import Control.Applicative
import Control.Monad (guard, forM_, when, replicateM_)
import Control.Monad.Trans.Writer (execWriter, tell)
import qualified Data.Data as IF
import Debug.Trace (trace, traceM, traceShow)
import System.Random (Random(randomIO, randomRIO))
import Web.AppM (replicateM)
import Control.Monad (forM)
import qualified Data.Aeson as A
import Data.List (foldl')

data StrategyAction = Open !IF.OrderOpened | Close !Word8 deriving (Eq,Show)
closeOrder :: IF.OrderOpened -> Float -> Text -> Text
closeOrder order realtimePrice operator =
  orderT <> _2 order <> "被" <> operator <> "平仓了,盈利" <> profitT
  where orderT = if _3 order == IF.Grow then "多单" else "空单"
        profitT | _3 order == IF.Grow = priceT (_2 order) $ realtimePrice - _4 order
                | otherwise = priceT (_2 order) $ _4 order - realtimePrice

priceT :: Text -> Float -> Text
priceT code price = 
  cs $ (""::String) <> printf ("%."<>IF.fixed code <>"f") price

tell' a = tell [a]
piFifth :: Text -> PIConfig -> C.PIFifthData -> [IF.OrderOpened] -> [StrategyAction]
piFifth code conf fiveCandle holdOrders = result where 
  result = do
    guard $ pi /= 0
    if pi > 0 then do
      guard $ null $ filter ((==IF.Grow) . _3) holdOrders
      bought
    else do
      guard $ null $ filter ((==IF.Shrink) . _3) holdOrders
      sold
  pi = seq () $ piFifthValue fiveCandle conf
  d2f = fromRational . toRational
  bought = execWriter $ do
    let stop = foldl1 min $ map (C.low.($fiveCandle)) [_2,_3,_4]
    let open = C.close (_5 fiveCandle)
    tell' $ Open (100,code,IF.Grow,d2f open,d2f stop,d2f (open + (open-stop)*ratio conf))
    forM_ holdOrders $ \ order -> do
      if _3 order == IF.Shrink then do
        tell' $ Close $ _1 order
      else pure ()
  sold = execWriter $ do
    let stop = foldl1 max $ map (C.high.($fiveCandle)) [_2,_3,_4]
    let open = C.close (_5 fiveCandle)
    tell' $ Open (100,code,IF.Shrink,d2f open,d2f stop,d2f (open - (stop-open)*ratio conf))
    forM_ holdOrders $ \ order -> do
      if _3 order == IF.Grow then do
        tell' $ Close $ _1 order
      else pure ()

data PIConfig = PIConfig
  { emplitude :: !Double
  , ratio :: !Double
  } deriving (Show)
instance Default PIConfig where
  def :: PIConfig
  def = PIConfig {emplitude=0.6,ratio=7}

piFifthValue :: C.PIFifthData -> PIConfig -> Double
piFifthValue (a,b,c,d,e) config = maybe 0 id (bought <|> sold) where
  bought = do
    let isFirstLowDiff = first_low_rate_diff > emplitude config 
    let isLastLowDiff = last_low_rate_diff > emplitude config
    guard $ isFirstLowDiff && isLastLowDiff 
    pure last_low_rate_diff
  sold = do
    let isFirstHighDiff = first_high_rate_diff > emplitude config 
    let isLastHighDiff = last_high_rate_diff > emplitude config
    guard $ isFirstHighDiff && isLastHighDiff 
    pure $ negate last_high_rate_diff
  (la,lb,lc,ld,le) = (C.low a,C.low b,C.low c,C.low d,C.low e)
  (ha,hb,hc,hd,he) = (C.high a,C.high b,C.high c,C.high d,C.high e)
  low_avg = (la + lb + lc + ld + le) / 5
  low_rates = (rate la low_avg,rate lb low_avg,rate lc low_avg,rate ld low_avg,rate le low_avg)
  min_low_rate = foldl1 min $ map ($low_rates) [_2,_3,_4]
  first_low_rate_diff = _1 low_rates - min_low_rate
  last_low_rate_diff = _5 low_rates - min_low_rate
  rate a avg = (a / avg - 1) * 100
  high_avg = (ha + hb + hc + hd + he) / 5
  high_rates = (rate ha high_avg,rate hb high_avg,rate hc high_avg,rate hd high_avg,rate he high_avg)
  max_high_rate = foldl1 max $ map ($high_rates) [_2,_3,_4]
  first_high_rate_diff = max_high_rate -  _1 high_rates
  last_high_rate_diff = max_high_rate -  _5 high_rates



_5 :: (a, b, c, d, e) -> e
_5 (_,_,_,_,a)= a

linerFun :: Double -> Double -> Double -> Double
linerFun b w x = w*x + b

lossDw b w (x,yp) = 2*(x*w+b-yp)*x
lossDb b w (x,yp)= 2*(x*w+b-yp)
loss b w xyps = foldl (+) 0 mse / len where
  len = toEnum $ length xyps
  mse = map mse' xyps
  mse' (x,y) = (linerFun b w x - y) ^ 2

stepGradient :: Double -> Double -> Double ->[(Double,Double)] -> (Double,Double)
stepGradient rate b w xyps = r where
  r = (b - db/len * rate,w - dw/len * rate)
  len = toEnum $ length xyps 
  dw = foldl' (+) 0 $ map (lossDw b w) $ xyps
  db = foldl' (+) 0 $ map (lossDb b w) $ xyps

testGen :: IO [(Double,Double)]
testGen = do
  let xs = ["1633.69","1647.945","1635.305","1629.555","1680.915","1675.435","1712.585","1706.625","1755.655","1771.295","1771.495","1778.28","1773.71","1760.57","1750.87","1738.185","1740","1749.6","1755.79","1753.555","1741.785","1749.575","1768.235","1799.105"]
  forM (zip xs [1,2..]) $ \(v,i) -> do
    pure $ (i,read v)
  -- forM [1,2..10] $ \ x -> do
  --   r <- randomRIO (-10,10)
  --   pure $ (x,linerFun 7 1.5 x+r)
testMain = do
  xyps <- testGen
  let (b,w) = gradient 30000 (0,1) xyps
  putStrLn "Real data:"
  putStrLn $ cs $ A.encode $ map (\(a,b)->(b,(printf "%02d" (fromEnum a) :: String))) $ xyps
  putStrLn $ "Params:" <> show (b,w)
  printf "Loss:%.5f\n" $ (loss b w xyps)
  putStrLn "LinderFn Data:"
  putStrLn $ cs $ A.encode $ map (\(a,b)->(b,(printf "%02d" (fromEnum a) :: String))) $ map (linerFn' b w) xyps
  where gradient 0 (b,w) _ = (b,w)
        gradient n (b,w) xyps = gradient (n-1) (b',w') xyps where
          (b',w') = stepGradient 0.001 b w xyps
        linerFn' b w (x,_) = (x,linerFun b w x)
