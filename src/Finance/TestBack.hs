{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module Finance.TestBack where
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.Trans.Writer (WriterT (runWriterT),tell)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Default.Class (Default(def))
import Web.AppM (MaybeT(runMaybeT), forM_, forM)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import I.Candle (Candle (high, low,close, open, time))
import Module.TemplateGetter 
import I.FinanceMessage (OrderOpened, Direction (Grow, Shrink))
import Control.Monad (guard)
import Control.Applicative((<|>))
import Data.Foldable (Foldable(fold))
import I.Strategies (piFifth,StrategyAction(..), closeOrder, PIConfig (emplitude, ratio))
import Module.Log (info,info')
import Data.List (find, partition)
import GHC.Float (float2Double, double2Float)
import Text.Printf (printf)
import Data.String.Conversions (cs)
import qualified Data.Time as T
import Kit (mkTimeFromTimestamp, toLocalTime)
import qualified Data.Aeson as A

data MyState = MyState
  { candles :: ![Candle]
  , orders :: ![OrderOpened]
  } deriving (Show,Eq)

instance Default MyState where
  def :: MyState
  def = MyState
    { candles = []
    , orders = []
    }

mkSetAndOver ''MyState
type MyLog = (Double,Text,Text)
type TestBackT m = MaybeT (StateT MyState (WriterT [MyLog] m))

getState :: Monad m => TestBackT m MyState
getState = lift $ get
getCandles :: Monad m => TestBackT m [Candle]
getCandles = do
  a <- getState
  pure $ candles a
getOrders :: Monad m => TestBackT m [OrderOpened]
getOrders = do
  a <- getState
  pure $ orders a
putState :: Monad m =>MyState -> TestBackT m ()
putState state = lift $ put state
-- 数量限制100
putCandle :: Monad m => Candle -> TestBackT m ()
putCandle candle = do
  s <- getState
  let a = over_candles ((candle:).take 100) s
  putState a

-- 数量限制100
putOrder :: Monad m => OrderOpened -> TestBackT m ()
putOrder order = do
  s <- getState
  let a = over_orders (order:) s
  putState a
removeOrder id = do
  s <- getState
  let a = over_orders (filter ((/= id)._1)) s
  putState $ a
updateOrder order = do
  s <- getState
  let a = over_orders (map update) s
  putState $ a
  where update o | _1 o == _1 order = order
                 | otherwise = o
flog :: Monad m => (Double,Text) -> TestBackT m ()
flog a = do
  [candle] <- takeCandles 1
  let utime = mkTimeFromTimestamp $ time candle
  let time = toLocalTime utime 
  let timeStr = T.formatTime T.defaultTimeLocale ("%Y-%m-%d %H") time
  lift $ lift $ tell [(_1 a, _2 a,cs timeStr)]

runTestBackT :: Monad m => TestBackT m a -> m ((Maybe a, MyState), [MyLog])
runTestBackT test = result where
  result =  runWriterT $ runStateT (runMaybeT test) def

runTestBackLog :: Monad m => TestBackT m a  -> m [MyLog]
runTestBackLog test = do
  (_,logs) <- runTestBackT test
  pure logs

takeCandles :: Monad m=> Int -> TestBackT m [Candle]
takeCandles n = do
  candles <- getCandles
  guard $ length candles >= n
  pure $ take n candles
takeFiveCandles = fmap tail $ takeCandles 6

tangN :: Monad m => (Candle -> Double) -> (Double -> Double -> Double) -> Int -> TestBackT m Double
tangN selector select n = do
  candles <- takeCandles (n+1)
  pure $ foldl1 select $ map selector $ tail candles

tangHigh :: Monad m=> Int -> TestBackT m Double
tangHigh = tangN high max
tangLow :: Monad m=> Int -> TestBackT m Double
tangLow = tangN low min

test :: TestBackT IO ()
test = do
  orderCheck
  undefined

f2d = float2Double
d2f = double2Float

closeMyOrder price order author = result where
  result = if _3 order == Grow then (boughtO,str) else (soldO,str)
  boughtO =  price - f2d (_4 order)
  soldO = f2d (_4 order) - price
  str = closeOrder order (d2f price) author <> "at" <>(cs $ show $ _4 order)
printLogs :: [MyLog] -> IO ()
printLogs logs' = do
  let logs = reverse $ foldl fn [] logs'
  forM logs $ \log -> do
    printf "\x1b[37m%+-.2f\t%s\t%s\n" (_1 log) ((cs $ _2 log) :: String) ((cs $ _3 log) :: String)
  printf "Total: %d\n" (length logs)
  putStrLn $ cs $ A.encode $ map (\a->(_1 a,_3 a)) $ logs
  putStrLn "\x1b[0m Done"
  where fn [] a = [a]
        fn r@(x:xs) a = over_1 (+ _1 x) a : r
split :: Char -> String -> [String]
split = split' []
split' :: [String] -> Char -> String -> [String]
split' last separator str = result where
  result = if null rest then reverse $ a:last else split' (a:last) separator (tail rest)
  (a,rest) = break eqSeparator str
  -- break [] last = (last,[])
  -- break text@(t:ts) last = if separator == take len text then (reverse last,drop len text) else break ts (t:last)
  eqSeparator = (==separator)
readCandles :: IO [Candle]
readCandles = do
  text <- readFile "/Users/rezero/Documents/finance/xauusd/xauusd-20221110.csv"
  let a = map (split ',') $ tail $ lines text
  pure $ map trans a
  where trans (time:o:h:l:c:_) = def
          { open = read o
          , low = read l
          , close = read c
          , high = read h
          , time = 1000 * read time
          }
        trans _ = def
moveStop moveStoper = do
  orders <- getOrders
  [nowC] <- takeCandles 1
  forM orders $ \ order -> do
    if _3 order == Grow then do
      let mover = high nowC - moveStoper
      if d2f mover > _5 order then do
        updateOrder (_1 order,_2 order,_3 order,_4 order,d2f mover,_6 order)
      else pure ()
    else do
      let mover = low nowC + moveStoper
      if d2f mover < _5 order then do
        updateOrder (_1 order,_2 order,_3 order,_4 order,d2f mover,_6 order)
      else pure ()
  pure ()
orderCheck = do
  orders <- getOrders
  [nowC] <- takeCandles 1
  forM orders $ \ order -> do
    if _3 order == Grow  then do
      if _5 order >= d2f (low nowC) then do
        removeOrder $ _1 order
        flog $ closeMyOrder (f2d $ _5 order) order "stoper__"
      else if high nowC >= f2d (_6 order) then do
        removeOrder $ _1 order
        flog $ closeMyOrder (f2d $ _6 order) order "targeter"
      else pure ()
    else do
      if d2f (high nowC) >= _5 order then do
        removeOrder $ _1 order
        flog $ closeMyOrder (f2d $ _5 order) order "stoper__"
      else if high nowC <= f2d (_6 order) then do
        removeOrder $ _1 order
        flog $ closeMyOrder (f2d $ _6 order) order "targeter"
      else pure ()
  pure ()
_6 (a,b,d,c,e,f) = f
_5 (a,b,d,c,e,f) = e
testBackPiFifty :: Text -> PIConfig -> TestBackT IO ()
testBackPiFifty code conf = do
  five <- takeFiveCandles
  orders <- getOrders
  let actions = piFifth code conf (five !! 4,five !! 3,five !! 2,five !! 1,five !! 0) orders
  forM_ actions $ \ action -> case action of 
    Open order -> do
      let norder = set_1 (if _3 order == Grow then 1 else 2) order
      -- info $ show norder
      putOrder norder
    Close id -> do
      order <- MaybeT $ pure $ find ((==id)._1) orders
      flog $ closeMyOrder (close $ head five) order "pififty_"
      removeOrder id 
thereIsNot n = do
  orders <- getOrders
  guard $ find ((==n)._3) orders == Nothing
testBackTangBought :: TestBackT IO ()
testBackTangBought = do
  thereIsNot Grow
  l' <- tangLow 15
  let l = d2f l'
  [candle] <- takeCandles 1
  guard $ low candle <= l'
  putOrder (1,"XAUUSD",Grow,l,l*(1-0.003),l*(1+0.012))
  pure () 

testBackTangSold :: TestBackT IO ()
testBackTangSold = do
  thereIsNot Shrink
  h' <- tangHigh 15
  let h = d2f h'
  [candle] <- takeCandles 1
  guard $ high candle >= h'
  putOrder (2,"XAUUSD",Shrink,h,h*(1+0.003),h*(1-0.012))
  pure () 

testBackTest :: TestBackT IO ()
testBackTest = do
  flog (2,"2122")

simulateMarket = do
  candles <- readCandles
  forM [0.6] $ \n-> do
    let config = def {emplitude=n}
    logs <- runTestBackLog $ forM candles $ \ candle -> do
      putCandle candle
      testBackPiFifty "XAUUSD" config <|> pure ()
      orderCheck <|> pure ()
      -- moveStop 70 <|> pure ()
    let (bs,ss) = partition (\a-> T.take 1 (_2 a) == "多") logs
    printLogs bs
    putStrLn "--------------"
    printLogs ss
    -- printLogs logs
    putStrLn $ "======" ++ show n
  pure ()

simulateTangMarket = do
  candles <- readCandles
  forM [15] $ \n-> do
    logs <- runTestBackLog $ forM candles $ \ candle -> do
      putCandle candle
      testBackTangBought <|> pure ()
      testBackTangSold <|> pure ()
      orderCheck <|> pure ()
    let (bs,ss) = partition (\a-> T.take 1 (_2 a) == "多") logs
    printLogs bs
    putStrLn "--------------"
    printLogs ss
    -- printLogs logs
    putStrLn $ "======" ++ show n
  pure ()