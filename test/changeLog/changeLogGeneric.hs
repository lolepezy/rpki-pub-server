{-# LANGUAGE BangPatterns #-}
import           Control.Concurrent.STM     as S
import          Control.Concurrent.STM.Stats (trackNamedSTM, dumpSTMStats)
import qualified STMContainers.Map          as TMap
import qualified ListT                      as LT
import qualified Data.ByteString.Char8 as L
import           Data.IORef
import           Control.Monad
import System.Random
import Control.Concurrent.Async
import           Options

type Key = Int
type StoredObject = L.ByteString
data Change = Update Key StoredObject | Remove Key
 deriving (Show, Eq)

type TListChangeLog = TVar [Change]
type TMapChangeLog = (TMap.Map Integer [Change], TVar Integer)

type Store = TMap.Map Key StoredObject
data AppState c = AppState {
  store :: Store,
  changeLog :: !c,
  updateChangeLog :: c -> [Change] -> STM c
}

mkListStore :: STM (AppState TListChangeLog)
mkListStore = do
   tm    <- TMap.new
   chLog <- newTVar []
   return AppState {
     store = tm,
     changeLog = chLog,
     updateChangeLog = \chl change -> do
       modifyTVar' chl (\log_ -> change ++ log_)
       return chl
   }

mkMapStore :: STM (AppState TMapChangeLog)
mkMapStore = do
  tm    <- TMap.new
  chMap <- TMap.new
  c     <- newTVar 0
  return AppState {
    store = tm,
    changeLog = (chMap, c),
    updateChangeLog = \(chm, counter) ch -> do
      curr <- readTVar counter
      TMap.insert ch curr chm
      writeTVar counter $ curr + 1
      return (chm, counter)
  }


updateState :: Store -> [Change] -> STM ()
updateState !tm !changes = sequence_ [
    case c of
      Update key val -> do
        v <- TMap.lookup key tm
        case v of
          Nothing -> TMap.insert val key tm
          Just _  -> return ()
      Remove key  -> TMap.delete key tm
    | c <- changes ]


update :: AppState c -> [Change] -> Bool -> String -> IO ()
update AppState {
    store = tm,
    changeLog = chLog,
    updateChangeLog  = updateChLog
 } !changes useChangeLog tag = trackNamedSTM tag $ do
  updateState tm changes
  when useChangeLog $ void $ updateChLog chLog changes


randString :: Int -> IO L.ByteString
randString n = fmap (L.pack . take n . randomRs ('a','z')) newStdGen

randInt :: Int -> IO Int
randInt n = fmap (fst . randomR (1, n)) newStdGen


updater :: AppState c -> Int -> Bool -> String -> IO ()
updater state changes useChangeLog tag = do
  !seed <- randInt $ 4 * 1000 * 1000 * 1000
  !keyCounter <- newIORef seed
  replicateM_ changes $ do
    !changeSet <- randUpdateSet keyCounter
    let !x = update state changeSet useChangeLog tag
    void x
    -- print $ "seed = " ++ show seed ++ " finished change set length = " ++ show (length changeSet)
  where
    randUpdateSet keyCounter = do
        !i <- randInt 500
        replicateM i $ do
          k <- readIORef keyCounter
          let v = randStringFake 1000
          atomicModifyIORef' keyCounter (\ii -> (ii + 1, ()))
          return $ Update k v

    randStringFake n = L.pack $ replicate n 'z'


data Opts = Opts Int Int Bool

instance Options Opts where
  defineOptions = pure Opts
      <*> simpleOption "threads" 10 "Number of threads"
      <*> simpleOption "changes" 50 "Number of changes"
      <*> simpleOption "use-change-log" True "Use change log or not"

testList :: Int -> Int -> Bool -> IO ()
testList threads changes useChangeLog = do
  !listStore <- trackNamedSTM "create list state" mkListStore
  let tag = "[list: " ++ show threads ++ "  " ++ show changes ++ "  " ++ show useChangeLog ++ "]"
  print tag
  as <- replicateM threads $ async $ updater listStore changes useChangeLog tag
  mapM_ wait as
  _ <- trackNamedSTM "get state" $ LT.toList $ TMap.stream $ store listStore
  return ()

testMap :: Int -> Int -> Bool -> IO ()
testMap threads changes useChangeLog = do
  !mapStore <- trackNamedSTM "create map state" mkMapStore
  let tag = "[map: " ++ show threads ++ "  " ++ show changes ++ "  " ++ show useChangeLog ++ "]"
  print tag
  as <- replicateM threads $ async $ updater mapStore changes useChangeLog tag
  mapM_ wait as
  _ <- trackNamedSTM "get state" $ LT.toList $ TMap.stream $ store mapStore
  return ()



main :: IO ()
main = do
  -- let ts = [ 1, 2, 5, 10, 20, 100 ]
  -- let chs = [ 10, 20, 50, 75, 100 ]
  let ts = [ 4 ]
  let chs = [ 100 ]
  sequence_ [ testList t c True  | t <- ts, c <- chs, t*c <= 10000 ]
  sequence_ [ testList t c False | t <- ts, c <- chs, t*c <= 10000 ]
  sequence_ [ testMap t c True  | t <- ts, c <- chs, t*c <= 10000 ]
  sequence_ [ testMap t c False | t <- ts, c <- chs, t*c <= 10000 ]
  dumpSTMStats
