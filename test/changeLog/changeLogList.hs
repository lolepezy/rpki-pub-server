{-# LANGUAGE BangPatterns #-}
import           Control.Concurrent.STM     as S
import qualified STMContainers.Map          as TMap
import qualified ListT                      as LT
import qualified Data.ByteString.Char8 as L
import           Control.Monad
import           Debug.Trace
import System.Random
import Control.Concurrent.Async
import           Options

type Key = Int
type StoredObject = L.ByteString
data Change = Update Key StoredObject | Remove Key
 deriving (Show, Eq)

type AppState = (TMap.Map Key StoredObject, TVar [Change])

traceKey :: [Change] -> [Key]
traceKey changes = [ case c of
                      Update key _ -> key
                      Remove key ->   key
                    | c <- changes ]

update :: AppState -> [Change] -> Bool -> IO AppState
update appState !changes useChangeLog = atomically $ do
  let (tm, changeLog) = appState
  sequence_ [ case c of
                Update key val -> do
                  v <- TMap.lookup key tm
                  case v of
                    Nothing -> TMap.insert val key tm
                    Just _  -> return ()
                Remove key  -> TMap.delete key tm
              | c <- changes ]
  when useChangeLog $ modifyTVar' changeLog (\log_ -> changes ++ log_)
  -- trace ("processing: " ++ show (traceKey changes)) $
  return (tm, changeLog)


randString :: Int -> IO L.ByteString
randString n = liftM (L.pack . take n . randomRs ('a','z')) newStdGen
randStringFake n = return $ L.pack $ replicate n 'z'

randInt :: Int -> IO Int
randInt n = liftM (fst . randomR (1, n)) newStdGen


randUpdateSet :: Int -> IO [Change]
randUpdateSet n = do
    !i <- randInt n
    replicateM i $ do
      (!k1, !v1) <- (,) <$> randInt 4000000000 <*> randStringFake 1000
      return $ Update k1 v1

updater :: AppState -> Int -> Bool -> IO ()
updater state changes useChangeLog = replicateM_ changes $ do
  !changeSet <- randUpdateSet 250
  void $ update state changeSet useChangeLog


data Opts = Opts Int Int Bool

instance Options Opts where
  defineOptions = pure Opts
      <*> simpleOption "threads" 10 "Number of threads"
      <*> simpleOption "changes" 50 "Number of changes"
      <*> simpleOption "use-change-log" True "Use change log or not"

main :: IO ()
main = runCommand $ \(Opts threads changes useChangeLog) _ -> do
  initState <- atomically $ (,) <$> TMap.new <*> newTVar []
  as <- replicateM threads $ async $ updater initState changes useChangeLog
  mapM_ wait as
  -- force all the data to be calculated
  s <- atomically $ LT.toList $ TMap.stream $ fst initState
  trace ("length = " ++ show (length s)) $ return ()
