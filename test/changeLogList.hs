import           Control.Concurrent.STM     as S
import qualified STMContainers.Map          as TMap
import qualified ListT                      as LT
import           Control.Monad
import           Debug.Trace
import System.Random
import Control.Concurrent.Async
import           Options

type StoredObject = String
data Change = Update String StoredObject | Remove String
 deriving (Show, Eq)

type AppState = (TMap.Map String StoredObject, TVar [Change])

traceKey :: [Change] -> [String]
traceKey changes = [ case c of
                      Update key _ -> key
                      Remove key ->   key
                    | c <- changes ]

update :: AppState -> [Change] -> Bool -> IO AppState
update appState changes useChangeLog = atomically $ do
  let (tm, changeLog) = appState
  sequence_ [ case c of
                Update key val -> do
                  v <- TMap.lookup key tm
                  case v of
                    Nothing -> TMap.insert key val tm
                    Just _  -> return ()
                Remove key     -> TMap.delete key tm
              | c <- changes ]
  when useChangeLog $ modifyTVar' changeLog (\log_ -> changes ++ log_)
  trace ("processing: " ++ show (traceKey changes)) $ return (tm, changeLog)


randString :: Int -> IO String
randString n = liftM (take n . randomRs ('a','z')) newStdGen

randInt :: Int -> IO Int
randInt n = liftM (fst . randomR (1, n)) newStdGen


randUpdateSet :: Int -> IO [Change]
randUpdateSet n = do
    i <- randInt n
    replicateM i $ do
      (k1, v1) <- (,) <$> randString 30 <*> randString 1000
      return $ Update k1 v1

updater :: AppState -> Int -> Bool -> IO ()
updater state changes useChangeLog = replicateM_ changes $ do
  changeSet <- randUpdateSet 100
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
