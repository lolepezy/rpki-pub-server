module Log (warn_, error_, info_) where

import           Data.Time.Clock
import           Data.Time.Format ()
-- import           System.Log.FastLogger

-- TODO Implement proper logging using fast-logger

log_ :: Show s => String -> s -> IO ()
log_ level s = do
  t0 <- getCurrentTime
  print $ show t0 ++ " " ++ level ++ " " ++ show s

warn_ :: Show s => s -> IO ()
warn_ = log_ "WARN"

info_ :: Show s => s -> IO ()
info_ = log_ "INFO"

error_ :: Show s => s -> IO ()
error_ = log_ "ERROR"
