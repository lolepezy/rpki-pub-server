{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Log (warn_, error_, info_) where

import           Data.Time.Clock
import           Data.Time.Format      ()
import           System.Log.FastLogger

-- TODO Implement proper logging using fast-logger


-- TODO find a better way to do it
class ToString a where
  toString :: a -> String

instance ToString String where toString = id
instance ToString Char where toString x = [x]
instance Show a => ToString a where toString = show


log_ :: (Show s, ToString s) => String -> s -> IO ()
log_ level s = do
  t0 <- getCurrentTime
  putStrLn $ show t0 ++ " " ++ level ++ " " ++ toString s

warn_ :: (Show s, ToString s) => s -> IO ()
warn_ = log_ "WARN"

info_ :: (Show s, ToString s) => s -> IO ()
info_ = log_ "INFO"

error_ :: (Show s, ToString s) => s -> IO ()
error_ = log_ "ERROR"
