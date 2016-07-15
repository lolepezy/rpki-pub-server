module Log (
  newLogger,
  module System.Logger
  ) where

import           Control.Monad.IO.Class
import qualified Data.Text              as T
import           System.Logger

newLogger :: MonadIO m => String -> m Logger
newLogger n = new (setName ((Just . T.pack) n) . setOutput StdOut $ defSettings)
