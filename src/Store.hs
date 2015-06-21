{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Store
    (
    ) where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)

import           Data.Acid            (AcidState, Query, Update, makeAcidic,
                                       openLocalState)
import           Data.Data            (Data, Typeable)
import           Data.IxSet           (Indexable, IxSet, empty, ixGen, ixSet,
                                       toList, (@=))
import qualified Data.IxSet           as IX
import           Data.SafeCopy        (base, deriveSafeCopy)

import           Network.URI
import           Types


newtype ClientId = ClientId String
  deriving (Show, Eq, Ord, Typeable, Data)

$(deriveSafeCopy 0 'base ''ClientId)

data RepoObject = RepoObject {
  clientId :: String,
  uri      :: URI,
  base64   :: Base64
} deriving (Show, Eq, Ord, Typeable, Data)

$(deriveSafeCopy 0 'base ''RepoObject)

instance Indexable RepoObject where
  empty = ixSet [
      ixGen (IX.Proxy :: IX.Proxy ClientId),
      ixGen (IX.Proxy :: IX.Proxy URI)
    ]

data Repo = Repo (IxSet RepoObject)
  deriving (Show, Typeable, Data)

$(deriveSafeCopy 0 'base ''Repo)

list :: String -> Query Repo [RepoObject]
list clientId = do
  Repo objs <- ask
  return $ toList $ objs @= clientId

add :: RepoObject -> Update Repo ()
add obj = do
  Repo r <- get
  put $ Repo $ IX.updateIx (uri obj) obj r

   
