{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

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

add :: RepoObject -> Update Repo ()
add obj = do
  Repo r <- get
  put $ Repo $ IX.updateIx (uri obj) obj r

getByClientId :: ClientId -> Query Repo [RepoObject]
getByClientId = getByA

getByURI :: URI -> Query Repo [RepoObject]
getByURI = getByA

getByA :: Typeable a => a -> Query Repo [RepoObject]
getByA f = do
  Repo objs <- ask
  return $ toList $ objs @= f

$(makeAcidic ''Repo [ 'add ])
