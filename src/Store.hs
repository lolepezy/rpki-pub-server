{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Store where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)

import           Data.Acid            (Query, Update, makeAcidic, EventResult)
import           Data.Data            (Data, Typeable)
import           Data.IxSet           (Indexable, IxSet, empty, ixGen, ixSet,
                                       toList, (@=), (@+))
import qualified Data.IxSet           as IX
import           Data.SafeCopy        (base, deriveSafeCopy)

import           Network.URI
import           Types

data RepoObject = RepoObject {
  clientId :: ClientId,
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

delete :: URI -> Update Repo ()
delete uri = do
  Repo r <- get
  put $ Repo $ IX.deleteIx uri r

getByClientId :: ClientId -> Query Repo [RepoObject]
getByClientId = getByA

getByURI :: URI -> Query Repo (Maybe RepoObject)
getByURI uri = do
  o <- getByA uri
  return $ case o of
    [x] -> Just x
    _   -> Nothing

getByURIs :: [URI] -> Query Repo [RepoObject]
getByURIs uris = do
  Repo objs <- ask
  return $ toList $ objs @+ uris

getByA :: Typeable a => a -> Query Repo [RepoObject]
getByA f = do
  Repo objs <- ask
  return $ toList $ objs @= f

initialStore :: Repo
initialStore = Repo IX.empty

$(makeAcidic ''Repo [ 'add, 'delete, 'getByURI, 'getByURIs ])
