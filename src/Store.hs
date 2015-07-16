{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Store where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)

import           Data.Acid            (Query, Update, makeAcidic)
import           Data.Data            (Data, Typeable)
import           Data.IxSet           (Indexable, IxSet, empty, ixGen, ixSet,
                                       toList, (@+), (@=))
import qualified Data.IxSet           as IX
import           Data.SafeCopy        (base, deriveSafeCopy)

import           Network.URI
import           Types                hiding (Delta)

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

data Delta = Delta Serial ClientId [QueryPdu]
  deriving (Show, Eq, Ord, Typeable, Data)

data Repo = Repo (IxSet RepoObject) (IxSet Delta)
  deriving (Show, Typeable, Data)

$(deriveSafeCopy 0 'base ''Repo)


data Deltas = Deltas (IxSet Delta)
  deriving (Show, Typeable, Data)

$(deriveSafeCopy 0 'base ''Delta)

instance Indexable Delta where
  empty = ixSet [
      ixGen (IX.Proxy :: IX.Proxy ClientId)
      -- ,ixFun $ \delta -> [ uri bp ]
    ]

$(deriveSafeCopy 0 'base ''Deltas)

add :: RepoObject -> Update Repo ()
add obj = do
  Repo r d <- get
  put $ Repo (IX.updateIx (uri obj) obj r) d

delete :: URI -> Update Repo ()
delete uri = do
  Repo r d <- get
  put $ Repo (IX.deleteIx uri r) d

getAll :: Query Repo [RepoObject]
getAll = do
  Repo objs d <- ask
  return $ toList objs

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
  Repo objs d <- ask
  return $ toList $ objs @+ uris

getByA :: Typeable a => a -> Query Repo [RepoObject]
getByA f = do
  Repo objs d <- ask
  return $ toList $ objs @= f


data ObjOperation a u d = Add_ a | Update_ u | Delete_ d

applyActions :: Repo -> ClientId -> Delta -> [ObjOperation (URI, Base64) (URI, Base64) URI] -> Update Repo Repo
applyActions repo cId delta@(Delta serial _ _) actions = do
  Repo objects deltas <- get
  -- TODO replace Add and Update with just one of them
  let adds    = [ RepoObject { uri = u, base64 = base64, clientId = cId } | Add_  (u, base64) <- actions ]
  let updates = [ RepoObject { uri  = u, base64 = base64, clientId = cId } | Update_ (u, base64) <- actions ]
  let newR = foldl (\accum r -> IX.updateIx (uri r) r accum) objects $ adds ++ updates
  let newD = IX.updateIx serial delta deltas
  put $ Repo newR newD
  return repo

initialStore :: Repo
initialStore = Repo IX.empty IX.empty

$(makeAcidic ''Repo [ 'add, 'delete, 'getByURI, 'getByURIs, 'applyActions ])
$(makeAcidic ''Deltas [ ])
