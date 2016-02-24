{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Store where

import           Control.Monad.Reader      (ask)
import           Control.Monad.State       (get, put)
import           Data.Maybe
import           Data.Set                  (fromList, member)

import           Data.Acid                 (Query, Update, makeAcidic)
import           Data.Data                 (Data, Typeable)
import           Data.IxSet                (Indexable, IxSet, empty, ixGen,
                                            ixSet, toList, (@+), (@=))
import qualified Data.IxSet                as IX
import           Data.SafeCopy             (base, deriveSafeCopy)

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

data Repo = Repo {
  objects :: IxSet RepoObject
} deriving (Show, Typeable, Data)

$(deriveSafeCopy 0 'base ''Repo)

getByClientId :: ClientId -> Query Repo [RepoObject]
getByClientId = getByA

getByURI :: URI -> Query Repo (Maybe RepoObject)
getByURI uri = do
  o <- getByA uri
  return $ listToMaybe o

getByURIs :: [URI] -> Query Repo [RepoObject]
getByURIs uris = do
  Repo objs <- ask
  return $ toList $ objs @+ uris

getByA :: Typeable a => a -> Query Repo [RepoObject]
getByA f = do
  Repo objs <- ask
  return $ toList $ objs @= f

getAllObjects :: Query Repo [RepoObject]
getAllObjects = do
  Repo { objects = objs } <- ask
  return $ toList objs

getRepo :: Query Repo [(URI, RepoObject)]
getRepo = do
  Repo objs <- ask
  return [ (u, o) | o @ RepoObject  { uri = u } <- toList objs ]


applyActions :: ClientId -> [Action] -> Update Repo Repo
applyActions cId actions = do
  oldR @ (Repo objects) <- get
  let toDelete = fromList [ uri | Delete_ uri <- actions ]
  let updates = [ RepoObject { uri = u, base64 = base64, clientId = cId } | AddOrUpdate_ (u, base64) <- actions, keep u toDelete ]
  let newR = foldl (\accum r -> IX.updateIx (uri r) r accum) objects updates
  let repo = oldR {  objects = newR }
  put repo
  return repo
  where
    keep u toDelete = not $ u `member` toDelete


initialStore :: Repo
initialStore = Repo IX.empty

$(makeAcidic ''Repo [ 'getByURI, 'getByURIs, 'getAllObjects, 'getRepo, 'applyActions ])
