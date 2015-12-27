{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Store where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)

import           Data.Maybe
import           Data.Set             (fromList, member)

import           Data.Acid            (Query, Update, makeAcidic)
import           Data.Data            (Data, Typeable)
import           Data.IxSet           (Indexable, IxSet, empty, ixGen, ixSet,
                                       toList, (@+), (@=))
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

data Repo = Repo {
  sessionId :: SessionId,
  serial :: Serial,
  objects :: IxSet RepoObject,
  deltas :: IxSet Delta,
  latestSynced :: Serial
} deriving (Show, Typeable, Data)

$(deriveSafeCopy 0 'base ''Repo)


data Deltas = Deltas (IxSet Delta)
  deriving (Show, Typeable, Data)

instance Indexable Delta where
  empty = ixSet [
      ixGen (IX.Proxy :: IX.Proxy ClientId),
      ixGen (IX.Proxy :: IX.Proxy Serial)
    ]

$(deriveSafeCopy 0 'base ''Deltas)

type RepoState = (SessionId, Serial, [RepoObject], [Delta], Serial)

getByClientId :: ClientId -> Query Repo [RepoObject]
getByClientId = getByA

getByURI :: URI -> Query Repo (Maybe RepoObject)
getByURI uri = do
  o <- getByA uri
  return $ listToMaybe o

getByURIs :: [URI] -> Query Repo [RepoObject]
getByURIs uris = do
  Repo { objects = objs } <- ask
  return $ toList $ objs @+ uris

getByA :: Typeable a => a -> Query Repo [RepoObject]
getByA f = do
  Repo { objects = objs } <- ask
  return $ toList $ objs @= f

getInfo :: Query Repo (SessionId, Serial, Serial)
getInfo = do
  Repo { sessionId = sId, serial = s, latestSynced = ls } <- ask
  return (sId, s, ls)

getAllObjects :: Query Repo [RepoObject]
getAllObjects = do
  Repo { objects = objs } <- ask
  return $ toList objs

getDelta :: Serial -> Query Repo (Maybe Delta)
getDelta serial = do
  Repo { deltas = d }  <- ask
  return $ listToMaybe $ toList $ d @= serial

getRepo :: Query Repo RepoState
getRepo = do
  Repo sessionId serial objects deltas latest <- ask
  return (sessionId, serial, toList objects, toList deltas, latest)

removeDelta :: Serial -> Update Repo Repo
removeDelta serial = do
  r @ Repo { deltas = d } <- get
  let r1 = r { deltas = IX.deleteIx serial d }
  put r1
  return r1


applyActions :: ClientId -> Delta -> [Action] -> Update Repo Repo
applyActions cId delta actions = do
  oldR @(Repo _ s objects deltas _) <- get
  -- TODO replace Add and Update with just one of them
  let toDelete = fromList [ uri | Delete_ uri <- actions ]
  let adds    = [ RepoObject { uri = u, base64 = base64, clientId = cId } | Add_  (u, base64) <- actions, keep u toDelete ]
  let updates = [ RepoObject { uri = u, base64 = base64, clientId = cId } | Update_ (u, base64) <- actions, keep u toDelete ]
  let newR = foldl (\accum r -> IX.updateIx (uri r) r accum) objects $ adds ++ updates
  let newD = IX.updateIx serial delta deltas
  let repo = oldR { serial = next s, objects = newR, deltas = newD }
  put repo
  return repo
  where
    next (Serial s) = Serial (s + 1)
    keep u toDelete = not $ u `member` toDelete

markSync :: Update Repo Repo
markSync = do
  oldR @ Repo { serial = s } <- get
  let repo = oldR { latestSynced = s }
  put repo
  return repo

initialStore :: SessionId -> Repo
initialStore sId = Repo sId (Serial 0) IX.empty IX.empty (Serial 0)

$(makeAcidic ''Repo [ 'getByURI, 'getByURIs, 'getInfo, 'getDelta, 'removeDelta, 'getAllObjects, 'getRepo, 'applyActions, 'markSync ])
$(makeAcidic ''Deltas [ ])
