{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

module Store where

import           Control.Monad (void)
import           Control.Monad.Reader (ask)
import           Control.Monad.State  (get, put)
import           Data.Set             (fromList, member)

import           Data.Acid            (AcidState, Query, Update, makeAcidic)
import           Data.Acid.Advanced   (query', update')
import           Data.Data            (Data, Typeable)
import           Data.IxSet           (Indexable, IxSet, empty, ixGen, ixSet,
                                       toList)
import qualified Data.IxSet           as IX
import           Data.SafeCopy        (base, deriveSafeCopy)

import           Network.URI
import           Types

data RepoObject = RepoObject ClientId URI Base64
  deriving (Show, Eq, Ord, Typeable, Data)

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

type RepoState = AcidState Repo

getAllObjects_ :: Query Repo [RepoObject]
getAllObjects_ = do
  Repo {..} <- ask
  return $ toList objects

applyActions_ :: ClientId -> [Action] -> Update Repo Repo
applyActions_ cId actions = do
  oldR @ (Repo objects) <- get
  let toDelete = fromList [ uri | Delete_ uri <- actions ]
  let updates = [ RepoObject cId u base64 | AddOrUpdate_ (u, base64) <- actions, keep u toDelete ]
  let newR = foldl (\accum r@(RepoObject _ u _) -> IX.updateIx u r accum) objects updates
  let repo = oldR { objects = newR }
  put repo
  return repo
  where
    keep u toDelete = not $ u `member` toDelete


initialStore :: Repo
initialStore = Repo IX.empty


$(makeAcidic ''Repo [ 'getAllObjects_, 'applyActions_ ])

getAllObjects :: AcidState Repo -> IO [RepoObject]
getAllObjects acid = query' acid GetAllObjects_

applyActions :: AcidState Repo -> ClientId -> [Action] -> IO ()
applyActions acid clientId actions = void $ update' acid (ApplyActions_ clientId actions)
