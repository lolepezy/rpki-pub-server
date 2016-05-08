{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

module Repo.State where

import qualified Control.Concurrent.AdvSTM  as AS
import           Control.Concurrent.STM     as S
import           Control.Exception.Base
import           Control.Monad
import           Data.Data                  (Typeable)
import           Data.Function              (on)
import           Data.List                  (sortBy)
import qualified ListT                      as LT
import           Network.URI
import qualified STMContainers.Map          as TMap
import qualified STMContainers.Multimap     as TMMap

import qualified Data.ByteString.Lazy.Char8 as L

import           Data.Acid
import           Data.Acid.Advanced         (update')

import           Config
import qualified Store                      as ST
import           Types
import qualified XML                        as XS


type RRDPValue r = Either RRDPError r

type RRDPResponse = RRDPValue L.ByteString

type ChangeSet = [QueryPdu]

type RepoState  = [(URI, (Base64, ClientId))]
type TRepoMap   = TMap.Map URI (Base64, ClientId)
type TClientMap = TMMap.Multimap ClientId URI
type TChangeLog = (TMap.Map Integer ChangeSet, TVar Integer)

data TRepoState = TRepoState {
  rMap      :: TRepoMap,
  cMap      :: TClientMap,
  changeLog :: TChangeLog
}

data AppState = AppState {
  currentState :: TRepoState,
  acidRepo     :: AcidState ST.Repo,
  changeSync   :: ChangeSet -> STM (),
  appConfig    :: AppConfig
}

data RollbackException = RollbackException [Action] deriving (Typeable, Show)

instance Exception RollbackException

tInsert :: TRepoState -> Base64 -> ClientId -> URI -> STM ()
tInsert TRepoState{..} base64 clientId uri = do
  TMap.insert (base64, clientId) uri rMap
  TMMap.insert uri clientId cMap

tDelete :: TRepoState -> ClientId -> URI -> STM ()
tDelete TRepoState{..} clientId uri = do
  TMap.delete uri rMap
  TMMap.delete uri clientId cMap


initialTRepo :: [ST.RepoObject] -> STM TRepoState
initialTRepo repoObjects = do
  (rmap, cmap) <- (,) <$> TMap.new <*> TMMap.new
  mapM_ (\(ST.RepoObject cId u b64) -> do
    TMap.insert (b64, cId) u rmap
    TMMap.insert u cId cmap) repoObjects

  logMap  <- TMap.new
  counter <- newTVar 0
  return TRepoState { rMap = rmap, cMap = cmap, changeLog = (logMap, counter) }


processMessage :: AppState -> ClientId -> L.ByteString -> IO (AppState, Reply)
processMessage appState clientId queryXml =
  case XS.parseMessage queryXml of
    Left e                    -> return (appState, Errors [XMLError e])
    Right ListMessage         -> listObjects appState clientId
    Right (PduMessage _ pdus) -> applyActionsToState appState clientId pdus


listObjects :: AppState -> ClientId -> IO (AppState, Reply)
listObjects a @ AppState { currentState = TRepoState{..} } clientId = atomically $ do
  uris <- LT.toList $ TMMap.streamByKey clientId cMap
  objs <- mapM (\u -> (u,) <$> TMap.lookup u rMap) uris
  return (a, ListReply [ ListPdu u h | (u, Just (Base64 _ h, _)) <- objs ])


applyActionsToState :: AppState -> ClientId -> [QueryPdu] -> IO (AppState, Reply)
applyActionsToState appState @ AppState {
    currentState = repoState @ TRepoState{..},
    acidRepo = repo,
    changeSync = chSync }
    clientId pdus = do
  actions <- applyToState `catch` rollbackActions
  return (appState, reply actions)
  where
    applyToState = AS.atomically $ do
      actions <- AS.liftAdv $ tActionSeq clientId pdus repoState
      case [ e | Wrong_ e <- actions ] of
        [] -> do
          AS.liftAdv $ do
            updateChangeLog changeLog
            chSync pdus
          AS.onCommit $ void $ update' repo (ST.ApplyActions clientId actions)
        _ ->
          AS.liftAdv $ throwSTM $ RollbackException actions

      return actions

    rollbackActions (RollbackException as) = return as

    updateChangeLog :: TChangeLog -> STM ()
    updateChangeLog (changeMap, counter) = do
      c <- readTVar counter
      TMap.insert pdus c changeMap
      writeTVar counter (c + 1)

    reply actions = case [ e | Wrong_ e <- actions ] of
      [] -> Success
      as -> Errors as


stateSnapshot :: TRepoMap -> STM RepoState
stateSnapshot tmap = LT.toList (TMap.stream tmap)

changeLogSnapshot :: TChangeLog -> STM [(Integer, ChangeSet)]
changeLogSnapshot (chLog, _) = do
  chl <- LT.toList (TMap.stream chLog)
  return $ sortBy (compare `on` fst) chl


{-
  Create actions only until the first error to avoid
  redundant STM modify/rollback overhead.
 -}
tActionSeq :: ClientId -> [QueryPdu] -> TRepoState -> STM [Action]
tActionSeq clientId pdus repoState @ TRepoState{..} = go pdus
 where
  go :: [QueryPdu] -> STM [Action]
  go [] = return []
  go (pdu: otherPdus) = do
    action <- makeAction pdu
    case action of
      w@(Wrong_ _) -> return [w]
      a            -> applyActionToMap a >> (a : ) <$> go otherPdus


  applyActionToMap :: Action -> STM Action
  applyActionToMap a@(AddOrUpdate_ (uri, base64)) = const a <$> tInsert repoState base64 clientId uri
  applyActionToMap a@(Delete_ uri)                = const a <$> tDelete repoState clientId uri
  applyActionToMap a                              = return a

  makeAction :: QueryPdu -> STM Action
  makeAction p =
   let
     withClientIdCheck objUri sClientId f
         | sClientId == clientId = f
         | otherwise = Wrong_ PermissionFailure { oUri = objUri, storedClientId = sClientId, queryClientId = clientId }

     withHashCheck queryHash storedHash f
         | queryHash == storedHash = f
         | otherwise = Wrong_ $ NoObjectMatchingHash storedHash p

     lookupObject :: URI -> STM (Maybe (Base64, ClientId))
     lookupObject u = TMap.lookup u rMap
     in
     case p of
       QP (Publish uri base64 Nothing _) -> do
         o <- lookupObject uri
         return $ case o of
           Nothing -> AddOrUpdate_ (uri, base64)
           Just _  -> Wrong_ $ ObjectAlreadyPresent p

       QP (Publish uri base64 (Just hash) _) -> do
         o <- lookupObject uri
         return $ case o of
           Nothing -> Wrong_ $ NoObjectPresent uri hash
           Just (Base64 _ h, cId) -> withClientIdCheck uri cId $ withHashCheck hash h $ AddOrUpdate_ (uri, base64)

       QW (Withdraw uri hash _) -> do
         o <- lookupObject uri
         return $ case o of
           Nothing -> Wrong_ $ NoObjectPresent uri hash
           Just (Base64 _ h, cId) -> withClientIdCheck uri cId $ withHashCheck hash h $ Delete_ uri
