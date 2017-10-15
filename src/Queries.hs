{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
module Queries
  (
    runExtDB
  , getScanPages
  , getPage
  , unValue
  ) where
import           ClassyPrelude.Yesod     hiding (groupBy, on, (==.))
import           Control.Monad.Logger    (NoLoggingT)
import           Database.Esqueleto
import           Database.Persist.Sql    (parseMigration)
import           Database.Persist.Sqlite (runSqlite)
import           Model


type QueryIO m = (MonadIO m, MonadBaseControl IO m)
type MonadQuery m a = (QueryIO m) => ReaderT SqlBackend m a
-- Some helpers (probably should live in Model/Application/Fondation etc.)
-- some place it can make in into Import
runExtDB :: (MonadIO m, MonadBaseControl IO m) =>
  Text ->
  ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runExtDB file = runSqlite file . asSqlBackendReader
  where
    asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
    asSqlBackendReader = id

isExtDBOK :: (MonadBaseControl IO m, MonadIO m) => Text -> m Bool
isExtDBOK file = runExtDB file $ do
  mig <- parseMigration migrateAllScan
  return $ case mig of
    Left _    -> False
    Right sql -> null sql

getScanPages ::(QueryIO m) => MonadQuery m [(Entity Page, Int)]
getScanPages = do
  pages <- select $
    from $ \(p `InnerJoin` pl) -> do
    on $ p ^. PageId ==. pl ^. PageLinkLink
    groupBy (p ^. PageId, p ^. PagePage)
    let num = countRows
    orderBy [desc num]
    limit 50
    return (p, num)
  return [(p, unValue n) | (p,n) <- pages]


getPage  ::(QueryIO m) => Int -> MonadQuery m (Maybe (Entity Page))
getPage key = do
  result <- select $
    from $ \p -> do
    where_ (p ^. PageId ==. valkey (fromIntegral key))
    limit 1
    return p
  case headMay result of
    Nothing -> return Nothing
    Just r -> return (Just r)
