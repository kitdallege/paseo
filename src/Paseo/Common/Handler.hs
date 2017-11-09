{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Paseo.Common.Handler
    ( getAppStoragePath
    , getScanFileFullPath
    ) where
import           Import
import           System.Directory        (getAppUserDataDirectory)

getAppStoragePath :: (HandlerSite m ~ App, MonadHandler m) => m Text
getAppStoragePath = do
    master <- getYesod
    let dataDir = appStoragePath $ appSettings master
    case dataDir of
        Nothing -> do
            dir <- liftIO $ getAppUserDataDirectory "paseo"
            return . pack $ dir
        Just dir -> return . pack $ dir

getScanFileFullPath :: (HandlerSite m ~ App, MonadHandler m) => Text -> m Text
getScanFileFullPath file = do
    dir <- getAppStoragePath
    return . pack $ unpack dir </> unpack file
