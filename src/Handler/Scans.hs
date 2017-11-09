{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Scans where

import           Database.Persist.Sql    (fromSqlKey, toSqlKey)
import           Import
import           System.Directory        (listDirectory)
-- import           Text.Julius             (RawJS (..))
import           Yesod.Form.Bootstrap3   (BootstrapFormLayout (..),
                                          renderBootstrap3)
import qualified Data.IntMap            as IntMap
import           Data.Time.Clock.POSIX       (POSIXTime, getPOSIXTime)
import Queries (runExtDB, runExtDB')
import qualified Paseo.Spider as Spider
import qualified Queries as Q
import Paseo.Common.Handler


getScanListR :: Handler Html
getScanListR = do
    path <- getAppStoragePath
    defaultLayout $ do
        files <- liftIO $ listDirectory (unpack path)
        setTitle "Scan List | Paseo"
        $(widgetFile "scans/list")

getScanDetailR :: Text -> Handler Html
getScanDetailR scanId = do
    pages <- runExtDB' Q.getScanPages =<< getScanFileFullPath scanId
    defaultLayout $ do
        let totalNumPages = length pages
        setTitle "Scan Detail | Paseo"
        $(widgetFile "scans/detail")

getScanPageDetailR :: Text -> Int -> Handler Html
getScanPageDetailR scanId pageId = do
    path <- getAppStoragePath
    let file = path <> scanId
    mPage <- runExtDB file $ get $ toSqlKey (fromIntegral pageId)
    metaTags <- runExtDB file $ selectList [PageMetaPage ==. toSqlKey (fromIntegral pageId)] []
    case mPage of
        Nothing -> notFound
        Just page -> defaultLayout $ do
            setTitle "Scan Page Detail | Paseo"
            $(widgetFile "scans/page-detail")

getScanPageMetaDetailR :: Text -> Int -> Int -> Handler Html
getScanPageMetaDetailR scanId pageId metaId = defaultLayout $ do
    [whamlet|<h2>Here's another |]

getScanNewR ::  Handler Html
getScanNewR = do
    (widget, enctype) <- generateFormPost scanForm
    defaultLayout
        [whamlet|
        <p>
            Scan a site to examin its SEO status.
            <form method=post action=@{ScansR(ScanNewR)} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]


-- Define our data that will be used for creating the form.
data ScanForm = ScanForm
    { scanFormUrl   :: Text
    , scanFormDepth :: Int
    } deriving (Show)

postScanNewR :: Handler Html
postScanNewR = do
    ((result, widget), enctype) <- runFormPost scanForm
    case result of
        FormSuccess res -> do
            App {..} <- getYesod
            timestamp <- liftIO $ showIntegral . (round :: POSIXTime -> Integer) <$> getPOSIXTime
            filepath <- getScanFileFullPath $ pack timestamp
            spider <- liftIO $ async $
                Spider.scanSite
                    (filepath <> ".db")
                    (scanFormUrl res)
                    (scanFormDepth res)
            scanId <- liftIO $ do
                chan <- newTChanIO
                scanId <- atomically $ do
                    jobId <- readTVar appNextScan
                    modifyTVar' appNextScan succ
                    appScans' <- readTVar appScans
                    writeTVar appScans $ IntMap.insert jobId chan appScans'
                    return jobId
                return scanId
            defaultLayout [whamlet|<p>#{show res} JobId: #{show scanId}|]
        _ -> defaultLayout
            [whamlet|
                <form method=post action=@{ScansR(ScanNewR)} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

scanForm :: Form ScanForm
scanForm = renderBootstrap3 BootstrapBasicForm $ ScanForm
    <$> areq textField urlSettings Nothing
    <*> areq intField depthSettings (Just 3)
    -- Add attributes like the placeholder and CSS classes.
  where
    urlSettings = FieldSettings
        { fsLabel = "URL To Scan: "
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs =
            [ ("class", "form-control")
            , ("placeholder", "http://local.lasvegassun.com")
            ]
        }
    depthSettings = FieldSettings
        { fsLabel = "Max Scan Depth: "
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs =[("class", "form-control")]
        }
