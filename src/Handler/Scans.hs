
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Handler.Scans where
-- import           Control.Monad.Logger    (NoLoggingT)
import           Database.Persist.Sql    (fromSqlKey, toSqlKey)
-- import           Database.Persist.Sqlite (runSqlite)
-- import qualified Database.Esqueleto as E
-- import Database.Esqueleto ((^.), (==.))
import           Import
import           System.Directory        (listDirectory)
import           Text.Julius             (RawJS (..))
import           Yesod.Form.Bootstrap3   (BootstrapFormLayout (..),
                                          renderBootstrap3)
import Queries (runExtDB)
import qualified Queries as Q

-- Some helpers (probably should live in Model/Application/Fondation etc.)
-- some place it can make in into Import
-- runExtDB :: (MonadIO m, MonadBaseControl IO m) =>
--   Text ->
--   ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
-- runExtDB file = runSqlite file . asSqlBackendReader
--   where
--     asSqlBackendReader :: ReaderT SqlBackend m a -> ReaderT SqlBackend m a
--     asSqlBackendReader = id
--
-- isExtDBOK :: (MonadBaseControl IO m, MonadIO m) => Text -> m Bool
-- isExtDBOK file = runExtDB file $ do
--   mig <- parseMigration migrateAllScan
--   return $ case mig of
--     Left _    -> False
--     Right sql -> null sql
--
-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo        :: FileInfo
    , fileDescription :: Text
    }

getScanListR :: Handler Html
getScanListR = do
  defaultLayout $ do
      files <- liftIO $ listDirectory "/tmp/paseo"
      setTitle "Scan List | Paseo"
      $(widgetFile "scans/list")

getScanDetailR :: Text -> Handler Html
getScanDetailR scanId = do
  pages <- runExtDB ("/tmp/paseo/" <> scanId) Q.getScanPages
  defaultLayout $ do
    let totalNumPages = length pages
    setTitle "Scan Detail | Paseo"
    $(widgetFile "scans/detail")

getScanPageDetailR :: Text -> Int -> Handler Html
getScanPageDetailR scanId pageId = do
  let file = "/tmp/paseo/" <> scanId
  mPage <- runExtDB file $ get $ toSqlKey (fromIntegral pageId)
  metaTags <- runExtDB file $ selectList [PageMetaPage ==. toSqlKey (fromIntegral pageId)] []
  case mPage of
    Nothing -> notFound
    Just page -> defaultLayout $ do
        setTitle "Scan Page Detail | Paseo"
        $(widgetFile "scans/page-detail")

getScanNewR ::  Handler Html
getScanNewR = do
  let submission = Nothing :: Maybe FileForm
      handlerName = "getScanNewR" :: Text
      files = mempty :: [FilePath]
  (formWidget, formEnctype) <- generateFormPost sampleForm
  defaultLayout $ do
      let (commentFormId, commentTextareaId, commentListId) = commentIds
      aDomId <- newIdent
      setTitle "Welcome To Paseo!"
      $(widgetFile "homepage")

postScanNewR :: Handler Html
postScanNewR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let submission = case result of
            FormSuccess res -> Just res
            _               -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
            files = mempty :: [FilePath]
            handlerName = "postScanNewR" :: Text
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
