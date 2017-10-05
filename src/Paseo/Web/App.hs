{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Paseo.Web.App
Description : Scotty ActionT monad instance for the Paseo Web Applicaiton.
Copyright   : (c) Kit C. Dallege 2017
License     : BSD3 (see the file LICENSE)
Maintainer  : Kit Dallege <kitdallege@gmail.com>
Stability   : experimental
Portability : POSIX

Contains the main Scotty application.
-}
module Paseo.Web.App
  (
    application
  ) where
import Control.Concurrent.STM
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Reader                 (asks, lift)
import           Data.Semigroup                       ((<>))
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Database.SQLite.Simple               (withConnection)
import           Network.HTTP.Types                   (status404)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Paseo.Spider  as Spider
import qualified Paseo.Web.Db                         as Db
import qualified Paseo.Web.Forms as Forms
import           Paseo.Web.Types
import qualified Paseo.Web.Views                      as Views
import           System.Directory                     (doesFileExist,
                                                       listDirectory)
import           System.FilePath.Posix
import Text.Digestive (getForm, postForm)
import Text.Digestive.Types
import qualified Text.Digestive.Types as DF
import qualified Text.Digestive.Form.Encoding as DF
import qualified Text.Blaze.Html5              as H
import qualified Web.Scotty.Trans                     as S
import Control.Concurrent.Async as Async


-- TODO: ConfigM should be AppM and contain cofigurations as well as other stuff.
-- Need a `activeCrawls :: TVar (Map Text Async)` at AppM level so that a crawl
-- can be started at web-ui level, and poll'd for its completion.
-- https://stackoverflow.com/questions/44552424/forking-new-threads-in-scotty-server
-- https://stackoverflow.com/questions/29115424/how-to-open-a-separate-socket-connection-in-scotty-or-yesod
-- https://stackoverflow.com/questions/22703289/scotty-connection-pool-as-monad-reader?rq=1
-- Bust out query logic, and view logic, so the various handlers are (way shorter).
-- (maybe go to the) /app/ /lib/ style (or /app/ /src/) cuz i love me some src.
-- persistent+esqueleto [might be worth it @ this point]
-- pagination: http://www.sqlite.org/cvstrac/wiki?p=ScrollingCursor
-- (streaming ?): https://hackage.haskell.org/package/streaming
-- micro-lens: http://hackage.haskell.org/package/microlens-platform
application :: S.ScottyT TL.Text AppM ()
application = do

  S.middleware logStdoutDev

  S.defaultHandler handleEx
  -- list of previous crawls
  S.get "/" $ do
    dir  <- lift $ asks (configStorageLocation . envConfig)
    files <- S.liftAndCatchIO $ reverse <$> listDirectory (T.unpack dir)
    S.html $ Views.index files

  -- config, run, and monitor the state of, a crawl of a site.
  S.get "/scan" $ do
    view <- getForm "scan" Forms.scanForm
    let view' = fmap H.toHtml view
    S.html $ Views.scanForm (Forms.scanFormView view')
  S.post "/scan" $ do
    params' <- S.params
    (view, scan) <- postForm "scan" Forms.scanForm (paramsToEnv params')
    case scan of
      Just s -> do
        S.text . TL.pack $ show s
        sid <- liftIO $ Async.async $ Spider.scanSite (Forms.scanUrl s) (Forms.scanDepth s)
        spiders <- lift $ asks envSpiders
        liftIO $ atomically $ modifyTVar' spiders (sid:)
        liftIO $ print (Async.asyncThreadId sid)
        S.redirect . TL.pack $ "/scans/" <> show (Async.asyncThreadId sid)
      Nothing -> do
        let view' = fmap H.toHtml view
        S.html $ Views.scanForm (Forms.scanFormView view')

  S.get "/scans/:scanId" $ do
    sId <- S.param "scanId"
    spiders <- lift $ asks envSpiders
    spiders' <- liftIO $ atomically $ readTVar spiders
    let spider = filter ((== sId) . show . Async.asyncThreadId) spiders'
    status <- if (not . null) spiders'
      then do
        result <- liftIO $ Async.poll (head spider)
        return $ case result of
          Nothing -> Nothing
          Just result' ->
            case result' of
              Left e -> Just (show e)
              Right fp -> Just fp
      else return Nothing
    S.text . TL.pack $ "Spiders has status of" <> show status

  -- explore crawl results
  S.get "/:db" $ do
    mdb <- S.param "db"
    dbFile <- lookupDbFile mdb
    case dbFile of
      Left e -> doFileNotFound e
      Right file -> do
        (numPages, pageList) <- S.liftAndCatchIO $ withConnection file $ \conn ->
          (,) <$> Db.getNumberOfPages conn <*> Db.getPageList conn
        S.html $ Views.dbDetail mdb numPages pageList

  S.get "/:db/:pageId" $ do
    pid <- S.param  "pageId"
    mdb <- S.param "db"
    dbFile <- lookupDbFile mdb
    case dbFile of
      Left e -> doFileNotFound e
      Right file -> do
        (pageHref, metaTags) <- S.liftAndCatchIO $ withConnection file $ \conn ->
          (,) <$> Db.getPageHref conn pid <*> Db.getPageMetaTags conn pid
        S.html $ Views.pageDetail mdb pageHref metaTags
  where
    lookupDbFile :: Text -> S.ActionT TL.Text AppM (Either Text String)
    lookupDbFile mdb = do
      dir <- lift $ asks (configStorageLocation . envConfig)
      let dbFile = makeValid (T.unpack dir) </> T.unpack mdb
      exists <- liftIO $ doesFileExist dbFile
      if exists then return $ Right dbFile else return $ Left (T.pack (show dbFile))

    doFileNotFound err = do
      S.status status404
      S.html $ Views.errorPage ("Error: " <> err <> " File Not Found!")
    -- | Like 'param', but returns @Maybe@ instead of doing 'raise'.
    -- paramSafe :: (Monad m, Functor m, S.Parsable a) => T.Text -> S.ActionT m (Maybe a)
    paramSafe :: (S.Parsable a, Monad m, S.ScottyError e) => TL.Text -> S.ActionT e m (Maybe a)
    paramSafe q = fmap Just (S.param q)
                  `S.rescue` \_ -> return Nothing
        -- Handler for uncaught exceptions.
    -- handleEx :: Monad m => Except -> ActionT Except m ()
    -- handleEx :: TL.Text -> S.ActionT TL.Text AppM ()
    handleEx = return . S.html $ Views.errorPage "Error!"

paramsToEnv :: Monad m => [S.Param] -> DF.FormEncType -> m (DF.Env m)
paramsToEnv input _enctype = return $ \key -> return $
    map (TextInput . TL.toStrict . snd) $
    filter ((== fromPath key) . TL.toStrict . fst) input
