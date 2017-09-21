{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-
    An example of embedding a custom monad into Scotty's transformer
    stack, using ReaderT to provide access to a global state.
-}
module Main where
import Data.Semigroup ((<>))
import           Control.Applicative
import Control.Monad (forM_)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, asks, lift,
                                                       runReaderT)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Prelude
import           System.Directory                     (listDirectory, doesFileExist)
import    System.FilePath.Posix
import qualified Web.Scotty.Trans                     as S
import Network.HTTP.Types (status404)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty
import           Database.SQLite.Simple


data Config = Config
  { configStorageLocation :: Text
  , configVerbose         :: Bool
  } deriving (Eq, Read, Show)

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

application :: S.ScottyT TL.Text ConfigM ()
application = do
  S.middleware logStdoutDev
  S.get "/" $ do
    let toLink e = H.a H.! A.href (H.toValue ("/" <> e)) $ H.toHtml e
    dir <- lift $ asks configStorageLocation
    files <- liftIO $ listDirectory (T.unpack dir)
    S.html . TL.pack . renderHtml $ do
      H.head $ H.title "PaSEO (pah-sey-oh)"
      H.body $ do
        H.h1 "PaSEO (pah-sey-oh)"
        H.h3 "Previous Strolls"
        H.ul $ forM_ files (H.li . toLink)
  S.get "/:metrixDb" $ do
    mdb <- S.param "metrixDb"
    dir <- lift $ asks configStorageLocation
    let dbFile = makeValid (T.unpack dir) </> mdb
        toLink (pid, p, _) = H.li . (H.a H.! A.href (H.toValue ("/" <> mdb <> "/" <> show pid))) $ H.toHtml p
    exists <- liftIO $ doesFileExist dbFile
    liftIO $ putStrLn $ "dbExists: " <> show exists
    if exists then do
      liftIO $ putStrLn "getting cntOfPages"
      cntOfPages <- liftIO $ withConnection (dbFile::String) $ \conn -> do
        r <- query_ conn "select count(*) from pages;" :: IO [Only Int]
        return $ case r of
          [x] -> fromOnly x
          [] -> -1
          (_:_:_) -> -1
      -- NOTE: join is way faster than subselect.
      pageList <- liftIO $ withConnection (dbFile::String) $ \conn -> 
        query_ conn "select pages.id, pages.page, count(page_links.id)  as num from pages join page_links on pages.id = page_links.link group by pages.id, pages.page order by num desc limit 20;" :: IO [(Int, Text, Int)]
      S.html . TL.pack . renderHtml $ do
        H.head $ H.title "PaSEO (pah-sey-oh)"
        H.body $ do
          H.h1 "PaSEO (pah-sey-oh)"
          H.h3 ("Viewing: " >> H.toHtml mdb)
          H.dl $ do
            H.dt "Number of Pages: "
            H.dd (H.toHtml cntOfPages)
            H.dt ("Top " >> H.toHtml (show (length pageList)) >> " Pages")
            H.dd $ H.ul $ forM_ pageList toLink
      else do
        S.status status404
        S.text $ "Error: " <> TL.pack dbFile
  S.get "/:metrixDb/:pageId" $ do
    pid <- S.param "pageId"
    let _ = pid :: String
    mdb <- S.param "metrixDb"
    dir <- lift $ asks configStorageLocation
    let dbFile = makeValid (T.unpack dir) </> mdb
    exists <- liftIO $ doesFileExist dbFile
    if exists then do
      pageHref <- liftIO $ withConnection (dbFile::String) $ \conn -> do
        r <- query conn "select page from pages where id = (?);" (Only pid):: IO [Only Text]
        return $ case r of
          [x] -> fromOnly x
          [] -> "unknown"
          (_:_:_) -> "unknown"
      metaTags <- liftIO $ withConnection (dbFile::String) $ \conn ->
        query conn "select repr from page_meta where page_meta.page = (?);" (Only pid) :: IO [Only Text]
      S.html . TL.pack . renderHtml $ do
        H.head $ H.title "PaSEO (pah-sey-oh)"
        H.body $ do
          H.h1 "PaSEO (pah-sey-oh)"
          H.h3 ("Viewing Database: " >> H.toHtml mdb)
          H.h3 ("Viewing Page: " >> H.toHtml pageHref)
          H.h3 "Page MetaTags"
          H.ul $ forM_ metaTags (H.li . H.toHtml . fromOnly)
      else do
        S.status status404
        S.text $ "Error: " <> TL.pack dbFile

main :: IO ()
main = S.scottyT 8080 runIO application where
  runIO :: ConfigM a -> IO a
  runIO m = runReaderT (runConfigM m) config

  config :: Config
  config = Config
    { configStorageLocation = "/tmp/paseo"
    , configVerbose = True
    }
