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
import Text.Blaze.Html.Renderer.Text
import           Database.SQLite.Simple


data Config = Config
  { configStorageLocation :: Text
  , configVerbose         :: Bool
  } deriving (Eq, Read, Show)

newtype ConfigM a = ConfigM
  { runConfigM :: ReaderT Config IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Config)

stdHead :: H.Html
stdHead = H.head $ do
  H.title "PaSEO (pah-sey-oh)"
  H.meta H.! A.name "viewport" H.! A.content "width=device-width, initial-scale=1"
  -- H.link  H.! A.rel "stylesheet"
  --         H.! A.type_ "text/css"
  --         H.! A.href "//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
  -- H.link  H.! A.rel "stylesheet"
  --         H.! A.type_ "text/css"
  --         H.! A.href "//cdn.rawgit.com/necolas/normalize.css/master/normalize.css"
  -- H.link  H.! A.rel "stylesheet"
  --         H.! A.type_ "text/css"
  --         H.! A.href "//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css"
  H.link  H.! A.rel "stylesheet"
          H.! A.type_ "text/css"
          H.! A.href "//cdnjs.cloudflare.com/ajax/libs/pure/1.0.0/pure-min.css"
  -- H.link  H.! A.rel "stylesheet"
  --         H.! A.type_ "text/css"
  --         H.! A.href "https://cdnjs.cloudflare.com/ajax/libs/mini.css/2.3.4/mini-dark.min.css"

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
application :: S.ScottyT TL.Text ConfigM ()
application = do
  S.middleware logStdoutDev
  S.get "/" $ do
    let toLink e = H.a H.! A.href (H.toValue ("/" <> e)) $ H.toHtml e
    dir <- lift $ asks configStorageLocation
    files <- liftIO $ listDirectory (T.unpack dir)
    S.html . renderHtml $ H.docTypeHtml $ do
      stdHead
      H.body $ do
        H.h1 $ H.a H.! A.href "/" $ "PaSEO (pah-sey-oh)"
        H.h3 "Previous Strolls"
        H.ul $ forM_ files (H.li . toLink)
  S.get "/:metrixDb" $ do
    mdb <- S.param "metrixDb"
    dir <- lift $ asks configStorageLocation
    let dbFile = makeValid (T.unpack dir) </> mdb
        toLink (pid, p, _) = H.li . (H.a H.! A.href (H.toValue ("/" <> mdb <> "/" <> show pid))) $ H.toHtml p
    exists <- liftIO $ doesFileExist dbFile
    if exists then do
      -- TODO: Combine multiple withConnections into a single block
      -- which gets data and either returns it, or just renders the
      -- html out with it and returns that..
      cntOfPages <- liftIO $ withConnection (dbFile::String) $ \conn -> do
        r <- query_ conn "select count(*) from pages;" :: IO [Only Int]
        return $ case r of
          [x] -> fromOnly x
          [] -> -1
          (_:_:_) -> -1
      -- NOTE: join is way faster than subselect.
      pageList <- liftIO $ withConnection (dbFile::String) $ \conn ->
        query_ conn "select pages.id, pages.page, count(page_links.id)  as num from pages join page_links on pages.id = page_links.link group by pages.id, pages.page order by num desc limit 50;" :: IO [(Int, Text, Int)]
      S.html . renderHtml $ H.docTypeHtml $ do
        stdHead
        H.body $ do
          H.h1 $ H.a H.! A.href "/" $ "PaSEO (pah-sey-oh)"
          H.h3 $ do
            "Viewing Database: "
            H.a H.! A.href (H.toValue ("/" <> mdb)) $ H.toHtml mdb
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
      S.html . renderHtml $ H.docTypeHtml $ do
          stdHead
          H.body $ do
            H.h1 $ H.a H.! A.href "/" $ "PaSEO (pah-sey-oh)"
            H.h3 $ do
              "Viewing Database: "
              H.a H.! A.href (H.toValue ("/" <> mdb)) $ H.toHtml mdb
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
