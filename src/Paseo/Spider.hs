{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Paseo.Spider
  (
    main
  , scanSite
  , printMigrations
  ) where
import           Control.Monad               (join)
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Data.Either                 (Either (..))
import qualified Data.Map.Strict             as Map
import           Data.Semigroup              ((<>))
import           Data.Text                   (Text)
import           Data.Time.Clock.POSIX       (POSIXTime, getPOSIXTime)
import           GHC.Generics                (Generic)
import           System.IO                   (BufferMode (..), Handle,
                                              IOMode (..), hSetBuffering,
                                              stdout, withFile)
-- html handling
import           Network.URI                 (URI (..))
import qualified Network.URI                 as URI
import           Text.HTML.TagSoup           (parseTags)
import           Text.HTML.TagSoup.Selection as TS
import           Text.HTML.TagSoup.Tree      (TagTree, renderTree)
-- Export as Json
import           Data.Aeson                  (ToJSON, encode)
--import           Database.SQLite.Simple

import           Control.Monad.Logger
import           Import
--import Control.Monad.Trans.Resource
import           Database.Persist.Sql        (SqlBackend, runSqlConn,
                                              transactionSave)
import           Database.Persist.Sqlite     (mockMigration, runMigration,
                                              withSqliteConn)
-- Library in the works
import           Charlotte                   as C
import qualified Charlotte.Request           as Request
import qualified Charlotte.Response          as CResponse

{-
This program crawls a website and records 'internal' links on a page.
Pages can then be rank'd via the # of other pages linking to them 'PageRank'.
-}
type Depth = Int
type Ref = String
data PageType = ScrapedPage Depth Ref
  deriving (Show, Eq, Ord, Generic)

data MetaTag = MetaTag {
    metaTagRepr  :: !String
  , metaTagAttrs :: !(Map.Map String String)
} deriving (Show, Generic)

data PageData = PageData {
    pagePath    :: !String
  , pageLinks   :: ![String]
  , pageDepth   :: !Int
  , pageRef     :: !String
  , pageMeta    :: ![MetaTag]
  , pageMetrics :: ![Metric]
} deriving (Show, Generic)

data Metric = Metric
  { metricType :: String
  , metricData :: Map.Map String String
  } deriving (Show, Generic)

instance ToJSON MetaTag
instance ToJSON PageData
instance ToJSON Metric

type ParseResult = Result PageType PageData

siteMapSpider :: SpiderDefinition PageType PageData
siteMapSpider = SpiderDefinition {
    _name = "site-map-generator"
  , _startUrl = (ScrapedPage 1 "START", "http://local.lasvegassun.com/")
  , _extract = parse 2
  , _transform = Just pipeline
  , _load = Nothing
}

linkSelector, metaSelector :: Selector
Right linkSelector = parseSelector "a"
Right metaSelector = parseSelector "meta"

-- crawlHost :: String
-- crawlHost = "local.lasvegassun.com"

crawlHostURI :: URI
Just crawlHostURI = URI.parseURI "http://local.lasvegassun.com"

-- maxDepth :: Int
-- maxDepth = 2

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  timestamp <- show . (round :: POSIXTime -> Integer) <$> getPOSIXTime :: IO String
  print timestamp
  -- mainDb timestamp
  -- mainJson timestamp

printMigrations :: IO ()
printMigrations = mockMigration migrateAllScan

scanSite :: Text -> Int -> IO FilePath
scanSite url depth = do
  timestamp <- showIntegral . (round :: POSIXTime -> Integer) <$> getPOSIXTime :: IO String
  let dbFile = "/tmp/paseo/" <> pack timestamp <> ".db"
  --runNoLoggingT/runStdoutLoggingT
  runResourceT . runNoLoggingT . withSqliteConn dbFile $ \conn-> do
    putStrLn "Performing Migrations"
    _ <- runSqlConn (runMigration migrateAllScan) conn
    putStrLn "Migrations Complete."
    _ <- liftIO $ runSpider siteMapSpider
          {
            _startUrl = (ScrapedPage 1 "START", unpack url)
          , _extract = parse depth
          , _load = Just (loadSqliteDb conn)
          }
    return $ "/tmp/paseo/" <> pack timestamp <> ".db"

mainJson :: String -> IO ()
mainJson filenamePart =
  withFile ("/tmp/paseo/" <> filenamePart <>".jl") WriteMode $ \hdl -> do
    hSetBuffering hdl LineBuffering
    runSpider siteMapSpider {_load = Just (loadJsonLinesFile hdl)}

-- mainDb :: String -> IO ()
-- mainDb filenamePart =
--   withConnection ("/tmp/paseo/" <> filenamePart <>".db") $ \conn -> do
--     mapM_ (execute_ conn) dropTableStmts
--     mapM_ (execute_ conn) createTableStmts
--     runSpider siteMapSpider {_load = Just (loadSqliteDb conn)}

parse :: Int -> PageType -> C.Response -> [ParseResult]
parse maxDepth (ScrapedPage depth ref) resp = let
  nextDepth = succ depth
  tagTree = parseTagTree resp
  links = parseLinks tagTree
  linkPaths = map URI.uriPath links
  metaTags = parseMetaTags tagTree
  responsePath = URI.uriPath $ CResponse.uri resp
  reqs = catMaybes $ Request.mkRequest <$> map show links
  results = map (\r->Request (ScrapedPage nextDepth responsePath, r)) reqs
  items = [Item $ PageData responsePath linkPaths depth ref metaTags []]
  in if depth < maxDepth then results <> items else items

parseTagTree :: C.Response -> [TagTree String]
parseTagTree resp = let
  r = (BSL.unpack $ CResponse.body resp) :: String
  tags = parseTags r
  in filter TS.isTagBranch $ TS.tagTree' tags

parseLinks :: [TagTree String] -> [URI]
parseLinks tt = let
  maybeHostName l = URI.uriRegName <$> URI.uriAuthority l
  setHostName l = l {
      URI.uriAuthority=URI.uriAuthority crawlHostURI
    , URI.uriScheme = URI.uriScheme crawlHostURI}
  getHref = (TS.findTagBranchAttr "href" . TS.content)
  links = mapMaybe getHref $ join $ TS.select linkSelector <$> tt
  relLinks = filter ((==) (maybeHostName crawlHostURI) . maybeHostName) $
    map setHostName $
    mapMaybe URI.parseRelativeReference $
    filter URI.isRelativeReference links
  absLinks = filter ((==) (maybeHostName crawlHostURI) . maybeHostName) $
    mapMaybe URI.parseAbsoluteURI $
    filter URI.isAbsoluteURI links
  in (absLinks <> relLinks)

parseMetaTags :: [TagTree String] -> [MetaTag]
parseMetaTags tt = let
  metaTags = join $ TS.select metaSelector <$> tt
  getAttrs = Map.fromList . TS.tagBranchAttrs . TS.content
  getRepr = renderTree . return . TS.content
  toMetaTag t = MetaTag (getRepr t) (getAttrs t)
  in map toMetaTag metaTags

pipeline :: PageData -> IO PageData
pipeline page = do
  -- results <- vnuValidate (pageResponse page)
  print page
  return page
----------------------------------
-- Load(ers) and suporting code.
----------------------------------
-- JsonLines
loadJsonLinesFile :: ToJSON a => Handle -> a -> IO ()
loadJsonLinesFile fh item = BSL.hPutStrLn fh (encode item)


loadSqliteDb :: SqlBackend -> PageData -> IO ()
loadSqliteDb conn PageData{..} = do
  --addPage pagePath
  pid <- addPage pagePath
  print $ "pageId: " <> show pid
  mapM_ (addPageLink pid pageDepth pageRef) pageLinks
  mapM_ (addPageMeta pid) pageMeta
  doSql transactionSave
  print $ "Inserted (" <> show (length pageLinks) <> ") page_links!"
  print $ "Inserted (" <> show (length pageMeta) <> ") page_meta!"
  return ()
    where
      doSql :: ReaderT SqlBackend IO a -> IO a
      doSql = flip runSqlConn conn
      addPage :: String -> IO (Key Page)
      addPage p = do
        r <- doSql $ upsert (Page (pack p)) []
        return $ entityKey r
      addPageLink pid depth ref link  = do
        lid <- addPage link
        rid <- addPage ref
        _ <- doSql $ insert (PageLink pid lid depth rid)
        return ()
      addPageMeta pid pm = do
        pmid <- doSql $ insert (PageMeta pid (pack (metaTagRepr pm)))
        mapM_ (uncurry (addMetaAttr pmid)) $ Map.toList $ metaTagAttrs pm
        return ()
      addMetaAttr mid k v = do
        _ <- doSql $ insert (MetaAttr mid (pack k) (pack v))
        return ()
