{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Paseo.WFind
    ( wfind
    , SearchPattern(..)
    , MatchInfo(..)
    , Match(..)
    ) where

import           Import
-- html handling
--import           Control.Concurrent.STM
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy.Char8  as BSL
import           Network.URI                 (URI (..))
import qualified Network.URI                 as URI
import           Text.HTML.TagSoup           (parseTags)
import           Text.HTML.TagSoup.Selection as TS
import           Text.HTML.TagSoup.Tree      (TagTree)

import           System.IO                   (BufferMode (..), hSetBuffering,
                                              stdout)

-- Library in the works
import           Charlotte                   as C
import qualified Charlotte.Request           as Request
import qualified Charlotte.Response          as CResponse

linkSelector :: Selector
Right linkSelector = parseSelector "a"

data SearchPattern = SearchPattern
    { spPattern     :: Text
    , spInvertMatch :: Bool
    } deriving (Show)

data PageType = ScrapedPage Int Text
  deriving (Show, Eq, Ord, Generic)

data Match = Match
    { matchPath    :: Text
    , matchPattern :: SearchPattern
    , matchDepth   :: Int
    , matchRef     :: Text
    , matchInfo    :: [MatchInfo]
    } deriving (Show)

data MatchInfo = MatchInfo
    { matchInfoLine   :: Int
    , matchInfoColumn :: Int
    } deriving (Show)

siteSearchSpider :: SpiderDefinition PageType Match
siteSearchSpider = let
    Just crawlHostURI = URI.parseURI "http://local.lasvegassun.com"
    in SpiderDefinition
    { _name = "wfind-spider"
    , _startUrl = (ScrapedPage 1 "START", "http://local.lasvegassun.com")
    , _extract = parse 0 [] crawlHostURI
    , _transform = Nothing
    , _load = Nothing
    }

wfind :: Text -> Int -> [SearchPattern] -> TChan (Maybe Text) -> IO ()
wfind uri depth patterns chan = do
    hSetBuffering stdout LineBuffering
    let suri = unpack uri
        Just crawlHostURI = URI.parseURI suri
    threadDelay 100000
    _ <- atomically $ writeTChan chan $ Just "Starting Search..."
    _ <- runSpider siteSearchSpider
        { _startUrl = (ScrapedPage 1 "START", suri)
        , _extract = parse depth patterns crawlHostURI
        , _load = Just (\x -> do
                atomically $ writeTChan chan $ Just (pack $ show x)
                return ()
                )
        }
    _ <- atomically $ writeTChan chan Nothing
    return ()

parse :: Int -> [SearchPattern] -> URI -> PageType -> C.Response -> [Result PageType Match]
parse maxDepth patterns crawlHostURI (ScrapedPage depth ref) resp = let
    nextDepth = succ depth
    tagTree = parseTagTree resp
    links = parseLinks crawlHostURI tagTree
    responsePath = pack $ URI.uriPath $ CResponse.uri resp
    reqs = catMaybes $ Request.mkRequest <$> map show links
    results = map (\r->Request (ScrapedPage nextDepth responsePath, r)) reqs
    items = map Item $ mapMaybe (performPatternMatch resp responsePath depth ref) patterns
    in if depth < maxDepth then results <> items else items

performPatternMatch :: CResponse.Response -> Text -> Int -> Text -> SearchPattern -> Maybe Match
performPatternMatch resp curPath depth ref pat = do
    let body = CResponse.body resp
        pat' = spPattern pat
        pat'' = encodeUtf8 pat' :: ByteString
        body' = BSL.toStrict body
        hasMatch = pat'' `isInfixOf` body'
        invertMatch = spInvertMatch pat
    if hasMatch && not invertMatch
        then Just $ Match curPath pat depth ref (mkMatchInfo pat'' body')
        else if invertMatch && not hasMatch then
            Just $ Match curPath pat depth ref []
            else Nothing

mkMatchInfo :: ByteString -> ByteString -> [MatchInfo]
mkMatchInfo pat body = let
    linesWithIndexes = [(i, p) | (p, i) <- zip (BS8.lines body) [1..], pat `isInfixOf` p]
    colIndex xs = length . fst $ BS8.breakSubstring pat xs
    infoItems = [(i, colIndex txt) | (i, txt) <- linesWithIndexes]
    in [MatchInfo l c | (l, c) <- infoItems]

parseTagTree :: C.Response -> [TagTree String]
parseTagTree resp = let
    r = BSL.unpack $ CResponse.body resp
    tags = parseTags r
    in filter TS.isTagBranch $ TS.tagTree' tags


parseLinks :: URI -> [TagTree String] -> [URI]
parseLinks crawlHostURI tt = let
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
