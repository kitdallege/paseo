{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Paseo.Web.Db
Description : Queries for the Paseo Web Applicaiton.
Copyright   : (c) Kit C. Dallege 2017
License     : BSD3 (see the file LICENSE)
Maintainer  : Kit Dallege <kitdallege@gmail.com>
Stability   : experimental
Portability : POSIX

Contains queries used in the web application.
-}
module Paseo.Web.Db
  (
    getNumberOfPages
  , getPageList
  , getPageHref
  , getPageMetaTags
  ) where
import           Data.Text              (Text)
import           Database.SQLite.Simple

getNumberOfPages :: Connection -> IO Int
getNumberOfPages conn = do
  [r] <- query_ conn "select count(*) from pages;" :: IO [[Int]]
  return $ case r of
    [x]   -> x
    []    -> 0
    (_:_) -> 0

-- NOTE: join is way faster than sub-select.
getPageList :: Connection -> IO [(Int, Text, Int)]
getPageList conn = query_ conn "select pages.id, pages.page, count(page_links.id)  \
      \as num from pages join page_links on pages.id = page_links.link \
      \group by pages.id, pages.page order by num desc limit 50;" :: IO [(Int, Text, Int)]

getPageHref :: Connection -> Text -> IO Text
getPageHref conn pid = do
  --TODO: This pattern match can fail.
  [r] <- query conn "select page from pages where id = (?);" (Only pid):: IO [[Text]]
  return $ case r of
    [x]   -> x
    []    -> mempty
    (_:_) -> mempty

getPageMetaTags :: Connection -> Text -> IO [Text]
getPageMetaTags conn pid = do
  r <- query conn "select repr from page_meta where page_meta.page = (?);" (Only pid) :: IO [Only Text]
  return $ map fromOnly r
