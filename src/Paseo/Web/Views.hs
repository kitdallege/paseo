{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Paseo.Web.Views
Description : Views for the Paseo Web Applicaiton
Copyright   : (c) Kit C. Dallege 2017
License     : BSD3 (see the file LICENSE)
Maintainer  : Kit Dallege <kitdallege@gmail.com>
Stability   : experimental
Portability : POSIX

Contains the Views which are functions using blaze-html to turn data into
Lazy.Text.
-}
module Paseo.Web.Views
  (
    index
  , errorPage
  , dbDetail
  , pageDetail
  ) where
import           Control.Monad                 (forM_)
import           Data.Semigroup                ((<>))
import qualified Data.Text                     as T
import           Data.Text.Lazy                (Text)
import           Prelude                       hiding (div, head, id)
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Text.Blaze.Html5              hiding (map)
import           Text.Blaze.Html5.Attributes   hiding (title)

baseLayout :: Html -> Html -> Html
baseLayout extraHead bodyContent = docTypeHtml $ do
  stdHead extraHead
  body $ do
    h1 $ a ! href "/" $ "PaSEO (pah-sey-oh)"
    bodyContent
  where
    stdHead :: Html -> Html
    stdHead eh = head $ do
      title "PaSEO (pah-sey-oh)"
      meta ! name "viewport" ! content "width=device-width, initial-scale=1"
      link ! rel "stylesheet"
           ! type_ "text/css"
           ! href "//cdnjs.cloudflare.com/ajax/libs/pure/1.0.0/pure-min.css"
      eh
      -- H.link  H.! A.rel "stylesheet"
      --         H.! A.type_ "text/css"
      --         H.! A.href "//fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"
      -- H.link  H.! A.rel "stylesheet"
      --         H.! A.type_ "text/css"
      --         H.! A.href "//cdn.rawgit.com/necolas/normalize.css/master/normalize.css"
      -- H.link  H.! A.rel "stylesheet"
      --         H.! A.type_ "text/css"
      --         H.! A.href "//cdn.rawgit.com/milligram/milligram/master/dist/milligram.min.css"
      -- H.link  H.! A.rel "stylesheet"
      --         H.! A.type_ "text/css"
      --         H.! A.href "https://cdnjs.cloudflare.com/ajax/libs/mini.css/2.3.4/mini-dark.min.css"



errorPage :: T.Text -> Text
errorPage errorString = renderHtml $ baseLayout mempty (h3 (toHtml errorString))

index :: [FilePath] -> Text
index files = renderHtml $ baseLayout mempty $ do
  h3 "Previous Strolls"
  ul $ forM_ files (li . toLink)
  where
    toLink e = a ! href (toValue ("/" <> e)) $ toHtml e

dbDetail :: T.Text -> Int -> [(Int, T.Text, Int)] -> Text
dbDetail mdb cntOfPages pageList = renderHtml $ baseLayout mempty $ do
  h3 $ do
    "Viewing Database: "
    a ! href (toValue ("/" <> mdb)) $ toHtml mdb
  dl $ do
    dt "Number of Pages: "
    dd (toHtml cntOfPages)
    dt ("Top " >> toHtml (show (length pageList)) >> " Pages")
    dd $ ul $ forM_ pageList toLink
  where
    toLink (pid, page, _) = li . (a ! href (toValue ("/" <> T.unpack mdb <> "/" <> show pid))) $ toHtml page

pageDetail :: T.Text -> T.Text -> [T.Text] -> Text
pageDetail mdb pageHref metaTags = renderHtml $ baseLayout mempty $ do
  h3 $ do
    "Viewing Database: "
    a ! href (toValue ("/" <> mdb)) $ toHtml mdb
  h3 ("Viewing Page: " >> toHtml pageHref)
  h3 "Page MetaTags"
  ul $ forM_ metaTags (li . toHtml)
