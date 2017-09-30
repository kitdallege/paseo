{-# LANGUAGE OverloadedStrings #-}
module Paseo.Web.Views
  (
    stdHead
  , index
  , dbDetail
  , pageDetail
  ) where
import Prelude hiding (head, id, div)
import Data.Semigroup ((<>))
import Control.Monad (forM_)
import Data.Text.Lazy (Text)
import qualified Data.Text                            as T
import Text.Blaze.Html5 hiding (map)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Html.Renderer.Text (renderHtml)

stdHead :: Html -> Html
stdHead extraHead = head $ do
  title "PaSEO (pah-sey-oh)"
  meta ! name "viewport" ! content "width=device-width, initial-scale=1"
  link ! rel "stylesheet"
       ! type_ "text/css"
       ! href "//cdnjs.cloudflare.com/ajax/libs/pure/1.0.0/pure-min.css"
  extraHead
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


baseLayout :: Html -> Html -> Html
baseLayout extraHead bodyContent = docTypeHtml $ do
  stdHead extraHead
  body $ do
    h1 $ a ! href "/" $ "PaSEO (pah-sey-oh)"
    bodyContent

index :: [FilePath] -> Text
index files = renderHtml $ baseLayout mempty $ do
  h3 "Previous Strolls"
  ul $ forM_ files (li . toLink)
  where
    toLink e = a ! href (toValue ("/" <> e)) $ toHtml e

dbDetail :: String -> Int -> [(Int, T.Text, Int)] -> Text
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
    toLink (pid, page, _) = li . (a ! href (toValue ("/" <> mdb <> "/" <> show pid))) $ toHtml page

pageDetail :: String -> T.Text -> [T.Text] -> Text
pageDetail mdb pageHref metaTags = renderHtml $ baseLayout mempty $ do
  h3 $ do
    "Viewing Database: "
    a ! href (toValue ("/" <> mdb)) $ toHtml mdb
  h3 ("Viewing Page: " >> toHtml pageHref)
  h3 "Page MetaTags"
  ul $ forM_ metaTags (li . toHtml)
