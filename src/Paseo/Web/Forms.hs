{-# LANGUAGE OverloadedStrings #-}
module Paseo.Web.Forms
  (
    scanForm
  , scanFormView
  , Scan(..)
  ) where
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import qualified Data.Text
import qualified Text.Blaze.Html5              as H
import qualified Text.Blaze.Html5.Attributes   as HA
import Text.Blaze.Html5 ((!))

import Text.Digestive
import Text.Digestive.Blaze.Html5
import Text.Digestive.Util


data Scan = Scan
  {
    scanUrl :: Text
  , scanDepth :: Int
  } deriving (Show)

scanForm :: (Monad m) => Form Text m Scan
scanForm = Scan
  <$> "url" .:  nonEmptyText (Just "http://local.lasvegassun.com")
  <*> "depth" .: stringRead "Can't parse Depth" (Just 1)
  where
    nonEmptyText def =
      check "Url cannot be empty" (not . Data.Text.null) $ text def

scanFormView :: View H.Html -> H.Html
scanFormView view = form view "/scan" $ do
  childErrorList "" view
  label "url" view "URL: "
  inputText "url" view ! HA.size "100"
  H.hr
  label "depth" view "Depth: "
  inputText "depth" view
  H.hr
  inputSubmit "Start Scan"
