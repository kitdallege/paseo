{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Scans where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import System.Directory (listDirectory)

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
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
  defaultLayout $ do
      let pages = mempty :: [(Int, Text)]
          totalNumPages = length pages
      setTitle "Scan Detail | Paseo"
      $(widgetFile "scans/detail")

getScanPageDetailR :: Text -> Int -> Handler Html
getScanPageDetailR scanId pageId = do
  defaultLayout $ do
      let pageName = mempty :: Text
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
            _ -> Nothing

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
