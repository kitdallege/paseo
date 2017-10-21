{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.WFind where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Yesod.Form.MassInput
import Text.Julius (RawJS (..))

data FindPattern = FindPattern
    { fpPattern :: Text
    , fpInvertMatch :: Bool
    } deriving (Show)


data WFindForm = WFindForm
    { wfindFormUrl      :: Text
    , wfindDepth        :: Int
    , wfindPattern :: [FindPattern]
    } deriving (Show)


fpForm :: AForm Handler [FindPattern]
fpForm = inputList "Find Patterns" massDivs mkPT (Just [FindPattern "" False])
  where
      mkPT :: Maybe FindPattern -> AForm (HandlerT App IO) FindPattern
      mkPT x = FindPattern
        <$> areq textField "Pattern" (fpPattern <$> x)
        <*> areq checkBoxField "Invert" (fpInvertMatch <$> x)

wfindForm :: Form WFindForm
wfindForm = renderBootstrap3 BootstrapBasicForm $ WFindForm
    <$> areq urlField urlSettings Nothing
    <*> areq intField (fsettings "Max Scan Depth: ") (Just 3)
    <*> fpForm
  where
    fsettings lbl = FieldSettings
        { fsLabel = lbl
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs = [ ("class", "form-control")]
        }
    urlSettings = FieldSettings
        { fsLabel = "URL To Scan: "
        , fsTooltip = Nothing
        , fsId = Nothing
        , fsName = Nothing
        , fsAttrs =
            [ ("class", "form-control")
            , ("placeholder", "http://local.lasvegassun.com")
            ]
        }


getWFindR :: Handler Html
getWFindR = do
    (widget, enctype) <- generateFormPost wfindForm
    defaultLayout
        [whamlet|
        <p>
            Scan a site searching for a given string.
            <form method=post action=@{WFindR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]

postWFindR :: Handler Html
postWFindR = do
    ((result, widget), enctype) <- runFormPost wfindForm
    patterns <- lookupPostParams "pattern"
    case result of
        FormSuccess res -> do
            defaultLayout [whamlet|<p>#{show res}|]
        _ -> defaultLayout
            [whamlet|
                <form method=post action=@{WFindR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]
