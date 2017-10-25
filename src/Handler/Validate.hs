{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.Validate where
import           Data.Aeson
import qualified Data.ByteString.Lazy            as BSL
import Import
-- import Control.Concurrent.Async
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import qualified Paseo.Spider.VnuValidation as Vnu

newtype ValidationForm = ValidationForm{validationUrl :: Text} deriving (Show)

validationForm :: Form ValidationForm
validationForm = renderBootstrap3 BootstrapBasicForm $ ValidationForm <$>
    areq urlField urlSettings Nothing
  where
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

getValidateR :: Handler Html
getValidateR = do
    (widget, enctype) <- generateFormPost validationForm
    defaultLayout
        [whamlet|
        <p>
            Run a url against VNU validation.
            <form method=post action=@{ValidateR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]

-- toJSONText :: Value -> Html
-- toJSONText v = preEscapedToHtml $ decodeUtf8 $ lazyToStrictBS $ encode $ v

postValidateR :: Handler Html
postValidateR = do
    ((result, widget), enctype) <- runFormPost validationForm
    case result of
        FormSuccess res -> do
            req <- parseRequest $ unpack (validationUrl res)
            let req' = req {requestHeaders = [("User-Agent", "Paseo(https://github.com/kitdallege/paseo)")]}
            resp <- httpLbs req'
            let body = responseBody resp
            -- print (requestHeaders req)
            results <- liftIO $ Vnu.vnuValidateBSL body
            case results of
                Left err -> defaultLayout [whamlet|when it comes to coding ya you totally suck! #{show err}|]
                Right vr -> do
                    --liftIO $ BSL.putStr $ encode vr
                    defaultLayout [whamlet|when it comes to coding ya you totally rock! <pre>#{show vr} </pre>|]
        _ -> defaultLayout
                [whamlet|
                <p>
                    Run a url against VNU validation.
                    <form method=post action=@{ValidateR} enctype=#{enctype}>
                        ^{widget}
                        <button>Submit
                |]
