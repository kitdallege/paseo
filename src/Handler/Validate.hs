{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Handler.Validate where
import Import
-- import Control.Concurrent.Async
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Paseo.Spider.VnuValidation hiding (test)

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
            results <- liftIO $ vnuValidateBSL body
            case results of
                Left err -> defaultLayout [whamlet|when it comes to coding ya you totally suck! #{show err}|]
                Right vr -> defaultLayout $ do
                    let messages = toList (valResultsMessages vr) :: [Message]
                    toWidget [whamlet|
                        <div id="results">
                            <ol>
                                $forall msg <- messages
                                    <li>
                                        <p>
                                            <strong>#{show (msgType msg)}
                                            :
                                            <span>#{msgMessage msg}
                                            <p location>
                                            From line
                                            <span>
                                                $maybe firstLine <- msgFirstLine msg
                                                    #{firstLine}
                                                $nothing
                                                    #{msgLastLine msg}
                                            , column
                                            <span>
                                                $maybe firstColumn <- msgFirstColumn msg
                                                    #{firstColumn}
                                                $nothing

                                            ; to line
                                            <span> #{msgLastLine msg}
                                            , column
                                            <span> #{msgLastColumn msg}
                                            <p extract>
                                            <code>
                                                $maybe extract <- msgExtract msg
                                                    #{extract}
                                                $nothing
                                                    ""
                    |]
                    toWidget [lucius|.results{}|]
                    toWidget [julius|
                        $(document).ready(function(){
                            console.info('wubalubadubdub');
                        });
                    |]
        _ -> defaultLayout
                [whamlet|
                <p>
                    Run a url against VNU validation.
                    <form method=post action=@{ValidateR} enctype=#{enctype}>
                        ^{widget}
                        <button>Submit
                |]
