{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards   #-}
module Handler.WFind where

import Import
import qualified Data.IntMap            as IntMap
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Yesod.Form.MassInput
import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)
import Network.Wai.EventSource (ServerEvent (..), eventSourceAppIO)
import Text.Julius (RawJS (..))
import qualified Paseo.WFind as WFind

data WFindForm = WFindForm
    { wfindFormUrl      :: Text
    , wfindDepth        :: Int
    , wfindPattern :: [WFind.SearchPattern]
    } deriving (Show)

fpForm :: AForm Handler [WFind.SearchPattern]
fpForm = inputList "Find Patterns" massDivs mkPT defSearchPatern
  where
      mkPT :: Maybe WFind.SearchPattern -> AForm Handler WFind.SearchPattern
      mkPT x = WFind.SearchPattern
        <$> areq textField "Pattern" (WFind.spPattern <$> x)
        <*> areq checkBoxField "Invert" (WFind.spInvertMatch <$> x)
      defSearchPatern = Just [WFind.SearchPattern "" False]

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
    case result of
        FormSuccess res -> do
            App {..} <- getYesod
            wfindId <- liftIO $ do
                chan <- newTChanIO
                _ <- async $ do
                    WFind.wfind
                        (wfindFormUrl res)
                        (wfindDepth res)
                        (wfindPattern res)
                        chan
                atomically $ do
                    jobId <- readTVar appNextScan
                    modifyTVar' appNextScan succ
                    appScans' <- readTVar appScans
                    writeTVar appScans $ IntMap.insert jobId chan appScans'
                    return jobId
            -- redirect $ WFindResultsR wfindId
            output <- newIdent
            defaultLayout $ do
                toWidget [whamlet|<div ##{output}>|]
                -- Just some CSS
                toWidget [lucius|
                    ##{output} {
                        width: 100%;
                        height: 500px;
                        border: 1px solid #999;
                        overflow: auto;
                    }
                |]
                -- And now that Javascript
                toWidgetBody [julius|
                    // Set up the receiving end
                    var output = document.getElementById(#{toJSON output});
                    var src = new EventSource("@{WFindResultsR wfindId}");
                    src.onmessage = function(msg) {
                        if(!msg) {
                            debugger
                            src.close();
                        }
                        // This function will be called for each new message.
                        var p = document.createElement("p");
                        p.appendChild(document.createTextNode(msg.data));
                        output.appendChild(p);

                        // And now scroll down within the output div so the most recent message
                        // is displayed.
                        output.scrollTop = output.scrollHeight;
                    };
                    src.onerror = function(event){
                        if (event.eventPhase == EventSource.CLOSED) {
                            src.close();
                        }
                    };
                |]
        _ -> defaultLayout
            [whamlet|
                <form method=post action=@{WFindR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]

getWFindResultsR :: IntMap.Key -> Handler ()
getWFindResultsR jobId = do
    App {..} <- getYesod
    mchan <- liftIO $ atomically $ do
        m <- readTVar appScans
        case IntMap.lookup jobId m of
            Nothing -> return Nothing
            Just chan -> Just <$> dupTChan chan
    case mchan of
        Nothing -> notFound
        Just chan -> sendWaiApplication $ eventSourceAppIO (loop chan)
  where
    loop :: TChan (Maybe Text) -> IO ServerEvent
    loop chan = do
        mtext <- atomically $ readTChan chan
        case mtext of
            Nothing -> return CloseEvent
            Just text -> return ServerEvent
                            { eventName = Nothing
                            , eventId = Nothing
                            , eventData = [fromText text]
                            }
