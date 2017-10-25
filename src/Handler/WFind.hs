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
                toWidget [whamlet|
                    <table class="table table-striped table-bordered table-hover table-condensed">
                        <thead>
                            <th>Path
                            <th>Pattern
                            <th>Match Info
                        <tfoot>
                            <tr>
                                <td colspan=2>Total Matches
                                <td id="total-matches">
                        <tbody>
                            <tr>
                                <td colspan=3>
                                    <div class="scroller">
                                        <table ##{output} class="table table-striped table-bordered table-hover table-condensed">
                                            <tbody>

                |]
                -- Just some CSS
                toWidget [lucius|
                    .scroller {overflow-y:scroll; height: 750px;}
                    ##{output} {width: 100%; border: 1px solid #999;}
                    table.table > thead:first-child > tr:first-child > th:first-of-type {min-width: 250px; width: 59%;}
                    div.scroller table.table tr td:nth-of-type(2) {min-width: 107px;}
                    table.table tr td:first-of-type{width: 60%;}
                    table.table tfoot td {font-size: 18px; font-weight: bold;}
                |]
                -- And now that Javascript
                toWidgetBody [julius|
                    // Set up the receiving end
                    var output = $("##{rawJS output} tbody");
                        container = $("##{rawJS output}").parent(),
                        totalMatches = 0,
                        $totalMatches = $("#total-matches"),
                        src = new EventSource("@{WFindResultsR wfindId}");
                    src.onmessage = function(msg) {
                        var data = JSON.parse(msg.data);
                        var $tr = $('<tr>');
                        $tr.append($('<td>').text(data.path))
                            .append($('<td>').text(JSON.stringify(data.pattern.pattern)))
                            .append($('<td>').text(JSON.stringify(data.info)));
                        output.append($tr);
                        // And now scroll down within the output div so the most recent message
                        // is displayed.
                        container.scrollTop(container[0].scrollHeight);
                        totalMatches += 1;
                        $totalMatches.text(totalMatches);
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
