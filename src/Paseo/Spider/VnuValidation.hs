{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Paseo.Spider.VnuValidation
  (
    vnuValidateBSL
  , ValidationResults(..)
  , Message(..)
  , MessageType
  , Messages
  , test
  ) where
import           Data.Aeson                     (eitherDecode)
import           Data.Aeson.Encode.Pretty       (Indent (..), confIndent,
                                                 defConfig, encodePretty')
import           Data.Aeson.Types               (Options (..), defaultOptions,
                                                 fieldLabelModifier,
                                                 genericParseJSON,
                                                 genericToJSON,
                                                 omitNothingFields)
import qualified Data.ByteString.Lazy           as BSL
import qualified Data.Char                      as Char
import qualified Data.Vector                    as V
import           Import
import           System.Exit                    (ExitCode (..))
import           System.Process                 (proc)
import           System.Process.ByteString.Lazy (readCreateProcessWithExitCode)

data MessageType =
      MsgError
    | MsgFatal
    | MsgInfo
    | MsgInternal
    | MsgIO
    | MsgNonDocument
    | MsgSchema
    | MsgWarning
    deriving (Show, Eq, Ord, Enum, Generic)

messageTypeOptions :: Options
messageTypeOptions = defaultOptions {
              constructorTagModifier = map Char.toLower . drop 3}

instance ToJSON MessageType where
    toJSON = genericToJSON messageTypeOptions

instance FromJSON MessageType where
    parseJSON = genericParseJSON messageTypeOptions

data Message = Message
    { msgType         :: MessageType
    , msgUrl          :: Maybe Text
    , msgLastLine     :: Int
    , msgFirstLine    :: Maybe Int
    , msgLastColumn   :: Int
    , msgFirstColumn  :: Maybe Int
    , msgSubType      :: Maybe MessageType
    , msgMessage      :: Text
    , msgExtract      :: Maybe Text
    , msgHiliteStart  :: Maybe Int
    , msgHiliteLength :: Maybe Int
    } deriving (Show, Eq, Ord, Generic)

messageOptions :: Options
messageOptions = defaultOptions {fieldLabelModifier = lowerOne . drop 3}
  where
    lowerOne (x:xs) = Char.toLower x : xs
    lowerOne []     = ""

instance ToJSON Message where
    toJSON = genericToJSON messageOptions

instance FromJSON Message where
    parseJSON = genericParseJSON messageOptions

type Messages = V.Vector Message
data ValidationResults = ValidationResults
    { valResultsMessages :: Messages
    , valResultsUrl      :: Maybe Text
    , valResultsSource   :: Maybe Text
    , valResultsLanguage :: Maybe Text
    } deriving (Show, Eq, Ord, Generic)

validationResultsOptions :: Options
validationResultsOptions = defaultOptions
            { fieldLabelModifier = map Char.toLower . drop 10
            , omitNothingFields = True
            }

instance ToJSON ValidationResults where
    toJSON = genericToJSON validationResultsOptions

instance FromJSON ValidationResults where
    parseJSON = genericParseJSON validationResultsOptions


-- TODO: This needs to be a parameter (or) programmatic.
-- all output is in stderr [not sure why]
-- java -jar ~/vnu.jar --format json /tmp/lvs-home.html 2>&1
vnuJarFile :: String
vnuJarFile = "/home/kit/vnu.jar"

-- | Validate a HTML document with the vnu.jar from "The Nu Html Checker (v.Nu)"
-- which is the backend to
--  * checker.html5.org
--  * html5.validator.nu
--  * validator.w3.org/nu
-- see: https://validator.github.io/validator/ for more information
-- The 'ValidationResults' are returned which contain all the fields
-- from the jar's output.
vnuValidateBSL :: LByteString                   -- ^ HTML page to validate
    -> IO (Either String ValidationResults)     -- ^ Results of validation or a String error if the subprocess (or) json decode fail.
vnuValidateBSL f = do
    let ps = proc "java" ["-jar", vnuJarFile, "--exit-zero-always", "--format", "json", "-"]
    (exitCode, _, err) <- readCreateProcessWithExitCode ps f
    case exitCode of
        ExitSuccess -> return $ eitherDecode err
        ExitFailure e -> return . Left $ "ExitCodeFailure: " <> show exitCode <> " Error: " <> show e

test :: IO ()
test = do
  f <- BSL.readFile "/tmp/lvs-home.html"
  results <- vnuValidateBSL f
  case results of
    Left e  -> print $ "Error: " <> e
    Right r -> BSL.putStr $ encodePretty' (defConfig {confIndent=Spaces 2}) r
