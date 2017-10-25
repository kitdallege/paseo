{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Paseo.Spider.VnuValidation
  (
    vnuValidateBSL
  , ValidationResults(..)
  , Message(..)
  , MessageType
  , Messages
  , main
  ) where
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types
import Data.Word8 (isSpace)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS8
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Char                  as Char
import           Data.Semigroup             ((<>))
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import           Data.Text                  (Text)
import qualified Data.Vector                as V
import           GHC.Generics
import           Prelude                    hiding (log)
import           System.Exit
import           System.IO
import           System.Process             hiding (readCreateProcessWithExitCode)
import           System.Process.ByteString.Lazy

vnuJarFile :: String
vnuJarFile = "/home/kit/vnu.jar"
-- all output is in stderr [not sure why]
-- java -jar ~/vnu.jar --format json /tmp/lvs-home.html 2>&1

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
validationResultsOptions = defaultOptions {
              fieldLabelModifier = map Char.toLower . drop 10
            , omitNothingFields = True
          }
instance ToJSON ValidationResults where
    toJSON = genericToJSON validationResultsOptions

instance FromJSON ValidationResults where
    parseJSON = genericParseJSON validationResultsOptions

gatherOutput :: ProcessHandle -> Handle -> IO (ExitCode, BSL.ByteString)
gatherOutput ph h = work mempty
  where
    work acc = do
        -- Read any outstanding input.
        bs <- BSL.hGetNonBlocking h (64 * 1024)
        let acc' = acc <> bs
        -- Check on the process.
        s <- getProcessExitCode ph
        -- Exit or loop.
        case s of
            Nothing -> work acc'
            Just ec -> do
                -- Get any last bit written between the read and the status
                -- check.
                last' <- BSL.hGetContents h
                return (ec, acc' <> last')

vnuValidateBSL :: BSL.ByteString -> IO (Either String ValidationResults)
vnuValidateBSL f = do
    let ps = proc "java" ["-jar", vnuJarFile, "--exit-zero-always", "--format", "json", "-"]
    (exitCode, _, err) <- readCreateProcessWithExitCode ps f
    case exitCode of
        ExitSuccess -> return $ eitherDecode err
        ExitFailure e -> return . Left $ "ExitCodeFailure: " <> show exitCode <> " Error: " <> show e

main :: IO ()
main = do
  f <- BSL.readFile "/tmp/lvs-home.html"
  results <- vnuValidateBSL f
  case results of
    Left e  -> print $ "Error: " <> e
    Right r -> BSL.putStr $ encodePretty' (defConfig {confIndent=Spaces 2}) r
