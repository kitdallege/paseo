{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Paseo.Spider.VnuValidation
  (
    vnuValidate
  , ValidationResults(..)
  , Message(..)
  , MessageType
  , Messages
  , main
  ) where
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Char                  as Char
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import qualified Data.Vector                as V
import           GHC.Generics
import           Prelude                    hiding (log)
import           System.Exit
import           System.IO
import           System.Process

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
  {
    msgType         :: MessageType
  , msgUrl          :: Maybe Text
  , msgLastLine     :: Int
  , msgLastColumn   :: Int
  , msgFirstColumn  :: Int
  , msgSubType      :: Maybe MessageType
  , msgMessage      :: Text
  , msgExtract      :: Text
  , msgHiliteStart  :: Int
  , msgHiliteLength :: Int
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
  {
    valResultsMessages :: Messages
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

vnuValidate :: BSL.ByteString -> IO (Either String ValidationResults)
vnuValidate f = do
  (Just inp, _,  Just errp, phdl) <- createProcess
    (proc "java" ["-jar", vnuJarFile, "--exit-zero-always", "--format", "json", "-"])
      { std_in = CreatePipe
      , std_err = CreatePipe
      , std_out = UseHandle stdout
      }
  BSL.hPutStr inp f
  exitCode <- waitForProcess phdl
  case exitCode of
    ExitSuccess -> do
      putStrLn "reading err pipe"
      input <- BSL.hGetContents errp
      let results = eitherDecode' input :: Either String ValidationResults
      return results
    ExitFailure _ -> return . Left $ "ExitCodeFailure: " <> show exitCode

main :: IO ()
main = do
  f <- BSL.readFile "/tmp/lvs-home.html"
  results <- vnuValidate f
  case results of
    Left e  -> print $ "Error: " <> e
    Right r -> BSL.putStr $ encodePretty' (defConfig {confIndent=Spaces 2}) r
