{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Paseo.Web.Types
Description : Core Types for the Paseo Web Applicaiton.
Copyright   : (c) Kit C. Dallege 2017
License     : BSD3 (see the file LICENSE)
Maintainer  : Kit Dallege <kitdallege@gmail.com>
Stability   : experimental
Portability : POSIX

Contains main AppM monad as well as configuration types.
-}
module Paseo.Web.Types
  (
    Config(..)
  , Environment(..)
  , AppM
  , runAppM
  ) where
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT,
                                       runReaderT)
import Data.Semigroup ((<>))
import           Data.Text            (Text)
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Data.Vector


data Config = Config
  { configStorageLocation :: !Text
  , configVerbose         :: !Bool
  } deriving (Eq, Read, Show)

data Environment = Env
  {
    envConfig :: Config
  , envPort :: Int
  , envSpiders :: TVar [Async FilePath]
  } deriving (Eq)

instance Show Environment where
  show e = "Env {envConfig=" <> show (envConfig e) <> ", envPort=" <> show (envPort e) <> "}"

newtype AppM a = AppM
  { unAppM :: ReaderT Environment IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadReader Environment)

runAppM :: Environment -> AppM a -> IO a
runAppM env m = runReaderT (unAppM m) env
