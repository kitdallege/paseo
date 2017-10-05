{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Paseo.Web
Description : Contains main entry point for the Paseo Web Applicaiton.
Copyright   : (c) Kit C. Dallege 2017
License     : BSD3 (see the file LICENSE)
Maintainer  : Kit Dallege <kitdallege@gmail.com>
Stability   : experimental
Portability : POSIX

Contains the main Scotty application.
-}module Paseo.Web
  (
    main
  , application

  ) where
import           Control.Concurrent.STM.TVar
import           Paseo.Web.App
import           Paseo.Web.Types
import qualified Web.Scotty.Trans            as S


main :: IO ()
main = do
  spiders <- newTVarIO []
  let env = Env { envConfig = config, envPort = 8080, envSpiders = spiders}
  S.scottyT (envPort env) (runAppM env) application
  where
    config :: Config
    config = Config
      { configStorageLocation = "/tmp/paseo"
      , configVerbose = True
      }
