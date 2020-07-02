module Logic.Config where

import Prelude hiding (FilePath)
import Types.Base
import Types.CliArguments
import Types.LocalConfig
import Types.AppConfig
import Adapter.Config as Config

build :: FilePath -> Maybe LocalConfig -> CliArgs -> Config
build defaultSaveFile Nothing (CliArgs [] verbose) = Config defaultSaveFile verbose
--build defaultSaveFile _ (CliArgs [] verbose) = Config defaultSaveFile verbose
