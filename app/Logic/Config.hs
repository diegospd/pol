module Logic.Config where

import Prelude hiding (FilePath)
import Types.Base
import Types.CliArguments
import Types.LocalConfig
import Types.AppConfig

build :: FilePath -> Maybe LocalConfig -> CliArgs -> Config
build defaultSaveFile Nothing                                (CliArgs [] verbose)         = Config defaultSaveFile verbose
build defaultSaveFile (Just (LocalConfig (Just saveFile) _)) (CliArgs [] verbose)         = Config saveFile verbose
build defaultSaveFile _localConfig                           (CliArgs [saveFile] verbose) = Config saveFile verbose
build defaultSaveFile _localConfig                           _cliArgs                     = Config defaultSaveFile False