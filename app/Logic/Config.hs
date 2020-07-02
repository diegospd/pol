module Logic.Config where

import Prelude hiding (FilePath)
import Types.Base
import Types.CliArguments
import Types.LocalConfig
import Types.AppConfig
import Adapter.Config as Config

build :: Maybe LocalConfig -> CliArgs -> Config
build Nothing cliArgs = Config.fromCliArgs cliArgs
