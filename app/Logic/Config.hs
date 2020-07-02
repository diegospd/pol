module Logic.Config where

import Prelude hiding (FilePath)
import Types.Base
import Types.CliArguments
import Types.LocalConfig
import Types.AppConfig
import Adapter.Config as Config

build :: Maybe LocalConfig -> CliArgs -> Config
build Nothing cliArgs = Config.fromCliArgs cliArgs
build (Just local) cliArgs = Config undefined undefined
    where cliConfig = Config.fromCliArgs cliArgs
