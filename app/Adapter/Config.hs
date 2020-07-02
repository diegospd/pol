{-# LANGUAGE OverloadedStrings #-}
module Adapter.Config where

import Types.CliArguments (CliArgs(..))
import Types.AppConfig (Config(..))
import Types.Base
import Prelude hiding(FilePath)


fromCliArgs :: CliArgs -> Config
fromCliArgs (CliArgs (file:_) _fallback verbose) = Config file     verbose
fromCliArgs (CliArgs []        fallback verbose) = Config fallback verbose
