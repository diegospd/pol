{-# LANGUAGE OverloadedStrings #-}
module Adapter.Config where

import Types.CliArguments (CliArgs(..))
import Types.AppConfig (Config(..))
import Types.Base
import Prelude hiding(FilePath)


build :: CliArgs -> Config
build (CliArgs (file:_) _fallback verbose) = Config file     verbose
build (CliArgs []        fallback verbose) = Config fallback verbose
