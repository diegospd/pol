{-# LANGUAGE OverloadedStrings #-}
module Adapter.Config where

import Types.CliArguments (CliArgs(..))
import Types.AppConfig (Config(..))
import Types.Base (FilePath)
import Prelude hiding(FilePath)


build :: CliArgs -> Config
build (CliArgs (file:_) _fallback) = Config file False
build (CliArgs []        fallback) = Config fallback False
