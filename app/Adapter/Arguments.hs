{-# LANGUAGE OverloadedStrings #-}
module Adapter.Arguments where

import Types.CliArguments (CliArgs(..))
import Types.EertArguments (AppArgs(..))
import Types.Base (FilePath)
import Prelude hiding(FilePath)


build :: CliArgs -> AppArgs
build (CliArgs (file:_) _fallback) = AppArgs file
build (CliArgs []        fallback) = AppArgs fallback
