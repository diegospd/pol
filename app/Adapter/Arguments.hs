{-# LANGUAGE OverloadedStrings #-}
module Adapter.Arguments where

import Types.CliArguments (CliArgs(..))
import Types.EertArguments (EertArgs(..))
import Types.Base (FilePath)
import Prelude hiding(FilePath)


build :: CliArgs -> EertArgs
build (CliArgs (file:_) _fallback) = EertArgs file
build (CliArgs []        fallback) = EertArgs fallback
