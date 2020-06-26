{-# LANGUAGE OverloadedStrings #-}
module Types.EertArguments where

import Types.Base (FilePath)
import Prelude hiding(FilePath)

newtype AppArgs = AppArgs {filename :: FilePath}
