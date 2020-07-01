{-# LANGUAGE OverloadedStrings #-}
module Types.AppConfig where

import Types.Base    (FilePath)
import Prelude hiding(FilePath)

newtype Config = Config {saveFile :: FilePath}
