{-# LANGUAGE OverloadedStrings #-}

module Types.AppConfig where

import Types.Base (FilePath)
import Prelude hiding (FilePath)

data Config
  = Config { saveFile :: FilePath
           , verbose  :: Bool
           } deriving(Show)
