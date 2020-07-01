{-# LANGUAGE OverloadedStrings #-}
module Types.LocalConfig where


import Types.Base
import Prelude hiding(FilePath)

data LocalConfig = LocalConfig
  { saveFile :: Maybe FilePath
  , verbose  :: Maybe Bool
  }
