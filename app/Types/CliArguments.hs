{-# LANGUAGE OverloadedStrings #-}
module Types.CliArguments where


import Types.Base
import Prelude hiding(FilePath)

data CliArgs = CliArgs
  { filelist :: [FilePath]
  , verbose  :: Bool
  } deriving(Show)
