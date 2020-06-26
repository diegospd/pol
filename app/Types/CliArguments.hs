module Types.CliArguments where


import Types.Base
import Prelude hiding(FilePath)

data CliArgs = CliArgs
  { filelist :: [FilePath]
  , filename  :: FilePath
  }