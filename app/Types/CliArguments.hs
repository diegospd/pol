module Types.CliArguments where

data CliArgs = CliArgs
  { filelist :: [String]
  , filenam  :: String
  }