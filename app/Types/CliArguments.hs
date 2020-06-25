module Types.CliArguments where

data CliArgs = CliArgs
  {filelist    :: [String],
  filename     :: String
  }