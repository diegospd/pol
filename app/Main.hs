{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid ((<>))
import IO.LocalConfig (loadLocalConfig)
import qualified Options.Applicative as OA
import StartApp (runTheApp)
import qualified Turtle as Sh
import Types.AppConfig
import qualified Types.AppConfig as Config
import qualified Logic.Config as Config
import Types.Base
import Types.CliArguments (CliArgs (..))
import Prelude hiding (FilePath)

-- | (LocalConfig, default saveFile)
defaultFileLocations :: IO (FilePath, FilePath)
defaultFileLocations = do
  homeDirectory <- Sh.home
  return ( homeDirectory </> ".pol.conf", homeDirectory </> ".pol.tree.json")

main :: IO ()
main = do
  (localConfigFilepath, defaultSaveFile) <- defaultFileLocations
  maybeLocalConfig        <- loadLocalConfig localConfigFilepath
  cliArgs                 <- OA.execParser (cliParser undefined)
  let config              =  Config.build localConfigFilepath maybeLocalConfig cliArgs
  operationMode config

cliParser :: FilePath -> OA.ParserInfo CliArgs
cliParser homeDir =
  OA.info
    (cliInstructions homeDir OA.<**> OA.helper)
    (  OA.fullDesc
    <> OA.header   "POL - Console GUI tree-based note taker"
    <> OA.progDesc "Launches the Pol GUI loading FILE or default "
    )

cliInstructions :: FilePath -> OA.Parser CliArgs
cliInstructions homeDir =
  CliArgs
    <$> OA.many (OA.argument OA.str
                             (OA.metavar "FILE"))
    <*> OA.switch
      ( OA.long "verbose"
       <> OA.short 'v'
       <> OA.help "Enable verbose mode" )

-- | Show options, no gui
greet :: Config -> IO ()
greet conf = do
   print conf


operationMode :: Config -> IO ()
operationMode conf = do
  greet conf
  runTheApp conf
