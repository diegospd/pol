{-# LANGUAGE OverloadedStrings #-}

module Main where

import Adapter.Config as Config
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


main :: IO ()
main = do
  homeDir                 <- Sh.home
  let localConfigFilepath =  homeDir </> ".eert.conf"
  maybeLocalConfig        <- loadLocalConfig localConfigFilepath
  cliArgs                 <- OA.execParser (cliParser homeDir)
  let config              =  Config.build maybeLocalConfig cliArgs
  operationMode config

cliParser :: FilePath -> OA.ParserInfo CliArgs
cliParser homeDir =
  OA.info
    (cliInstructions homeDir OA.<**> OA.helper)
    ( OA.fullDesc
        <> OA.progDesc "Console GUI tree-based note taker"
        <> OA.header "you can choose which tree file to load"
    )

cliInstructions :: FilePath -> OA.Parser CliArgs
cliInstructions homeDir =
  CliArgs
    <$> OA.many (OA.argument OA.str
                             (OA.metavar "FILE"))
    <*> OA.strOption
      ( OA.long         "filename"
          <> OA.short   'f'
          <> OA.metavar "FILE"
          <> OA.value   (homeDir </> ".eert.tree.json")
          <> OA.help    "Loads tree in FILE"
      )
    <*> OA.switch
      ( OA.long "verbose"
       <> OA.short 'v'
       <> OA.help "Enable verbose mode" )

-- | Show options, no gui
greet :: Config -> IO ()
greet conf = do
   putStrLn $ "Filename input: " <> show (saveFile conf)
   putStrLn $ "verbosity: " <> show (Config.verbose conf)


operationMode :: Config -> IO ()
operationMode conf = do
  greet conf
--  runTheApp conf
