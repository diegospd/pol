{-# LANGUAGE OverloadedStrings #-}

module Main where

import Adapter.Config as Config
import Data.Monoid ((<>))
import IO.Config (loadLocalConfig)
import qualified Options.Applicative as OA
import StartApp (runTheApp)
import qualified Turtle as Sh
import Types.AppConfig (Config (..))
import Types.Base
import Types.CliArguments (CliArgs (..))
import Prelude hiding (FilePath)

operationMode = runTheApp

main :: IO ()
main = do
  homeDir <- Sh.home
  let localConfigFile = homeDir </> ".eert.conf"
  localConfig <- loadLocalConfig localConfigFile
  config <- Config.build <$> OA.execParser (cliParser homeDir)
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
greet conf = putStrLn $ "Filename input: " <> show (saveFile conf)
