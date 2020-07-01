{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Options.Applicative as OA
import Data.Monoid         ((<>))

import Types.Base
import Prelude hiding(FilePath)
import Types.CliArguments  (CliArgs(..))
import Types.AppConfig (Config(..))
import qualified Turtle as Sh

import Adapter.Config as Config
import StartApp(runTheApp)


operationMode = runTheApp

main :: IO ()
main = do
    homeDir <- Sh.home
    config  <- Config.build <$> OA.execParser (cliParser homeDir)
    operationMode config

cliParser :: FilePath -> OA.ParserInfo CliArgs
cliParser homeDir = OA.info (cliInstructions homeDir <**> OA.helper)
                   (  OA.fullDesc
                   <> OA.progDesc "Console GUI tree-based note taker"
                   <> OA.header   "you can choose which tree file to load" )

cliInstructions :: FilePath -> OA.Parser CliArgs
cliInstructions homeDir = CliArgs
      <$> OA.many (OA.argument OA.str (OA.metavar "FILE"))
      <*> OA.strOption
             ( OA.long    "filename"
            <> OA.short   'f'
            <> OA.metavar "FILE"
            <> OA.value   (homeDir </> ".eert.tree.json")
            <> OA.help    "Loads tree in FILE" )


-- | Show options, no gui
greet :: Config -> IO ()
greet (Config filename) = putStrLn $ "Filename input: " <> show filename
