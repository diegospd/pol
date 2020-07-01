{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative as OA
import Options.Applicative ((<**>))
import Data.Monoid         ((<>))

import Types.Base
import Prelude hiding(FilePath)
import Types.CliArguments  (CliArgs(..))
import Types.AppConfig (Config(..))

import Adapter.Config as Config
import StartApp(runTheApp)


main :: IO ()
main = operationMode =<< Config.build <$> OA.execParser cliParser

cliParser :: OA.ParserInfo CliArgs
cliParser = OA.info (cliInstructions <**> OA.helper)
                   ( OA.fullDesc
                   <> OA.progDesc "Console GUI tree based note taker"
                   <> OA.header   "you can choose which eert file to load" )

cliInstructions :: OA.Parser CliArgs
cliInstructions = CliArgs
      <$> OA.many (OA.argument OA.str (OA.metavar "FILE"))
      <*> OA.strOption
             ( OA.long    "filename"
            <> OA.short   'f'
            <> OA.metavar "FILE"
            <> OA.value   "~/.eert.main.json"
            <> OA.help    "Loads tree in FILE" )

operationMode = runTheApp

-- | Show options, no gui
greet :: Config -> IO ()
greet (Config filename) = putStrLn $ "Filename input: " <> show filename
