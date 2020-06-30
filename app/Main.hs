{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative as OA
import Options.Applicative ((<**>))
import Data.Monoid         ((<>))

import Types.Base
import Prelude hiding(FilePath)
import Types.CliArguments  (CliArgs(..))
import Types.AppConfig (Config(..))

import Adapter.Arguments as Arguments
import StartApp(runTheApp)


main :: IO ()
main = operationMode =<< Arguments.build <$> OA.execParser opts
  where
    opts = OA.info (sample <**> OA.helper)
         ( OA.fullDesc
        <> OA.progDesc "Console GUI tree based note taker"
        <> OA.header   "you can choose which eert file to load" )

sample :: OA.Parser CliArgs
sample = CliArgs
      <$> OA.many (OA.argument OA.str (OA.metavar "FILE"))
      <*> OA.strOption
             ( OA.long    "filename"
            <> OA.short   'f'
            <> OA.metavar "FILE"
            <> OA.value   "~/.eert.main.json"
            <> OA.help    "Loads tree in FILE" )

operationMode (Config filename) = runTheApp filename

-- | Show options, no gui
greet :: Config -> IO ()
greet (Config filename) = putStrLn $ "Filename input: " <> show filename
