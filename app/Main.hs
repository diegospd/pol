{-# LANGUAGE OverloadedStrings #-}

module Main where

import IO.LocalConfig (loadLocalConfig)
import qualified Options.Applicative as OA
import StartApp (runTheApp)
import qualified Turtle as Sh
import Types.AppConfig
import qualified Logic.Config as Config
import Types.Base
import Types.CliArguments (CliArgs (..))
import Prelude hiding (FilePath)

filenameDefaultSaveFile :: FilePath
filenameDefaultSaveFile = ".pol.tree.json"

filenameLocalConfig :: FilePath
filenameLocalConfig = ".pol.conf"


-- | (LocalConfig, default saveFile)
defaultFullFilePaths :: IO (FilePath, FilePath)
defaultFullFilePaths = do
  homeDirectory <- Sh.home
  return ( homeDirectory </> filenameLocalConfig
         , homeDirectory </> filenameDefaultSaveFile
         )

main :: IO ()
main = do
  (pathLocalConfig
   ,pathDefaultSaveFile) <- defaultFullFilePaths
  maybeLocalConfig       <- loadLocalConfig pathLocalConfig
  cliArgs                <- OA.execParser cliParser
  let config             =  Config.build pathDefaultSaveFile maybeLocalConfig cliArgs
  operationMode config

cliParser :: OA.ParserInfo CliArgs
cliParser =
  OA.info
    (cliInstructions OA.<**> OA.helper)
    (  OA.fullDesc
    <> OA.header    "POL - Console GUI tree-based note taker"
    <> OA.progDesc ("Launches the Pol GUI loading FILE or defaults to '~/"
                   <> Sh.encodeString filenameDefaultSaveFile
                   <> "' when no FILE argument is provided.")
    )

cliInstructions :: OA.Parser CliArgs
cliInstructions =
  CliArgs
    <$> OA.many (OA.argument
                    OA.str
                    (OA.metavar "FILE"))
    <*> OA.switch
      (   OA.long  "verbose"
       <> OA.short 'v'
       <> OA.help  "Enable verbose mode" )

operationMode :: Config -> IO ()
operationMode conf = do
  greet conf
  runTheApp conf

greet :: Config -> IO ()
greet conf = do
   print conf
