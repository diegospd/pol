module Main where

import Start

import qualified Options.Applicative
import           Options.Applicative as OA
import           Options.Applicative (metavar,help, helper, info, execParser, long,option, auto, showDefault, value, short, switch, strOption,header,progDesc, (<**>), fullDesc)
import           Data.Monoid   ((<>))

import           Types.CliArguments (CliArgs(..))
import           Types.EertArguments (EertArgs(..))
import           Adapters.Arguments (buildArgs)

sample :: Options.Applicative.Parser CliArgs
sample = CliArgs
      <$> OA.many (OA.argument OA.str (OA.metavar "FILE"))
      <*> OA.strOption
             ( OA.long "filename"
            <> OA.short 'f'
            <> OA.metavar "FILE"
            <> OA.value "~/.eert.main.json"
            <> OA.help "Loads tree in FILE" )

main :: IO ()
main = greet =<< buildArgs <$> execParser opts
  where
    opts = OA.info (sample <**> OA.helper)
      ( fullDesc
     <> progDesc "Console GUI tree based note taker"
     <> header "you can choose which eert file to load" )

greet :: EertArgs -> IO ()
greet (EertArgs filename) = putStrLn $ "Filename input: " <> filename

--main = putStrLn "Hola"