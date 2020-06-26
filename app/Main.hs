module Main where

import           Options.Applicative as OA
import           Options.Applicative ((<**>))
import           Data.Monoid ((<>))

import           Types.CliArguments (CliArgs(..))
import           Types.EertArguments (EertArgs(..))

import qualified Adapter.Arguments as Adapt
import Start


main :: IO ()
main = greet =<< Adapt.buildArgs <$> OA.execParser opts
  where
    opts = OA.info (sample <**> OA.helper)
      ( OA.fullDesc
     <> OA.progDesc "Console GUI tree based note taker"
     <> OA.header   "you can choose which eert file to load" )

sample :: OA.Parser CliArgs
sample = CliArgs
      <$> OA.many (OA.argument OA.str (OA.metavar "FILE"))
      <*> OA.strOption
             ( OA.long "filename"
            <> OA.short 'f'
            <> OA.metavar "FILE"
            <> OA.value "~/.eert.main.json"
            <> OA.help "Loads tree in FILE" )

greet :: EertArgs -> IO ()
greet (EertArgs filename) = putStrLn $ "Filename input: " <> filename
