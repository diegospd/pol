{-# LANGUAGE OverloadedStrings #-}
module App.IO where

import Prelude hiding (FilePath)

import Types.Base
import Types.ETree
import Data.String.Conversions(cs)

import qualified Data.Aeson as Json
import qualified Turtle     as Sh


checkOrCreate :: FilePath -> IO Bool
checkOrCreate saveFile = do
    exists <- Sh.testfile saveFile
    unless exists $ Sh.touch saveFile
    return exists

writeTree :: FilePath -> Tree Entry -> IO ()
writeTree saveFile t = do
    _exists <- checkOrCreate saveFile
    Sh.writeTextFile saveFile (cs $ Json.encode t)

readTree :: FilePath -> IO (Maybe (Tree Entry))
readTree saveFile = do
    exists <- Sh.testfile saveFile
    if not exists
        then return Nothing
        else do
            text <- Sh.readTextFile saveFile
            return $ Json.decode (cs text)
