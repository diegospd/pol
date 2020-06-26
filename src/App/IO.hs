{-# LANGUAGE OverloadedStrings #-}

module App.IO where

import Types.Base
import Types.ETree

import Data.Aeson
import Prelude hiding (FilePath)
import Turtle
import Data.String.Conversions(cs)


getSaveFile :: FilePath -> IO FilePath
getSaveFile saveFile = home >>= \h -> return $ h </> saveFile

checkOrCreate :: FilePath -> IO Bool
checkOrCreate saveFile = do
    file <- getSaveFile saveFile
    b    <- testfile file
    unless b $ touch file
    return b

writeTree :: FilePath -> Tree Entry -> IO ()
writeTree saveFile t = do
    exists <- checkOrCreate saveFile
    file   <- getSaveFile saveFile
    writeTextFile file (cs $ encode t)

readTree :: FilePath -> IO (Maybe (Tree Entry))
readTree saveFile = do
    file   <- getSaveFile saveFile
    exists <- testfile file
    if not exists
        then return Nothing
        else do
            text <- readTextFile file
            return $ decode (cs text)
