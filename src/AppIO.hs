{-# LANGUAGE OverloadedStrings #-}

module AppIO where

import Types
import Utils

import Data.Aeson
import Prelude hiding (FilePath)
import Turtle
import Data.String.Conversions(cs)

saveFile :: FilePath
saveFile = ".pol_tree"
-- saveFile = "/home/your_user/some_dir/use_this_file.json"

getSaveFile :: IO FilePath
getSaveFile = home >>= \h -> return $ h </> saveFile 

checkOrCreate :: IO Bool
checkOrCreate = do
    file <- getSaveFile
    b    <- testfile file
    unless b $ touch file
    return b

writeTree :: Tree Entry -> IO ()
writeTree t = do
    exists <- checkOrCreate
    file   <- getSaveFile
    writeTextFile file (cs $ encode t)

readTree :: IO (Maybe (Tree Entry))
readTree = do
    file   <- getSaveFile
    exists <- testfile file
    if not exists
        then return Nothing
        else do
            text <- readTextFile file
            return $ decode (cs text) 
