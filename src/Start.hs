{-# LANGUAGE OverloadedStrings #-}
module Start
    ( someFunc
    ) where

import Types.Base
import Types.EState
import Types.ETree
import Adapter.ETree as ETree
import App
import qualified App.IO

someFunc :: IO ()
someFunc = runTheApp

emptyTree :: ETree
emptyTree = ETree.fixTree $ Node (textToEntry "root") []

runTheApp :: IO ()
runTheApp = do
    fromDisk <- App.IO.readTree
    let tree = fromMaybe emptyTree fromDisk
    let st   = ETree.toState tree & lastSavedTree ?~ tree
    void $ defaultMain theApp st


mkState :: Tree Text -> EState
mkState = toState .  textTreeToETree
