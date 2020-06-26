{-# LANGUAGE OverloadedStrings #-}
module Start
    ( someFunc
    ) where

import Types.Base
import Types.EState
import Types.ETree
import Adapter.ETree as ETree
import Adapter.Tree as Tree
import Logic.ETree as ETree
import Adapter.Entry as Entry
import App(runTheApp)
import qualified App.IO

someFunc :: IO ()
someFunc = runTheApp


mkState :: Tree Text -> EState
mkState = toState .  Tree.textTreeToETree
