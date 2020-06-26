{-# LANGUAGE OverloadedStrings #-}
module Adapter.Tree where

import Types.ETree
import Types.Base
import Prelude hiding (FilePath)
import Types.EState
import Adapter.Entry as Entry
import Logic.ETree as ETree

textTreeToETree :: Tree Text -> ETree
textTreeToETree = ETree.fixTree . fmap Entry.fromText

textTreeToEState :: Tree Text -> EState
textTreeToEState = ETree.toState . textTreeToETree
