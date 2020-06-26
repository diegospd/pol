module Adapter.Tree where

import Types.ETree
import Types.Base
import Adapter.Entry as Entry
import Logic.ETree as ETree

textTreeToETree :: Tree Text -> ETree
textTreeToETree = ETree.fixTree . fmap Entry.textToEntry
