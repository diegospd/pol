{-# LANGUAGE OverloadedStrings #-}
module Adapter.Tree where

import           Prelude       hiding (FilePath)
import           Types.Base
import           Types.ETree
import           Types.EState
import           Types.AppConfig(Config(..))
import qualified Adapter.Entry as Entry
import qualified Logic.ETree   as ETree


textTreeToETree :: Tree Text -> ETree
textTreeToETree = ETree.reveal . fmap Entry.fromText

textTreeToEState :: Config -> Tree Text -> EState
textTreeToEState conf = ETree.toState conf . textTreeToETree
