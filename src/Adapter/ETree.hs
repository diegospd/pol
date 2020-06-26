{-# LANGUAGE OverloadedStrings #-}
module Adapter.ETree where


import Prelude hiding (FilePath)
import Types.Base
import Types.ETree as ETree
import Types.EState

import qualified Types.Brick  as B
import qualified Data.Vector  as V
import qualified Logic.Tree   as T
import qualified Logic.Zipper as Z


toTree :: ETree -> Tree (Entry, Zipper)
toTree t = Z.zipETree (Z.fromTree t) t

toList :: ETree -> B.List B.N (Entry, Zipper)
toList t = B.list "theList" es 1
    where es = V.fromList $ filter ((^. ETree.isVisible) . fst) $ T.flatten $ toTree t
