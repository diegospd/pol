{-# LANGUAGE OverloadedStrings #-}
module Adapter.ETree where


import Prelude hiding (FilePath)
import Types.Base
import Types.ETree
import Types.EState
import Types.Brick as Brick

import qualified Data.Vector as V
import qualified Logic.Tree as T
import qualified Data.Tree.Zipper as Tz



toList :: ETree -> Brick.List Brick.N (Entry, Zipper)
toList t = list "theList" es 1
    where es = V.fromList $ filter ((^.isVisible) . fst) $ T.flatten $ toTree t
    -- where es = V.fromList $ {-filter (^.isVisible) $-} flatten $ toTree t

toTree :: ETree -> Tree (Entry, Zipper)
toTree t = setZippers (Tz.fromTree t) t

setZippers :: Zipper -> ETree -> Tree (Entry, Zipper)
setZippers z (Node e ts) = Node (e, z) (zipWith setZippers zs ts)
    where zs = [fromJust $ Tz.childAt (n-1) z | n <- [1 .. length ts] ]


