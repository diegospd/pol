module Logic.Zipper
  ( isFirstLevelOrRoot
  , zipETree
  , module X
  )
where

import Data.Maybe
import Data.Tree.Zipper as X
import Types.ETree as ETree


isFirstLevelOrRoot :: ETree.Zipper -> Bool
isFirstLevelOrRoot z = maybe True X.isRoot (X.parent z)

zipETree :: ETree.Zipper -> ETree -> Tree (Entry, Zipper)
zipETree z (Node e ts) = Node (e, z) (zipWith zipETree zs ts)
    where zs = [fromJust $ X.childAt (n-1) z | n <- [1 .. length ts] ]
