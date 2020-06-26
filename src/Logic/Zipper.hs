module Logic.Zipper
  ( isFirstLevelOrRoot,
    module X
  )
where

import Data.Tree.Zipper as X

import Types.ETree(Zipper(..))

isFirstLevelOrRoot :: Zipper -> Bool
isFirstLevelOrRoot z = maybe True X.isRoot (X.parent z)
