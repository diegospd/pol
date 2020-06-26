module Logic.Zipper where

import Types.ETree
import Data.Tree.Zipper as Z

isFirstLevelOrRoot :: Zipper -> Bool
isFirstLevelOrRoot z = maybe True Z.isRoot (Z.parent z)