module Logic.Tree
 (applyToRoot,
 applyToAllButRoot,
 applyToFirstGen,
 applyToForest,
 module X
 )
 where

import Types.Base
import Prelude hiding (FilePath)
import Types.ETree

import Data.Tree as X

-- | Applies a function only to the root of a Tree.
applyToRoot :: (a -> a) -> Tree a -> Tree a
applyToRoot f (Node e ts) = Node (f e) ts

-- | Applies a function to every element of the tree except for the root.
applyToAllButRoot :: (a -> a) -> Tree a -> Tree a
applyToAllButRoot f (Node e ts) = Node e $ map (fmap f) ts

-- | Applies a function only to the sons of the root node of a Tree.
applyToFirstGen :: (a -> a) -> Tree a -> Tree a
applyToFirstGen f (Node e ts) = Node e $ map (applyToRoot f) ts

applyToForest :: ([Tree a] -> [Tree a]) -> Tree a -> Tree a
applyToForest f (Node e ts) = Node e (f ts)