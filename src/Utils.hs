module Utils 
    ( Text(..)
    , Vector(..)
    , Tree(..)
    -- , TreePos(..)
    , applyToRoot
    , applyToAllButRoot
    , applyToFirstGen
    , replaceZipper
    , (!!!)
    , module X
    )
where


import Lens.Micro.Platform
import qualified Data.Vector as V
import Data.Tree

------------ exports --------------
import Data.Vector (Vector(..))
import Data.Text(Text(..))
import Data.Tree.Zipper(TreePos(..))

import Brick                as X
import Brick.Widgets.List   as X
import Brick.Widgets.Center as X
import Brick.Widgets.Edit   as X
import Data.Text.Zipper     as X
import Control.Monad        as X
import Data.Monoid          as X
import Data.List            as X
import Data.Maybe           as X
import Data.Default         as X
import Data.Char            as X
import Data.Either          as X

(!!!) :: [a] -> Int -> Maybe a
(!!!) xs n
    | length xs <= n = Nothing
    | otherwise = Just $ xs !! n


-- | Applies a function only to the root of a Tree.
applyToRoot :: (a -> a) -> Tree a -> Tree a
applyToRoot f (Node e ts) = Node (f e) ts 

-- | Applies a function to every element of the tree except for the root.
applyToAllButRoot :: (a -> a) -> Tree a -> Tree a
applyToAllButRoot f (Node e ts) = Node e $ map (fmap f) ts

-- | Applies a function only to the sons of the root node of a Tree.
applyToFirstGen :: (a -> a) -> Tree a -> Tree a
applyToFirstGen f (Node e ts) = Node e $ map (applyToRoot f) ts


replaceZipper :: Text -> b -> TextZipper Text 
replaceZipper t  = const $ textZipper [t] (Just 1)
