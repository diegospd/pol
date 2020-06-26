module Adapter.ETree where

import Types.Base
import Types.ETree
import Types.Brick
import qualified Data.Vector as V


textTreeToETree :: Tree Text -> ETree
textTreeToETree = fixTree . fmap textToEntry

-- | Sets visibilites and depths of an ETree
fixTree :: ETree -> ETree
fixTree = setDepths 0 . setVisibilities . rootIsNeitherCollapsedNorVisible

rootIsNeitherCollapsedNorVisible :: ETree -> ETree
rootIsNeitherCollapsedNorVisible (Node e ts) = Node e' ts
    where e' = e & isCollapsed .~ False & isVisible .~ False

setVisibilities :: ETree -> ETree
setVisibilities t@(Node e ts)
    | e^.isCollapsed = applyToAllButRoot (& isVisible .~ False) t
    | otherwise = Node e $ map (setVisibilities . applyToRoot (& isVisible .~ True)) ts

setDepths :: Int -> ETree -> ETree
setDepths n (Node e ts) = Node (e & itsDepth .~ n) $ map (setDepths (n+1)) ts


----------

toList :: ETree -> List N (Entry, Zipper)
toList t = list "theList" es 1
    where es = V.fromList $ filter ((^.isVisible) . fst) $ flatten $ toETreeL t
    -- where es = V.fromList $ {-filter (^.isVisible) $-} flatten $ toETreeL t

toETreeL :: ETree -> Tree (Entry, Zipper)
toETreeL t = setZippers (fromTree t) t

setZippers :: Zipper -> ETree -> Tree (Entry, Zipper)
setZippers z (Node e ts) = Node (e, z) (zipWith setZippers zs ts)
    where zs = [fromJust $ childAt (n-1) z | n <- [1 .. length ts] ]
