{-# LANGUAGE OverloadedStrings #-}
module Adapter.ETree where

import Adapter.Tree

import Types.Base
import Types.ETree
import Types.EState
import Types.Brick
import Logic.ETree as ETree

import qualified Data.Vector as V
import qualified Data.Tree as T
import qualified Data.Tree.Zipper as Tz


-- | Makes an Entry out of some Text.
textToEntry :: Text -> Entry
textToEntry x = En { _itsText     = x
                   , _isCollapsed = True
                   , _isVisible   = False
                   , _itsDepth    = -1
                   }

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
    where es = V.fromList $ filter ((^.isVisible) . fst) $ T.flatten $ toETreeL t
    -- where es = V.fromList $ {-filter (^.isVisible) $-} flatten $ toETreeL t

toETreeL :: ETree -> Tree (Entry, Zipper)
toETreeL t = setZippers (Tz.fromTree t) t

setZippers :: Zipper -> ETree -> Tree (Entry, Zipper)
setZippers z (Node e ts) = Node (e, z) (zipWith setZippers zs ts)
    where zs = [fromJust $ Tz.childAt (n-1) z | n <- [1 .. length ts] ]


-- | Makes a EState out of an ETree. Should be useful when
-- reading an ETree from disk.
toState :: ETree -> EState
toState t = St { _theTree        = t
                , _theList       = toList t
                , _theEditor     = editorText "theEditor" (Just 1) ""
                , _inEditMode    = False
                , _showingHelp   = ETree.isEmpty t
                , _lastSavedTree = Nothing
                , _minorChanges  = True
                , _rewinder      = []
                }