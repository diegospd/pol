{-# LANGUAGE OverloadedStrings #-}
module Logic.ETree where

import Types.ETree
import Types.Base
import Prelude hiding (FilePath)
import Types.Brick
import Types.EState
import Adapter.ETree as ETree
import Adapter.Entry as Entry

import Logic.Tree as T

isEmpty :: ETree -> Bool
isEmpty (Node _ [])    = True
isEmpty (Node _ (_:_)) = False

isNotEmpty :: ETree -> Bool
isNotEmpty = not . isEmpty

-- | Sets visibilites and depths of an ETree
fixTree :: ETree -> ETree
fixTree = setDepths 0 . setVisibilities . rootIsNeitherCollapsedNorVisible

setDepths :: Int -> ETree -> ETree
setDepths n (Node e ts) = Node (e & itsDepth .~ n) $ map (setDepths (n+1)) ts



rootIsNeitherCollapsedNorVisible :: ETree -> ETree
rootIsNeitherCollapsedNorVisible (Node e ts) = Node e' ts
    where e' = e & isCollapsed .~ False & isVisible .~ False

setVisibilities :: ETree -> ETree
setVisibilities t@(Node e ts)
    | e^.isCollapsed = applyToAllButRoot (& isVisible .~ False) t
    | otherwise = Node e $ map (setVisibilities . T.applyToRoot (& isVisible .~ True)) ts

-- | Makes a EState out of an ETree. Should be useful when
-- reading an ETree from disk.
toState :: ETree -> EState
toState t = St { _theTree        = t
                , _theList       = ETree.toList t
                , _theEditor     = editorText "theEditor" (Just 1) ""
                , _inEditMode    = False
                , _showingHelp   = isEmpty t
                , _lastSavedTree = Nothing
                , _minorChanges  = True
                , _rewinder      = []
                }

-- | Says if two trees are equal based only on their structure and their texts.
eqByText :: ETree -> ETree -> Bool
eqByText t1 t2 = (f <$> t1) == (f <$> t2)
    where f = (^.itsText)

emptyTree :: ETree
emptyTree = fixTree $ Node (Entry.textToEntry "root") []