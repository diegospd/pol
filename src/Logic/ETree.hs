{-# LANGUAGE OverloadedStrings #-}
module Logic.ETree where

import Prelude hiding (FilePath)
import Types.Base
import Types.ETree
import Types.EState
import qualified Types.Brick   as B
import qualified Adapter.ETree as ETree
import qualified Adapter.Entry as Entry

import Logic.Tree as T

isEmpty :: ETree -> Bool
isEmpty (Node _ [])    = True
isEmpty (Node _ (_:_)) = False

isNotEmpty :: ETree -> Bool
isNotEmpty = not . isEmpty

-- | Sets visibilites and depths of an ETree
reveal :: ETree -> ETree
reveal = updateDepths 0 . updateVisibilities . expandAndRevealRootNode

updateDepths :: Int -> ETree -> ETree
updateDepths n (Node e ts) = Node (e & itsDepth .~ n) $ map (updateDepths (n+1)) ts


expandAndRevealRootNode :: ETree -> ETree
expandAndRevealRootNode (Node e ts) = Node e' ts
    where e' = e & isCollapsed .~ False & isVisible .~ False

updateVisibilities :: ETree -> ETree
updateVisibilities t@(Node e ts)
    | e^.isCollapsed = applyToAllButRoot (& isVisible .~ False) t
    | otherwise = Node e $ map (updateVisibilities . T.applyToRoot (& isVisible .~ True)) ts

-- | Makes a EState out of an ETree. Should be useful when
-- reading an ETree from disk.
toState :: ETree -> EState
toState t = St { _theTree        = t
                , _theList       = ETree.toList t
                , _theEditor     = B.editorText "theEditor" (Just 1) ""
                , _inEditMode    = False
                , _showingHelp   = isEmpty t
                , _lastSavedTree = Nothing
                , _minorChanges  = True
                , _rewinder      = []
                }

-- | Says if two trees are equal based only on their structure and their texts.
equalByText :: ETree -> ETree -> Bool
equalByText t1 t2 = (f <$> t1) == (f <$> t2)
    where f = (^.itsText)

emptyTree :: ETree
emptyTree = reveal $ Node (Entry.fromText "root") []