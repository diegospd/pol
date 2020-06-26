{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.State where

import Types.Brick (N(..), List(..), Editor(..))
import Types.ETree
import GHC.Generics
import Lens.Micro.Platform

data EState = St { _theTree       :: ETree
                 , _theList       :: List N (Entry, Zipper)
                 , _theEditor     :: Editor Text N
                 , _inEditMode    :: Bool
                 , _showingHelp   :: Bool
                 , _lastSavedTree :: Maybe ETree
                 , _minorChanges  :: Bool -- True iff current tree is the same than last saved tree except for collapsed nodes
                 , _rewinder      :: [(Maybe Int, ETree)]
                 } deriving Show

makeClassy ''EState
