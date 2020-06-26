{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.EState
  (EState(..)
  , HasEState(..)
  )
where

import GHC.Generics
import Types.Brick (Editor (..), List (..), N (..))
import Types.ETree
import Types.Base
import Prelude hiding (FilePath)

data EState
  = St
      { _theTree       :: ETree,
        _theList       :: List N (Entry, Zipper),
        _theEditor     :: Editor Text N,
        _inEditMode    :: Bool,
        _showingHelp   :: Bool,
        _lastSavedTree :: Maybe ETree,
        _minorChanges  :: Bool, -- True iff current tree is the same than last saved tree except for collapsed nodes
        _rewinder      :: [(Maybe Int, ETree)]
      }
  deriving (Show)

makeClassy ''EState
