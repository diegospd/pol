{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.EState
  ( EState(..)
  , HasEState(..)
  )
where

import Prelude hiding (FilePath)
import Types.Base
import GHC.Generics
import qualified Types.Brick as B
import Types.ETree


data EState
  = St
      { _theTree       :: ETree,
        _theList       :: B.List B.N (Entry, Zipper),
        _theEditor     :: B.Editor Text B.N,
        _inEditMode    :: Bool,
        _showingHelp   :: Bool,
        _lastSavedTree :: Maybe ETree,
        _minorChanges  :: Bool, -- True iff current tree is the same than last saved tree except for collapsed nodes
        _rewinder      :: [(Maybe Int, ETree)],
        _saveFile      :: FilePath
      }
  deriving (Show)

makeClassy ''EState
