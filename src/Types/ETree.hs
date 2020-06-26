{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Types.ETree (
      ETree(..)
    , Zipper(..)
    , Empty(..)
    , Full(..)
    , Entry(..)
    , HasEntry(..)
    , T.Tree(..)
    )
where
import Data.Tree.Zipper (Full (..), Empty (..))

import Prelude hiding (FilePath)
import Types.Base
import Data.Tree as T
import GHC.Generics
import Data.Aeson(FromJSON, ToJSON)

-- | The main Tree for the state.
type ETree  = T.Tree Entry

-- | Intermediate Tree for building a List.
type ETreeL = Tree (Entry, Zipper)

-- | A zipper for an ETree
type Zipper = TreePos Full Entry

data Entry = En { _itsText     :: Text
                , _isCollapsed :: Bool
                , _isVisible   :: Bool
                , _itsDepth    :: Int
                } deriving (Generic, Eq, Show)

instance FromJSON Entry
instance ToJSON   Entry
makeClassy      ''Entry
