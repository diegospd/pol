{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Types.ETree (
      ETree -- (..)
    , Zipper(..)
    , Empty(..)
    , Full(..)
    , Entry(..)
    , HasEntry(..)
    , module X
    )
where
import Data.Tree.Zipper (Full (..), Empty (..))
import Types.Base as X
import Types.Base
import GHC.Generics
import Lens.Micro.Platform
import Data.Aeson(FromJSON, ToJSON)

-- | The main Tree for the state.
type ETree  = Tree Entry

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
