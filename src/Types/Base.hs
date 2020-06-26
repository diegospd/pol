{-# LANGUAGE OverloadedStrings #-}
module Types.Base
  ( Text (..),
    Vector (..),
    Tree (..),
    TreePos (..),
    FilePath(..),
    module X,
  )
where

------------ exports --------------

import Control.Monad as X
import Data.Char as X
import Data.Default as X
import Data.Either as X
import Data.List as X
import Data.Maybe as X
import Data.Monoid as X
import Lens.Micro.Platform as X
import Prelude hiding (FilePath)
import Turtle (FilePath(..))
import Data.Text (Text (..))
import Data.Tree
import Data.Tree.Zipper (TreePos (..))
import qualified Data.Vector as V
import Data.Vector (Vector (..))

