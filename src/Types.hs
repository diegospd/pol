{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


module Types (
      ETree -- (..)
    , Zipper(..)
    , TreePos(..)
    , Empty(..)
    , Full(..)
    , Entry(..)
    , HasEntry(..)
    , N
    , PState(..)
    , HasPState(..)

    , emptyTree
    , emptyNode
    , isEmpty
    , isNotEmpty
    , isFirstLevelOrRoot
    , toState
    , zipperToState
    , treeToState
    , textTreeToETree
    )
where 

import Utils

import GHC.Generics

import Lens.Micro.Platform
import qualified Data.Vector as V
import Data.Tree
import Data.Tree.Zipper
import Data.Aeson(FromJSON, ToJSON)
import qualified Data.Text as T



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

instance ToJSON Entry
instance FromJSON Entry

type N = Text
data PState = St { _theTree       :: ETree
                 , _theList       :: List N (Entry, Zipper)
                 , _theEditor     :: Editor Text N
                 , _inEditMode    :: Bool
                 , _showingHelp   :: Bool
                 , _lastSavedTree :: Maybe ETree
                 , _minorChanges  :: Bool -- True iff current tree is the same than last saved tree except for collapsed nodes
                 , _rewinder      :: [(Maybe Int, ETree)] 
                 } deriving Show



makeClassy ''Entry
makeClassy ''PState




---------------------- th-splice ---------------------------


emptyTree :: ETree
emptyTree = fixTree $ Node (textToEntry "root") []


emptyNode :: ETree
emptyNode = Node (textToEntry "") []


-------------------- predicates ------------------------


isEmpty :: ETree -> Bool
isEmpty (Node _ [])    = True
isEmpty (Node _ (_:_)) = False


isNotEmpty :: ETree -> Bool
isNotEmpty = not . isEmpty


isFirstLevelOrRoot :: Zipper -> Bool
isFirstLevelOrRoot z = maybe True isRoot (parent z)






---------------------------------------------------------

-- | Makes a PState out of an ETree. Should be useful when
-- reading an ETree from disk.
toState :: ETree -> PState
toState t = St { _theTree        = t
                , _theList       = toList t
                , _theEditor     = editorText "theEditor" (txt . T.concat) (Just 1) ""
                , _inEditMode    = False
                , _showingHelp   = isEmpty t
                , _lastSavedTree = Nothing
                , _minorChanges  = True
                , _rewinder      = []
                }


-- | Makes a new state ouf of an old state and a new ETree. 
treeToState :: PState -> ETree -> PState
treeToState old = setPreviousFlags old . toState . fixTree

-- | Makes a new state out of an old state and a Zipper for the
-- new ETree.
zipperToState :: PState -> Zipper -> PState
zipperToState old = treeToState old . toTree


-- | Says if two trees are equal based only on their structure and their texts.
eqByText :: ETree -> ETree -> Bool
eqByText t1 t2 = (f <$> t1) == (f <$> t2)
    where f = (^.itsText)


withMinorChanges :: PState -> PState -> Bool
withMinorChanges old new = case old^.lastSavedTree of
    Nothing -> False
    Just t1 -> eqByText t1 (new^.theTree)

-- | Carries the flags from the old state to the new one/
setPreviousFlags :: PState -> PState -> PState
setPreviousFlags old new = new & showingHelp   .~ (old^.showingHelp)
                               & lastSavedTree .~ (old^.lastSavedTree)
                               & minorChanges  .~ withMinorChanges old new

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
    where es = V.fromList $ filter ((^.isVisible) . fst) $ flatten $ toETreeL t
    -- where es = V.fromList $ {-filter (^.isVisible) $-} flatten $ toETreeL t


toETreeL :: ETree -> Tree (Entry, Zipper)
toETreeL t = setZippers (fromTree t) t  


setZippers :: Zipper -> ETree -> Tree (Entry, Zipper)
setZippers z (Node e ts) = Node (e, z) (zipWith setZippers zs ts)
    where zs = [fromJust $ childAt (n-1) z | n <- [1 .. length ts] ]



---------------

