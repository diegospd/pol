{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}


module Types where


import Utils

import GHC.Generics

import Lens.Micro.Platform
import qualified Data.Vector as V
import Data.Tree
import Data.Tree.Zipper
import Data.Aeson
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
                 , _rewinder      :: [(Maybe Int, ETree)] 
                 } deriving Show



makeLenses ''Entry
makeLenses ''PState



---------------------- splice ---------------------------


emptyTree :: ETree
emptyTree = fixTree $ Node (textToEntry "root") []

emptyNode :: ETree
emptyNode = Node (textToEntry "") []

isEmpty :: ETree -> Bool
isEmpty (Node _ [])    = True
isEmpty (Node _ (_:_)) = False

textTreeToETree :: Tree Text -> ETree
textTreeToETree = fixTree . fmap textToEntry

fixTree :: ETree -> ETree
fixTree = setDepths 0 . setVisibilities . rootIsNeitherCollapsedNorVisible

textToEntry :: Text -> Entry
textToEntry x = En { _itsText     = x
                   , _isCollapsed = True
                   , _isVisible   = False
                   , _itsDepth    = -1
                   } 


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



setPreviousFlags :: PState -> PState -> PState
setPreviousFlags old new = new & showingHelp .~ (old^.showingHelp)
                               & lastSavedTree .~ (old^.lastSavedTree)

toState :: ETree -> PState
toState t = St { _theTree        = t
                , _theList       = toList t
                , _theEditor     = editorText "theEditor" (txt . T.concat) (Just 1) ""
                , _inEditMode    = False
                , _showingHelp   = isEmpty t
                , _lastSavedTree = Nothing
                , _rewinder      = []
                }



zipperToState :: PState -> Zipper -> PState
zipperToState old = setPreviousFlags old . toState . fixTree . toTree

