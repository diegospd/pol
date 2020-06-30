{-# LANGUAGE OverloadedStrings #-}
module StartApp where

import Prelude hiding (FilePath)
import Types.Base
import Types.ETree
import Types.EState
import Types.Brick as Brick
import Types.AppConfig (Config(..))

import Logic.ETree as ETree
import Logic.Tree as Tree
import Logic.EState as EState
import Logic.TextZipper as Tz
import Logic.Zipper as Zipper
import Adapter.ETree as ETree
import Adapter.Entry as Entry

import App.IO as IO
import App.Draw

import Data.Tree.Zipper as Z
import Data.Tree
import Graphics.Vty hiding (Config)
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Monad.IO.Class(liftIO)

theApp :: Config -> App EState () N
theApp conf@(Config saveFile) = App { appDraw         = myDraw saveFile
                                    , appChooseCursor = showFirstCursor
                                    , appHandleEvent  = handleEv conf
                                    , appStartEvent   = return
                                    , appAttrMap      = theAttrMap
                                    }

runTheApp :: Config -> IO ()
runTheApp conf@(Config saveFile) = do
    fromDisk <- IO.readTree saveFile
    let tree  = fromMaybe ETree.emptyTree fromDisk
    let st    = ETree.toState conf tree & lastSavedTree ?~ tree
    void $ Brick.defaultMain (theApp conf) st

emptyNode :: ETree
emptyNode = Node (Entry.fromText "") []

-----------------------------------------------------------------
--               E v e n t    H a n d l e r
-----------------------------------------------------------------

handleEv :: EState -> BrickEvent N () -> EventM N (Next EState)
handleEv st (VtyEvent (EvKey (KChar 'e') []))
    | not (st^.inEditMode) = continue' st $ editCurrentLine st
handleEv st (VtyEvent (EvKey KEsc []))
    | st^.inEditMode = continue $ cancelEdit st & inEditMode .~ False
    | otherwise      = halt st
handleEv st (VtyEvent (EvKey KEnter []))
    | st^.inEditMode = continue' st $ flushEditor st
    | otherwise      = continue' st $ toggleCollapse st
handleEv st (VtyEvent (EvKey (KChar 'a') []))
    | not (st^.inEditMode) = continue $ addLineHere st & inEditMode .~ True
handleEv st (VtyEvent (EvKey (KChar 'a') [MCtrl]))
    | not (st^.inEditMode) && isNotEmpty (st^.theTree) = continue $ addLineBelow st & inEditMode .~ True
handleEv st (VtyEvent e)
    | st^.inEditMode = continue =<< handleEventLensed st theEditor handleEditorEvent e
handleEv st (VtyEvent e)
    | e `elem` listers =  continue =<< handleEventLensed st theList handleListEvent e

handleEv st (VtyEvent (EvKey (KChar 'd') [MCtrl] )) = continue' st $ dropCurrent st
handleEv st (VtyEvent (EvKey KUp         [MCtrl] )) = continue' st $ dragAbove st
handleEv st (VtyEvent (EvKey KDown       [MCtrl] )) = continue' st $ dragBelow st
handleEv st (VtyEvent (EvKey KLeft       [MCtrl] )) = continue' st $ dragUpperLevel st
handleEv st (VtyEvent (EvKey KRight      [MCtrl] )) = continue' st $ dragLowerLevel st
handleEv st (VtyEvent (EvKey (KChar 'h') []      )) = continue $ st & showingHelp %~ not
handleEv st (VtyEvent (EvKey (KChar 'c') []      )) = continue' st $ collapseAll st
handleEv st (VtyEvent (EvKey (KChar 'p') []      )) = continue $ moveToParent st
handleEv st (VtyEvent (EvKey (KChar 'c') [MCtrl] )) = continue' st $ expandAll st
handleEv st (VtyEvent (EvKey (KChar 's') []      )) = continue' st $ sortEntries st
handleEv st (VtyEvent (EvKey (KChar 'z') [MCtrl] )) = continue $ rewind st
handleEv st (VtyEvent (EvKey (KChar 's') [MCtrl] )) = do
    liftIO (writeChanges undefined st)
    let st' = st & lastSavedTree ?~ st^.theTree
    continue st'

handleEv st _ = continue st


listers :: [Event]
listers = map (\k -> EvKey k []) [KUp, KDown, KHome, KEnd, KPageDown, KPageUp]


-----------------------------------------------------------------
--     M o v i n g    a r o u n d   t h e    t r e e
-----------------------------------------------------------------

-- | Makes some kind of transformation to the tree. Uses a function
-- that takes the index of the selected element in the List and
-- its corresponding Zipper, and Maybe returns the new selected index and
-- a zipper for a new Tree. If the function returns Nothing or there is
-- no selected item in the List the state is not modified. Otherwise
-- it makes a new state out of the zipper that is returned by the function
-- and sets the selected element in the List accordingly.
moveAround :: ((Int, Zipper) -> Maybe (Int, Zipper)) -> EState -> EState
moveAround f st = fromMaybe st $ do
    (n, (e,z)) <- listSelectedElement (st^.theList)
    (n', z')   <- f (n, z)
    return $ EState.zipperToState st z' & theList %~ listMoveTo n'


-----------------------------------------------------------------
--   The following functions only mess with the selected
--   element in the list or modifies the visiblity of some nodes.
--   The tree structure is not altered in any other way.
-----------------------------------------------------------------


-- | Changes the selected item in the list to the parent of the current element.
-- If the selected element is a top level node nothing is changed.
moveToParent :: EState -> EState
moveToParent = moveAround moveToParent'

moveToParent' :: (Int, Zipper) -> Maybe (Int, Zipper)
moveToParent' (n, z)
    | Zipper.isFirstLevelOrRoot z = Nothing
    | otherwise = return (n', z)
    where n' = n - 1 - countNodesBeforeParent z


-- | The tree is fully expanded. All nodes become visible.
expandAll :: EState -> EState
expandAll = moveAround (collapseAll' False)


-- | The tree is fully collapsed. Only the top level nodes remain visible.
collapseAll :: EState -> EState
collapseAll st =  moveAround (collapseAll' True) st & theList %~ listMoveTo 0

collapseAll' :: Bool ->  (Int, Zipper) -> Maybe (Int, Zipper)
collapseAll' col (n, z) = return (n, z')
    where z' =  modifyTree  (fmap (& isCollapsed .~ col)) (root z)


-- | Collapses or expands the selected element.
toggleCollapse :: EState -> EState
toggleCollapse = moveAround toggleCollapse'

toggleCollapse' :: (Int, Zipper) -> Maybe (Int, Zipper)
toggleCollapse' (n, z) = Just (n, modifyLabel (& isCollapsed %~ not) z)


-----------------------------------------------------------------
--     The following functions transforms the tree structure
--        without adding or removing any extra elements.
-----------------------------------------------------------------

-- | Sorts the current level alphabetically
sortEntries :: EState -> EState
sortEntries = moveAround sortEntries'

sortEntries' :: (Int, Zipper) -> Maybe (Int, Zipper)
sortEntries' (n, z) = return (n, z')
  where on t = rootLabel t ^. itsText
        z'   = modifyTree (Tree.applyToForest $ sortOn on) z
        -- z'   = fromTree $ applyToForest (sortOn on) (tree z)


dragSideways :: Direction -> EState -> EState
dragSideways d = moveAround (dragSideways' d)

dragSideways' :: Direction -> (Int, Zipper) -> Maybe (Int, Zipper)
dragSideways' d (n, z) = do
    let (moveT, moveS, f) = case d of
            Up   -> (prevTree, prevSpace, (n-))
            Down -> (nextTree, nextSpace, (n+))
    let t = moveT (Z.delete z)
    n' <- length . flatten . tree <$> t
    z' <- (Z.insert (tree z) . moveS) <$> t
    return (f n', z')


dragBelow :: EState -> EState
dragBelow = moveAround dragBelow'

dragBelow' :: (Int, Zipper) -> Maybe (Int, Zipper)
dragBelow' (n, z) = do
    let t  =  tree z
    t'     <- nextTree $ Z.delete z
    let z' =  nextSpace t'
    let n' =  n + countVisible (tree t')
    return  (n' , Z.insert t z')


dragAbove :: EState -> EState
dragAbove = moveAround dragAbove'


dragAbove' :: (Int, Zipper) -> Maybe (Int, Zipper)
dragAbove' (n, z) = do
    let t  =  tree z
    t'     <- prevTree $ Z.delete z
    let z' =  prevSpace t'
    let n' =  n - countVisible (tree t')
    return  (n' , Z.insert t z')


dragUpperLevel :: EState -> EState
dragUpperLevel = moveAround dragUpperLevel'


dragUpperLevel' :: (Int, Zipper) -> Maybe (Int, Zipper)
dragUpperLevel' (n, z) = do
    guard =<< not . isRoot <$> parent z
    z' <- (Z.insert (tree z) . prevSpace) <$> parent (Z.delete z)
    let i = 1 + countNodesBeforeParent z
    return (n-i, z')


dragLowerLevel :: EState -> EState
dragLowerLevel = moveAround dragLowerLevel'


dragLowerLevel' :: (Int, Zipper) -> Maybe (Int, Zipper)
dragLowerLevel' (n, z) = do
    z' <- parent =<< (Z.insert (tree z) . children <$> nextTree (Z.delete z))
    return (n+1, modifyLabel (& isCollapsed .~ False) z')


-----------------------------------------------------------------
-- The following functions add or remove nodes from the tree.
-----------------------------------------------------------------


addLineHere :: EState -> EState
addLineHere st
    | ETree.isEmpty (st^.theTree) = EState.zipperToState st . snd . fromJust $ addLineBelow' (0, fromTree $ st^.theTree)
    | otherwise = moveAround addLineHere' st

addLineHere' :: (Int, Zipper) -> Maybe (Int, Zipper)
addLineHere' (n, z) = return (n, z')
    where z' = Z.insert emptyNode $ prevSpace z


addLineBelow :: EState -> EState
addLineBelow = moveAround addLineBelow'

addLineBelow' :: (Int, Zipper) -> Maybe (Int, Zipper)
addLineBelow' (n, z) = do
    let z' = modifyLabel (& isCollapsed .~ False) z
    let z'' = Z.insert emptyNode (children z')
    return (n+1, z'')


dropCurrent :: EState -> EState
dropCurrent  = moveAround dropCurrent'

dropCurrent' :: (Int, Zipper) -> Maybe (Int, Zipper)
dropCurrent' (n, z) = Just (n, fromJust . parent $ Z.delete z)


-----------------------------------------------------------------
--           Editing the information in a node
-----------------------------------------------------------------
editCurrentLine :: EState -> EState
editCurrentLine st = st & theEditor  %~ applyEdit replaceOrKeep
                        & inEditMode .~ inEdit
    where line = getCurrentLineList st
          inEdit = not $ T.null line
          replaceOrKeep = if inEdit then Tz.replaceZipper line else id


-- | If the old and new texts are both empty, then the current node
-- is deleted. If only the new text is empty, then the old text is
-- kept. Otherwise the new text replaces the old one.
flushEditor :: EState -> EState
flushEditor st = moveAround (flushEditor' st) st

flushEditor' :: EState -> (Int, Zipper) -> Maybe (Int, Zipper)
flushEditor' st (n, z)
    | all T.null [edText, oldText] = (,) n <$> parent (Z.delete z)
    | T.null edText = return (n, z)
    | otherwise = return (n, modifyLabel (& itsText .~ edText) z)
    where edText = T.strip . head . getEditContents $ st^.theEditor
          oldText = T.strip $ label z ^.itsText


-- | Cancels edit when in edit mode. If the previous text text
-- was empty then the node is deleted. Otherwise the old text
-- is kept.
cancelEdit :: EState -> EState
cancelEdit = moveAround cancelEdit'


cancelEdit' :: (Int, Zipper) -> Maybe (Int, Zipper)
cancelEdit' (n,z)
    | T.null oldText = (,) n <$> parent (Z.delete z)
    | otherwise = return (n, z)
    where oldText = T.strip $ label z ^.itsText


-- | Counts how many visible nodes are between the zipper and its parent.
-- i.e. it counts how many visible nodes are in the subtrees of its previous siblings
countNodesBeforeParent :: Zipper -> Int
countNodesBeforeParent z = sum $ map countVisible $ before z


countVisible :: ETree -> Int
countVisible = length . filter (^.isVisible) . flatten


-- | When the new state is likely to have a different tree than the previous
-- one, this functions records the old tree in the new state rewinder so that
-- the previous state may be recovered later.
continue' :: EState -> EState -> EventM n (Next EState)
continue' old new
    | (old^.theTree) == (new^.theTree) = continue new
    | otherwise = continue $ new & rewinder .~ (old^.theList.listSelectedL, old^.theTree):(old^.rewinder)


-- | If the rewinder is not empty, then it restores the previous state.
rewind :: EState -> EState
rewind st
    | null (st^.rewinder) = st
    | otherwise = let ((mn, prev):rest) = st^.rewinder
                      st' = EState.transition st prev
                  in st' & rewinder .~ rest & theList . listSelectedL .~ mn


-- | Writes the tree to disk
writeChanges :: FilePath -> EState -> IO ()
writeChanges saveFile st = IO.writeTree saveFile (st^.theTree)


getCurrentLineList :: EState -> Text
getCurrentLineList st = fromMaybe ""  $ ((^.itsText) . fst . snd) <$> listSelectedElement (st ^. theList)
