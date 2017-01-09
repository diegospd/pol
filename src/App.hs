{-# LANGUAGE OverloadedStrings #-}

module App where

import Utils
import Types
import AppIO
import AppDraw

import Lens.Micro.Platform
import Data.Tree.Zipper as Z

import Graphics.Vty
import qualified Data.Vector as V
import qualified Data.Text as T

import Control.Monad.IO.Class(liftIO)





theApp :: App PState () N
theApp  = App { appDraw = myDraw
              , appChooseCursor = showFirstCursor
              , appHandleEvent = handleEv
              , appStartEvent = return
              , appAttrMap = theAttrMap
              }

runTheApp :: IO ()
runTheApp = do
    fromDisk <- readTree
    let tree = fromMaybe emptyTree fromDisk
    let st = toState tree & lastSavedTree .~ Just tree
    void $ defaultMain theApp st 

-----------------------------------------------------------------
-----------------------------------------------------------------

handleEv :: PState -> BrickEvent N () -> EventM N (Next PState)
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
    | not (st^.inEditMode) = continue $ addLineBelow st & inEditMode .~ True
handleEv st (VtyEvent e) 
    | st^.inEditMode = continue =<< handleEventLensed st theEditor handleEditorEvent e 
handleEv st (VtyEvent e) 
    | e `elem` listers =  continue =<< handleEventLensed st theList handleListEvent e 

handleEv st (VtyEvent (EvKey (KChar 'd') [MCtrl] )) = continue' st $ dropCurrent st
handleEv st (VtyEvent (EvKey KUp         [MCtrl] )) = continue' st $ dragSideways Up st   
handleEv st (VtyEvent (EvKey KDown       [MCtrl] )) = continue' st $ dragSideways Down st
handleEv st (VtyEvent (EvKey KLeft       [MCtrl] )) = continue' st $ dragUpperLevel st 
handleEv st (VtyEvent (EvKey KRight      [MCtrl] )) = continue' st $ dragLowerLevel st 
handleEv st (VtyEvent (EvKey (KChar 'h') []      )) = continue $ st & showingHelp %~ not
handleEv st (VtyEvent (EvKey (KChar 'c') []      )) = continue' st $ collapseAll st
handleEv st (VtyEvent (EvKey (KChar 'c') [MCtrl]))  = continue' st $ expandAll st
handleEv st (VtyEvent (EvKey (KChar 'z') [MCtrl]))  = continue $ rewind st
handleEv st (VtyEvent (EvKey (KChar 's') [MCtrl]))  = do 
    liftIO (writeChanges st) 
    let st' = st & lastSavedTree .~ Just (st^.theTree) 
    continue st'

handleEv st _ = continue st



listers :: [Event]
listers = map (\k -> EvKey k []) [KUp, KDown, KHome, KEnd, KPageDown, KPageUp]

-----------------------------------------------------------------

continue' :: PState -> PState -> EventM n (Next PState)
continue' old new 
    | (old^.theTree) == (new^.theTree) = continue new
    | otherwise = continue $ new & rewinder .~ (old^.theList.listSelectedL, old^.theTree):(old^.rewinder)

moveAround :: ((Int, Zipper) -> Maybe (Int, Zipper)) -> PState -> PState
moveAround f st = fromMaybe st $ do
    (n, (e,z)) <- listSelectedElement (st^.theList)
    (n', z') <- f (n, z)
    return $ zipperToState st z' & theList %~ listMoveTo n'



rewind :: PState -> PState
rewind st 
    | null (st^.rewinder) = st
    | otherwise = let ((mn, prev):rest) = st^.rewinder
                      st' = setPreviousFlags st . toState . fixTree $ prev
                  in st' & rewinder .~ rest & theList . listSelectedL .~ mn

writeChanges :: PState -> IO ()
writeChanges st = writeTree (st^.theTree)


addLineHere :: PState -> PState
addLineHere st 
    | isEmpty (st^.theTree) = zipperToState st . snd . fromJust $ addLineBelow' (0, fromTree $ st^.theTree)
    | otherwise = moveAround addLineHere' st

addLineHere' :: (Int, Zipper) -> Maybe (Int, Zipper)
addLineHere' (n, z) = return (n, z')
    where z' = Z.insert emptyNode $ prevSpace z


addLineBelow :: PState -> PState
addLineBelow = moveAround addLineBelow'

addLineBelow' :: (Int, Zipper) -> Maybe (Int, Zipper)
addLineBelow' (n, z) = do
    let z' = modifyLabel (& isCollapsed .~ False) z
    let z'' = Z.insert emptyNode (children z')
    return (n+1, z'')

flushEditor :: PState -> PState
flushEditor st = moveAround (flushEditor' st) st

flushEditor' :: PState -> (Int, Zipper) -> Maybe (Int, Zipper)
flushEditor' st (n, z) 
    | all T.null [edText, oldText] = (,) n <$> parent (Z.delete z)
    | T.null edText = return (n, z)
    | otherwise = return (n, modifyLabel (& itsText .~ edText) z)
    where edText = T.strip . head . getEditContents $ st^.theEditor
          oldText = T.strip $ label z ^.itsText 


cancelEdit :: PState -> PState
cancelEdit = moveAround cancelEdit'


cancelEdit' :: (Int, Zipper) -> Maybe (Int, Zipper)
cancelEdit' (n,z) 
    | T.null oldText = (,) n <$> parent (Z.delete z)
    | otherwise = return (n, z)
    where oldText = T.strip $ label z ^.itsText


expandAll :: PState -> PState
expandAll = moveAround (collapseAll' False)

collapseAll :: PState -> PState
collapseAll st =  moveAround (collapseAll' True) st & theList %~ listMoveTo 0

collapseAll' :: Bool ->  (Int, Zipper) -> Maybe (Int, Zipper)
collapseAll' col (n, z) = return (n, z')
    where z' =  modifyTree  (fmap (& isCollapsed .~ col)) (root z)



dropCurrent :: PState -> PState
dropCurrent  = moveAround dropCurrent'

dropCurrent' :: (Int, Zipper) -> Maybe (Int, Zipper)
dropCurrent' (n, z) = Just (n, fromJust . parent $ Z.delete z)



toggleCollapse :: PState -> PState
toggleCollapse = moveAround toggleCollapse'

toggleCollapse' :: (Int, Zipper) -> Maybe (Int, Zipper)
toggleCollapse' (n, z) = Just (n, modifyLabel (& isCollapsed %~ not) z)




dragSideways :: Direction -> PState -> PState
dragSideways d = moveAround (dragSideways' d)

dragSideways' :: Direction -> (Int, Zipper) -> Maybe (Int, Zipper)
dragSideways' d (n, z) = do
    let (moveT, moveS, n') = case d of 
            Up   -> (prevTree, prevSpace, n-1)
            Down -> (nextTree, nextSpace, n+1)
    z' <- (Z.insert (tree z) . moveS) <$> moveT (Z.delete z) 
    return (n', z')


dragUpperLevel :: PState -> PState
dragUpperLevel = moveAround dragUpperLevel'

dragUpperLevel' :: (Int, Zipper) -> Maybe (Int, Zipper)
dragUpperLevel' (n, z) = do
    guard =<< not . isRoot <$> parent z
    z' <- (Z.insert (tree z) . prevSpace) <$> parent (Z.delete z)
    let i = 1 + length (before z)
    return (n-i, z')
 



dragLowerLevel :: PState -> PState
dragLowerLevel = moveAround dragLowerLevel'

dragLowerLevel' :: (Int, Zipper) -> Maybe (Int, Zipper)
dragLowerLevel' (n, z) = do
    z' <- parent =<< (Z.insert (tree z) . children <$> nextTree (Z.delete z)) 
    return (n+1, modifyLabel (& isCollapsed .~ False) z')




editCurrentLine :: PState -> PState
editCurrentLine st = st & theEditor  %~ applyEdit replaceOrKeep
                        & inEditMode .~ inEdit
    where line = getCurrentLineList st
          inEdit = not $ T.null line
          replaceOrKeep = if inEdit then replaceZipper line else id


          
getCurrentLineList :: PState -> Text
getCurrentLineList st = fromMaybe ""  $ ((^.itsText) . fst . snd) <$> listSelectedElement (st ^. theList)




-----------------------------------------------------------------
-----------------------------------------------------------------











