{-# LANGUAGE OverloadedStrings #-}
module App.Draw (myDraw, theAttrMap) where

import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Core
import qualified Data.Text as T
import Data.Tree
import Data.Tree.Zipper as Z
import Graphics.Vty

import Logic.List as L
import Logic.ETree as ETree

import Types.Base
import Prelude hiding (FilePath)
import Types.ETree
import Types.Brick as Brick
import Types.EState

debug_flag = False

---------------------- show help ---------------------------
helpUI :: [(Text, Text)]
helpUI =
  [ ("Esc", "quit no save"),
    ("C-s", "save changes"),
    ("C-z", "undo last action"),
    ("Enter", "toggle collapse"),
    ("c", "collapse all"),
    ("C-c", "expand all"),
    ("C-[↑|↓]", "drag up/down"),
    ("C-[←|→]", "drag left/right"),
    ("C-d", "delete subtree"),
    ("a", "add a line here"),
    ("C-a", "add a line in next level"),
    ("e", "edit line"),
    ("p", "jump to parent"),
    ("s", "sort children"),
    ("h", "show/hide this")
  ]

displayHelp :: Bool -> Widget n
displayHelp True = hCenter $ twoColumns helpUI
displayHelp False = padLeft (Pad 3) $ Brick.str offerHelp
  where
    offerHelp = "Press h to show help"

twoColumns :: [(Text, Text)] -> Widget n
twoColumns ls = joinH left <+> center <+> joinH right
  where
    half = ceiling $ fromIntegral (length ls) / 2
    (left, right) = L.splitAt half ls
    center = str $ replicate 10 ' '

joinH :: [(Text, Text)] -> Widget n
joinH xs = vBox as <+> vBox bs
  where
    as = map (txt . fst) xs
    bs = map (\x -> txt $ " - " <> snd x) xs

-----------------------last save------------------------------

hasChanges :: EState -> Bool
hasChanges st = Just (st ^. theTree) /= st ^. lastSavedTree

lastSave :: FilePath -> EState -> Widget N
lastSave saveFile st
  | hasChanges st = Brick.str $ "Save changes with Ctrl-s  " <> show saveFile
  | otherwise     = Brick.str "No changes to save"

--------------------------------------------------------------

myDraw :: FilePath -> EState -> [Widget N]
myDraw saveFile st = do
  let list  = drawList st
  let debug = drawInfo st
  let saved = padLeft (Pad 3) . padTopBottom 1 $ lastSave saveFile st
  let help  = displayHelp (st ^. showingHelp)
  return $ vBox [list, help, saved, debug]

----------------------------------------------------------

drawDepth :: (Entry, Zipper) -> Widget n
drawDepth (e, z)
  | isLast z && (e ^. itsDepth) > 1 = hBox $ replicate (e ^. itsDepth - 2) step ++ [last]
  | otherwise = hBox $ replicate (e ^. itsDepth - 1) step
  where
    step = str "  │  "
    last = str "  └──"

drawText :: Entry -> Widget n
drawText e = case e ^. itsDepth of
  1 -> txt $ "[" <> e ^. itsText <> "]"
  _ -> txt (e ^. itsText)

drawBullet :: (Entry, Zipper) -> Widget n
drawBullet (e, z)
  | e ^. isCollapsed && hasChildren z = str "  + "
  | hasChildren z = str "  ♦ "
  | otherwise = str "  · "

drawList :: EState -> Widget N
drawList st
  | ETree.isEmpty (st ^. theTree) = center $ str "  Such an empty tree..." <=> str "Press 'a' to add and entry"
  | otherwise = padAll 2 ls
  where
    ls = drawBorder st $ renderList (renderEntry st) True (st ^. theList)

drawBorder :: EState -> Widget n -> Widget n
drawBorder st = withBorderStyle b1 . border . withBorderStyle b2 . border
  where
    (b1, b2) = drawBorder' st

drawBorder' :: EState -> (BorderStyle, BorderStyle)
drawBorder' st
  | hasChanges st && st ^. minorChanges = (unicodeBold, borderStyleFromChar ' ')
  | hasChanges st = (unicodeBold, unicodeBold)
  | otherwise = (unicode, borderStyleFromChar ' ')

drawDecoration :: (Entry, Zipper) -> Widget n
drawDecoration (e, z) = drawDepth (e, z) <+> drawBullet (e, z)

renderEntry :: EState -> Bool -> (Entry, Zipper) -> Widget N
renderEntry _ False (e, z) = drawDecoration (e, z) <+> drawText e
renderEntry st True (e, z)
  | st ^. inEditMode = drawDecoration (e, z) <+> renderEditor (txt . T.concat) True (st ^. theEditor)
  | otherwise = drawDecoration (e, z) <+> drawText e

-- | Looks ugly; won't use for now.
tagWithNumOfSons :: Zipper -> Widget n
tagWithNumOfSons z = str $ "  [" <> fstLvl <> "|" <> total <> "]"
  where
    fstLvl = maybe "0" (show . length) ((!!! 1) . levels . tree $ z)
    total = show . subtract 1 . length . flatten . tree $ z

------------------------debug--------------------------------------

drawInfo :: EState -> Widget N
drawInfo st = if debug_flag then info' st else emptyWidget

info' :: EState -> Widget N
info' st =
  case listSelectedElement (st ^. theList) of
    Nothing -> emptyWidget
    Just (n, e) -> str (debug_entry e) <=> str ("Edit: " <> show (st ^. inEditMode))
    <=> str ("History:" <> show (length $ st ^. rewinder))
    <=> str ("Minor changes: " <> show (st ^. minorChanges))

debug_entry :: (Entry, Zipper) -> String
debug_entry (e, _) =
  "Collapsed:" ++ show (e ^. isCollapsed) ++ "   "
    ++ "Visible: "
    ++ show (e ^. isVisible)
    ++ "   "
    ++ show (e ^. itsText)

--------------------------------------------------------------

theAttrMap _ =
  attrMap
    defAttr
    [(listSelectedAttr, blue `on` cyan)]
