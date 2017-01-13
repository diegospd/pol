{-# LANGUAGE OverloadedStrings #-}

module AppDraw where

import Utils
import Types


import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

import Lens.Micro.Platform
import Graphics.Vty

debug_flag = False


---------------------- show help ---------------------------
helpUI :: [(Text,Text)]
helpUI = [ ( "Esc"            , "quit no save"  )  
         , ( "C-s"            , "save changes"  )  
         , ( "C-z"            , "undo last action")
         , ( "Enter"          , "toggle collapse")
         , ( "c"              , "collapse all" )
         , ( "C-c"            , "expand all" )
         , ( "C-[↑|↓]"        , "drag up/down"  )
         , ( "C-[←|→]"        , "drag left/right"  )   
         , ( "C-d"            , "delete subtree")     
         , ( "a"              , "add a line here")
         , ( "C-a"            , "add a line in next level")
         , ( "e"              , "edit line"     )
         , ( "p"              , "jump to parent")
         , ( "h"              , "show/hide this")
         ]
         




displayHelp :: Bool -> Widget n
displayHelp True = hCenter $ twoColumns helpUI
displayHelp False = padLeft (Pad 3) $ str offerHelp
    where offerHelp = "Press h to show help"


twoColumns :: [(Text,Text)] -> Widget n 
twoColumns ls = joinH left <+> center <+> joinH right 
    where half = ceiling $  fromIntegral (length ls) / 2
          (left, right) = splitAt half ls
          center = str $ replicate 10 ' '

joinH :: [(Text,Text)] -> Widget n
joinH xs = vBox as <+> vBox bs
    where as = map (txt . fst) xs
          bs = map (\x -> txt $ " - " <> snd x) xs

-----------------------last save------------------------------
hasChanges :: PState -> Bool
hasChanges st  = Just (st^.theTree) /= st^.lastSavedTree

lastSave :: PState -> Widget N
lastSave st 
    | hasChanges st =  str "Save changes with Ctrl-s"
    | otherwise = str "No changes to save"



--------------------------------------------------------------

myDraw :: PState -> [Widget N]
myDraw st = do
    let list  = renderList' st 
    let debug = info st
    let saved = padLeft (Pad 3) . padTopBottom 1 $ lastSave st
    let help  = displayHelp (st^.showingHelp)
    return $ vBox [list, help, saved, debug]


renderList' :: PState -> Widget N
renderList' st 
    | isEmpty (st^.theTree) = center $ str "  Such an empty tree..." <=> str "Press 'a' to add and entry"
    | otherwise = padAll 2  ls
    where ls = myBorder $ renderList (renderEntry st) True (st^.theList) 
          myBorder = if hasChanges st then withBorderStyle unicodeBold . border . border
                                      else withBorderStyle unicode . border . withBorderStyle (borderStyleFromChar ' ') . border

renderEntry :: PState -> Bool -> (Entry, Zipper) -> Widget N
renderEntry _ False (e,_) = padDepth e  (renderEntry' e)
renderEntry st True (e,_) 
    | st^.inEditMode = padDepth e $ renderEditor True (st ^. theEditor)
    | otherwise = padDepth e (renderEntry' e)

padDepth :: Entry -> Widget n -> Widget n
padDepth e = padLeft (Pad $ 4 * (e^.itsDepth))

renderEntry' :: Entry -> Widget n
renderEntry' e = case e^.itsDepth of
    1 -> txt $ "[" <> e^.itsText <> "]"
    2 -> txt $ "- " <> e^.itsText 
    3 -> txt $ "· " <> e^.itsText 
    _ -> txt (e^.itsText)



------------------------debug--------------------------------------


info :: PState -> Widget N
info st = if debug_flag then info' st else emptyWidget

info' :: PState -> Widget N
info' st = case listSelectedElement (st^.theList) of
    Nothing -> emptyWidget
    Just(n,e) -> str (debug_entry e) <=> str ("Edit: " <> show (st ^. inEditMode)) 
    <=> str ("History:" <> show (length $ st^.rewinder))

debug_entry :: (Entry,Zipper) -> String
debug_entry (e,_) = "Collapsed:" ++ show (e^.isCollapsed) ++ "   " ++
                "Visible: " ++ show (e^.isVisible) ++ "   " ++ show (e^.itsText)

--------------------------------------------------------------


theAttrMap _ = attrMap defAttr
              [ (listSelectedAttr,  blue `on`  cyan)]
