{-# LANGUAGE OverloadedStrings #-}

module AppDraw where

import Utils
import Types


import Brick.Widgets.Core
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
lastSave :: PState -> Widget N
lastSave st 
    | Just (st^.theTree) == st^.lastSavedTree = str "No changes to save"
    | otherwise = str "Save changes with Ctrl-s"
    where last = st^.lastSavedTree



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
    | otherwise = renderList (renderEntry st) True (st^.theList) 

renderEntry :: PState -> Bool -> (Entry, Zipper) -> Widget N
renderEntry _ False (e,_) = padLeft (Pad $ 6 * (e^.itsDepth)) (txt $ e^.itsText)
renderEntry st True (e,_) 
    | st^.inEditMode = padLeft (Pad $ 6 * (e^.itsDepth)) $ renderEditor True (st ^. theEditor)
    | otherwise = padLeft (Pad $ 6 * (e^.itsDepth)) (txt $ e^.itsText)

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
