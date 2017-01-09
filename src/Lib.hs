{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Utils
import Types
import App

someFunc :: IO ()
someFunc = runTheApp
-- someFunc = runMyApp


runMyApp :: IO ()
runMyApp = void $ defaultMain theApp (mkState aTree)


mkState :: Tree Text -> PState
mkState = toState .  textTreeToETree


aTree :: Tree Text
aTree = Node "root" [ Node "first line"  
                          [ Node "1a" []
                          , Node "2a" []
                          , Node "3a" []
                          ]
                    , Node "second line" 
                           [ Node "1b" []
                           , Node "2b" []
                           , Node "3b (｡◕‿◕｡)" []
                           ]
                    , Node "third line" 
                           [ Node "1c" []
                           , Node "2c" []
                           , Node "3c" []
                           , Node "4c" []
                           ]
                    , Node "fourth line"  
                          [ Node "1d" []
                          , Node "2d" []
                          , Node "3d" []
                          , Node "4d" []
                          ]
                    ]


