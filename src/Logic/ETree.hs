{-# LANGUAGE OverloadedStrings #-}
module Logic.ETree where

import Types.ETree

isEmpty :: ETree -> Bool
isEmpty (Node _ [])    = True
isEmpty (Node _ (_:_)) = False

isNotEmpty :: ETree -> Bool
isNotEmpty = not . isEmpty
