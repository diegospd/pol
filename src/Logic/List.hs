{-# LANGUAGE OverloadedStrings #-}
module Logic.List
  ( (!!!),
    module X,
  )
where

import Data.List as X

(!!!) :: [a] -> Int -> Maybe a
(!!!) xs n
  | length xs <= n = Nothing
  | otherwise = Just $ xs !! n
