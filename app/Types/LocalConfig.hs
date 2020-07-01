{-# LANGUAGE OverloadedStrings #-}

module Types.LocalConfig where

import qualified Data.Map as M
import qualified TCE.Data.KVConf as KVConf
import Types.AppConfig

parseLocalConfig :: String -> Config
parseLocalConfig rawString =
  let conf = KVConf.parseToMap
   in Config
        { saveFile = "aqui.tree"
        , debug = False
        }

loadLocalConfig :: FilePath -> IO Config
loadLocalConfig filename = undefined

main = do
  -- Parse a String into a KVConf datatype
  conf <- KVConf.parseToMap <$> readFile "kv-example.conf"
  -- It's just a map, so use Data.Map functions to access
  print $ M.lookup "foo" conf
  print $ M.lookup "baz-blorp" conf
