{-# LANGUAGE OverloadedStrings #-}
module IO.LocalConfig (loadLocalConfig)
where

import Prelude hiding(FilePath)
import Types.Base
import qualified Data.Map as M
import qualified TCE.Data.KVConf as KVConf
import Types.AppConfig
import Types.LocalConfig as Local
import qualified Turtle as Sh
import qualified Data.Text as T
import Data.Maybe
import Adapter.Config (localConfig)

withSaveFile :: KVConf.KVConf ->  Maybe FilePath
withSaveFile conf = Nothing

parseLocalConfig :: String -> LocalConfig
parseLocalConfig rawString =
  let conf = KVConf.parseToMap rawString
   in LocalConfig
        { Local.saveFile = withSaveFile conf
        , Local.verbose  = undefined
        }

loadLocalConfig :: FilePath -> IO (Maybe LocalConfig)
loadLocalConfig filepath = do
  putStrLn $ "loading local config from " <> show filepath
  hasLocalConfig <- Sh.testfile filepath
  if hasLocalConfig
     then  Just <$> parseLocalConfig <$> T.unpack <$> Sh.readTextFile filepath
     else  return Nothing
