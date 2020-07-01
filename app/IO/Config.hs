{-# LANGUAGE OverloadedStrings #-}
module IO.Config (loadLocalConfig)
where

import Prelude hiding(FilePath)
import Types.Base
import qualified Data.Map as M
import qualified TCE.Data.KVConf as KVConf
import Types.AppConfig
import qualified Turtle as Sh
import qualified Data.Text as T
import Data.Maybe
import Adapter.Config (localConfig)

withSaveFile :: KVConf.KVConf ->  Maybe FilePath
withSaveFile conf = Nothing

parseLocalConfig :: FilePath -> String -> Config
parseLocalConfig homeDir rawString =
  let conf = KVConf.parseToMap rawString
   in Config
        { saveFile = fromMaybe (localConfig homeDir) (withSaveFile conf)
        , debug = False
        }

loadLocalConfig :: FilePath -> IO Config
loadLocalConfig filename = do
  homeDir <- Sh.home
  parseLocalConfig homeDir <$> T.unpack <$> Sh.readTextFile filename
