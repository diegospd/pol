module Logic.TextZipper where

import Types.Base
import Prelude hiding (FilePath)
import Data.Text.Zipper as Tz

replaceZipper :: Text -> b -> TextZipper Text
replaceZipper t  = const $ Tz.textZipper [t] (Just 1)
