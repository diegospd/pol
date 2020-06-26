module Logic.TextZipper where

import Prelude hiding (FilePath)
import Types.Base
import Data.Text.Zipper as Tz


replaceZipper :: Text -> b -> TextZipper Text
replaceZipper t = const $ Tz.textZipper [t] (Just 1)
