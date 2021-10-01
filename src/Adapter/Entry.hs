module Adapter.Entry where

import Types.ETree
import Types.Base
import Prelude hiding (FilePath)


-- | Makes an Entry out of some Text.
fromText :: Text -> Entry
fromText x = En { _itsText     = x
                , _isCollapsed = True
                , _isVisible   = False
                , _itsDepth    = -1
                }
