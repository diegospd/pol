module Adapter.Entry where

import Types.ETree
import Types.Base

-- | Makes an Entry out of some Text.
textToEntry :: Text -> Entry
textToEntry x = En { _itsText     = x
                   , _isCollapsed = True
                   , _isVisible   = False
                   , _itsDepth    = -1
                   }