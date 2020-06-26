module Types.Brick
  (
    N (..),
    module X
  )
where

import Brick as X
import Brick.Widgets.Center as X
import Brick.Widgets.Edit as X
import Brick.Widgets.List as X
import Data.Text(Text(..))

type N = Text
