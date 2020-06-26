module Logic.EState where

import Prelude hiding (FilePath)
import Types.Base
import Types.ETree
import Types.EState

import qualified Logic.ETree as ETree
import qualified Data.Tree.Zipper as Z


-- | Carries the flags from the old state to the new one/
setPreviousFlags :: EState -> EState -> EState
setPreviousFlags old new = new & showingHelp   .~ (old^.showingHelp)
                               & lastSavedTree .~ (old^.lastSavedTree)
                               & minorChanges  .~ withMinorChanges old new


-- | Makes a new state out of an old state and a new ETree.
transition :: EState -> ETree -> EState
transition old = setPreviousFlags old . ETree.toState . ETree.reveal

withMinorChanges :: EState -> EState -> Bool
withMinorChanges old new = case old^.lastSavedTree of
    Nothing -> False
    Just t1 -> ETree.equalByText t1 (new^.theTree)

-- | Makes a new state out of an old state and a Zipper for the
-- new ETree.
zipperToState :: EState -> Zipper -> EState
zipperToState old = transition old . Z.toTree