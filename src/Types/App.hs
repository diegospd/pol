module Types.App where

import qualified Types.Brick as B
import Types.AppConfig(Config(..))

import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class


--newtype GuiM a =
--    GuiM { runGui :: ReaderT Config (runEventM $ B.EventM B.N a)
--           }
--           deriving ( Functor
--                    , Applicative
--                    , Monad
--                    , MonadIO
--                    , MonadThrow
--                    , MonadCatch
--                    , MonadMask
--                    , MonadFail
--                    )