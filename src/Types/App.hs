module Types.App where

import qualified Types.Brick as B
import Types.Base
import Types.AppConfig(Config(..))
--
--import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
--import Control.Monad.Trans.State.Lazy
--import Control.Monad.Fail (MonadFail)
import Control.Monad.Trans.Reader
--import Control.Monad.IO.Class
--
--
--newtype GuiM a =
--    GuiM { runGui :: ReaderT Config (B.EventM Text a)
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


{-# LANGUAGE DeriveFunctor #-}
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Data.Functor.Identity

newtype GuiM a = GuiM {runGuiM :: Config -> ReaderT Config Identity a}

--runReader :: GuiM a -> Config -> a
--runReader guiM config = (runIdentity . runReaderT guiM) config

askConfig :: Monad m => ReaderT r m r
askConfig = undefined

main :: IO ()
main = runReaderT main' "Hello World"

main' :: ReaderT String IO ()
main' = do
  lift $ putStrLn "I'm going to tell you a message"
  liftIO $ putStrLn "The message is:"
  message <- ask
  lift $ putStrLn message
