{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Types.App where

--
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
--import Control.Monad.Trans.State.Lazy
import Control.Monad.Fail (MonadFail)

--import Control.Monad.IO.Class

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Types.AppConfig (Config (..))
import Types.Base
import qualified Types.Brick as B

newtype GuiM a
  = GuiM
      { runGuiM :: Config -> ReaderT Config (B.EventM Text) a
      }
  deriving (Functor)



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
