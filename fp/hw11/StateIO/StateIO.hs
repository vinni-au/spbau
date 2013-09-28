{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateIO
    ( StateIO
    , runStateIO
    ) where

import Control.Monad.State
import Data.IORef

newtype StateIO s a = StateIO { evalStateIO :: IORef s -> IO a }

instance Monad (StateIO s) where
  return a = StateIO $ \state -> return a
  StateIO m >>= k = StateIO $ \state -> do
    a <- m state
    evalStateIO (k a) state

instance MonadState s (StateIO s) where
  get = StateIO $ \x -> readIORef x
  put s = StateIO $ \x -> writeIORef x s

runStateIO :: StateIO s a -> s -> IO a
runStateIO (StateIO f) s = do
  res <- newIORef s
  f res

