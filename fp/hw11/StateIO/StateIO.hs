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
    return a = StateIO undefined
    StateIO m >>= k = StateIO undefined

instance MonadState s (StateIO s) where
    get = StateIO undefined
    put s = StateIO undefined

runStateIO :: StateIO s a -> s -> IO a
runStateIO (StateIO f) s = undefined
