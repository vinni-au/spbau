{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Test.HUnit

import StateIO

-- Эти instance можно реализовать, используя реализацию MonadState
instance MonadReader s (StateIO s) where
    ask = get
    local f m = do
      res <- get
      modify f
      m1 <- m
      put res
      return m1

instance Monoid s => MonadWriter s (StateIO s) where
    tell w = get >>= \x -> put $ mappend x w
    listen m = m >>= \x -> get >>= \y -> return (x, y)
    pass m = do
      (a, b) <- m
      modify b
      return a

-- tests

reverse' :: [a] -> StateIO [a] [a]
reverse' xs = put [] >> rev xs >> get
  where
    rev :: [a] -> StateIO [a] ()
    rev [] = return ()
    rev (x:xs) = modify (x:) >> rev xs

testReader :: StateIO Int Int
testReader = do
    x <- ask
    y <- local (+1) ask
    z <- ask
    return (x + y + z)

testWriter :: Int -> StateIO String ()
testWriter 0 = tell "0" >> return ()
testWriter n = do
    tell (show n ++ " ")
    testWriter (n - 1)
    tell (" " ++ show n)

main = fmap (const ()) $ runTestTT $ test
    $    label "MonadState"
    [ TestCase $ liftM (== reverse [1..100]) (runStateIO (reverse' [1..100]) undefined)
        @? "reverse' [1..100] shoud be equal to [100..1]"
    ] ++ label "MonadReader"
    [ TestCase $ let p = 7; r = 3 * p + 1 in liftM (== r) (runStateIO testReader p)
        @? "testReader " ++ show p ++ " shoud be equal to " ++ show r
    ] ++ label "MonadWriter"
    [ TestCase $ let p = 3; r = "3 2 1 0 1 2 3" in liftM (== r) (runStateIO (testWriter p >> get) "")
        @? "testWriter " ++ show p ++ " shoud be equal to " ++ show r
    ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
