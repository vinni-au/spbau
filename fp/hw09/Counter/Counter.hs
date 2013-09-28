module Counter
    ( Counter
    , tick
    , runCounter
    ) where

-- Монада Counter считает количество тиков, т.е. вызовов функции tick
data Counter a = Counter Int a

-- Возвращает результат вычислений и количество тиков
runCounter :: Counter a -> (a, Int)
runCounter (Counter t v) = (v, t)

instance Monad Counter where
    return a = Counter 0 a
    (>>=) (Counter n a) f = let (Counter c d) = f a in
      Counter (c+n) d

tick :: Counter ()
tick = Counter 1 ()
