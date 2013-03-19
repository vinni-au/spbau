-- task 1
minp :: (Integer -> Bool) -> Integer
minp f = minph f 0
minph f x = if f x 
  then x 
  else if f (-x) 
    then -x 
    else minph f (x+1)

-- testf is used to test minp
testf :: Integer -> Bool
testf x = 
  if x < 0
    then True
    else False

-- task 2a
numlen :: Integer -> Integer
numlen x = if x < 0
  then numlen (-x)
  else if x < 10
    then 1
    else 1 + numlen (div x 10)

-- task 2b
numsum :: Integer -> Integer
numsum x = if x < 0
  then numsum (-x)
  else if x < 10
    then x
    else mod x 10 + numsum (div x 10)

-- task 3
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (mod a b)

-- task 4
fib :: Integer -> Integer
fib' :: Integer -> (Integer , Integer)
fib n = if n <= 0
  then 0
  else fst (fib' n)
fib' 1 = (1, 1)
fib' n = (snd (fib' (n-1)), (fst (fib' (n-1))) + (snd (fib' (n-1))))

-- task 5
integral :: (Double -> Double) -> Double -> Double -> Double
integralh :: (Double -> Double) -> Double -> Double -> Integer -> Double
sumh :: (Double -> Double) -> Double -> Double -> Double -> Double
integral f a b = integralh f a b 100
integralh f a b n =
  ((sumh f a b h) + t)*h
  where
    t = (f a + f b) / 2
    h = (b - a) / (fromInteger n)
sumh f a b h = sum $ map f [a+h, a+2*h .. b-h]

-- func is used to test integral
func x = x * sin x
