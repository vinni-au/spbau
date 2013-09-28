import Counter

filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' _ [] = do
    tick
    return []

filter' p (x:xs) 
  | p x = do
      tick
      l <- filter' p xs
      append [x] l
  | otherwise = do
      tick
      filter' p xs

append :: [a] -> [a] -> Counter [a]
append l [] = do
  tick
  return l

append [] l = do
  tick
  return l

append (x:xs) ys = do
  tick
  l <- append xs ys
  return (x:l)

-- Реализуйте qsort через filter' и append
qsort :: Ord a => [a] -> Counter [a]
qsort [] = do
  tick
  return []

qsort (x:xs) = do
  l <- filter' (<= x) xs
  r <- filter' (> x) xs
  l <- qsort l
  r <- qsort r
  res <- append l [x]
  res <- append res r
  return res

-- Первый вызов должен занимать большее количество тиков ~ в 2 раза
main = do
    print $ runCounter $ qsort [1..15]
    print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
