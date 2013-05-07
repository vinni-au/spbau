import Data.Char
import Data.List

numsum :: Int -> Int
numsum  = sum . map digitToInt . show

-- reverses list
reversel [] = []
reversel (h:t) = (reversel t) ++ [h]

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f [] = []
mapFilter f (h:t) = case f h of
  Nothing ->  mapFilter f t
  Just x -> x : mapFilter f t

shuffleOddEven :: [a] -> [a]
shuffleOddEven (a : b : t) = b : a : shuffleOddEven t
shuffleOddEven x = x

mapn :: (Int -> a -> b) -> [a] -> [b]
mapn f = mapn' 0
  where
    mapn' n [] = []
    mapn' n (x:xs) = f n x : mapn' (n+1) xs

mapnz :: (Int -> a -> b) -> [a] -> [b]
mapnz f l = map (\p-> f (fst p) (snd p)) (zip [0..] l)

