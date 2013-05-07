-- data Person = Person String Int String
-- data Person = Person {name::String, age::Int, address::String}
-- data Color = Red | Blue | Green
-- data Maybe a = Just a | Nothing

-- 
import Data.Char
findDigit :: String -> Maybe Char
findDigit [] = Nothing
findDigit (s:ss) = if (isDigit s) then Just s else findDigit ss

findDigitAndPos :: String -> Maybe (Char,Int)
findDigitAndPos ss = findDigitAndPosh ss 0
  where
    findDigitAndPosh [] _ = Nothing
    findDigitAndPosh (s:ss) i = if (isDigit s) then Just (s,i) else findDigitAndPosh ss (i+1)

findDigitAndPosOrLen :: String -> Either (Char,Int) Int
findDigitAndPosOrLen ss = findhh ss 0
  where
    findhh [] i = Right i
    findhh (s:ss) i = if (isDigit s) then Left (s,i) else findhh ss (i+1)

data Res = Digit Char Int | Letter Char Int | Len Int
findDigitAndPosOrLetterAndPosOrLen :: String -> Res
findDigitAndPosOrLetterAndPosOrLen ss = findh ss 0
  where
    findh [] i = Len i
    findh (s:ss) i 
      | isDigit s =  Digit s i
      | isLetter s = Letter s i
      | otherwise = findh ss (i+1)


--data Expr = Const Int | Minus Expr | Plus Expr Expr | Mul Expr Expr deriving Show
--eval :: Expr -> Int
--eval (Const i) = eval i
--eval (Minus i) = - (eval i)
--eval (Plus el er) = eval el + eval er
--eval (Mul el er) = eval el * eval er
--

data Tree a b = Leaf b | Node a (Tree a b) (Tree a b)
height :: Tree a b -> Int
avg :: Tree Int Int -> Int

height (Leaf b) = 0
height (Node a l r) = 1 + (max (height l) (height r))

avg t = fst (browse t) `div` snd (browse t)
  where
    browse (Leaf i) = (i,1)
    browse (Node i l r) = (i + fst (browse l) + fst (browse r), 1 + snd (browse l) + snd (browse r))
    

merge :: (Maybe a -> Maybe b -> Maybe c) -> Tree a () -> Tree b () -> Tree c ()
merge f ta tb = undefined


-- data Expr = Plus (Expr -> Expr)
-- data Term = Var String | App Term Term | Lam String Term
data HTerm = App HTerm HTerm | Lam (HTerm -> HTerm)
k = Lam $ \x -> Lam $ \y -> x
