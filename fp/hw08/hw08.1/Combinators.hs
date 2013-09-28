module Combinators
    ( module Parser
    , many
    , many1
    , lexeme
    , anyLexeme
    , string
    , oneOf
    , digit
    , natural
    , integer
    , spaces
    , try
    , endBy
    , endBy1
    , sepBy
    , sepBy1
    , foldr1P
    , foldl1P
    , between
    , brackets
    , parens
    , braces
    , angles
--    , ff
    ) where

import Parser
import Data.Char

lexeme :: Eq l => l -> Parser l ()
lexeme lex = fmap (\x -> ()) $ satisfy (== lex)

anyLexeme :: Parser l l
anyLexeme = satisfy $ \x -> True

digit :: Parser Char Int
digit = fmap digitToInt $ satisfy (isDigit)

string :: Eq l => [l] -> Parser l ()
string (c:cs) = (fmap (\x -> id) (satisfy (== c))) <*> (string cs)
string c = pure ()

oneOf :: Eq l => [l] -> Parser l l
oneOf = foldr (\x -> ((satisfy (== x)) <|> )) empty

many :: Parser l a -> Parser l [a]
many p = ((fmap (\x y -> [x] ++ y) p) <*> (many p)) <|> (pure [])

many1 :: Parser l a -> Parser l [a]
many1 p = (fmap (\x y -> x:y) p) <*> (many p)

natural :: Parser Char Integer
natural = fmap read $ many1 $ satisfy $ isDigit

integer :: Parser Char Integer
integer = (fmap (\ _ x -> -x) (satisfy (== '-'))) <*> natural <|> natural 

spaces :: Parser Char ()
spaces = fmap (\x -> ()) $ many $ satisfy (== ' ')

try :: Parser l a -> Parser l (Maybe a)
try p = fmap Just p <|> pure Nothing

endBy :: Parser l a -> Parser l b -> Parser l [a]
endBy p1 p2 = many $ (fmap (\x _ -> x) p1) <*> p2

endBy1 :: Parser l a -> Parser l b -> Parser l [a]
endBy1 p1 p2 = many1 $ (fmap (\x _ -> x) p1) <*> p2

sepBy :: Parser l a -> Parser l b -> Parser l [a]
sepBy p1 p2 = sepBy1 p1 p2 <|> pure []

sepBy1 :: Parser l a -> Parser l b -> Parser l [a]
sepBy1 p1 p2 = (fmap (\ x y -> [x] ++ y) p1) <*> (many $ fmap (\_ -> id) p2 <*> p1) 

between :: Parser l a -> Parser l b -> Parser l c -> Parser l c
between a b c = fmap (\x y z -> y) a <*> c <*> b

brackets :: Parser Char a -> Parser Char a
brackets p = between (lexeme '[') (lexeme ']') p

parens :: Parser Char a -> Parser Char a
parens p = between (lexeme '(') (lexeme ')') p

braces :: Parser Char a -> Parser Char a
braces p = between (lexeme '{') (lexeme '}') p

angles :: Parser Char a -> Parser Char a
angles p = between (lexeme '<') (lexeme '>') p

-- Эти функции можно не реализовывать.
-- Они работают как sepBy1, только возвращают не список, а сворачивают его при помощи функции.
foldr1P :: (a -> b -> a -> a) -> Parser l a -> Parser l b -> Parser l a
foldr1P f p1 p2 = (fmap ff p1) <*> (many $ fmap (\x -> makepair x) p2 <*> p1)
  where
    makepair x y = (x,y)
    ff x [] = x
    ff x lp = f x (fst $ head lp) (ff (snd $ head lp) (tail lp))

foldl1P :: (a -> b -> a -> a) -> Parser l a -> Parser l b -> Parser l a
foldl1P f p1 p2 = (fmap ff p1) <*> (many $ fmap (\x -> makepair x) p2 <*> p1)
  where
    makepair x y = (x, y)
    ff x [] = x
    ff x lp = ff (f x (fst $ head lp) (snd $ head lp)) (tail lp)

