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
    ) where

import Parser
import Data.Char

lexeme :: Eq l => l -> Parser l ()
lexeme = undefined

anyLexeme :: Parser l l
anyLexeme = undefined 

digit :: Parser Char Int
digit = undefined

string :: Eq l => [l] -> Parser l ()
string = undefined

oneOf :: Eq l => [l] -> Parser l l
oneOf = undefined

many :: Parser l a -> Parser l [a]
many = undefined

many1 :: Parser l a -> Parser l [a]
many1 = undefined

natural :: Parser Char Integer
natural = undefined

integer :: Parser Char Integer
integer = undefined

spaces :: Parser Char ()
spaces = undefined

try :: Parser l a -> Parser l (Maybe a)
try = undefined

endBy :: Parser l a -> Parser l b -> Parser l [a]
endBy = undefined

endBy1 :: Parser l a -> Parser l b -> Parser l [a]
endBy1 = undefined

sepBy :: Parser l a -> Parser l b -> Parser l [a]
sepBy = undefined

sepBy1 :: Parser l a -> Parser l b -> Parser l [a]
sepBy1 = undefined

between :: Parser l a -> Parser l b -> Parser l c -> Parser l c
between = undefined

brackets :: Parser Char a -> Parser Char a
brackets = undefined

parens :: Parser Char a -> Parser Char a
parens = undefined

braces :: Parser Char a -> Parser Char a
braces = undefined

angles :: Parser Char a -> Parser Char a
angles = undefined

-- Эти функции можно не реализовывать.
-- Они работают как sepBy1, только возвращают не список, а сворачивают его при помощи функции.
foldr1P :: (a -> b -> a -> a) -> Parser l a -> Parser l b -> Parser l a
foldr1P = undefined

foldl1P :: (a -> b -> a -> a) -> Parser l a -> Parser l b -> Parser l a
foldl1P = undefined
