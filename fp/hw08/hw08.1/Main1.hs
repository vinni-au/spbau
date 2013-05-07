module Main
    ( main
    , boolP
    , maybeP
    , listP
    , listP'
    , treeP
    ) where

import Combinators
import Test.HUnit

boolP :: Parser Char Bool
boolP = undefined

maybeP :: Parser Char a -> Parser Char (Maybe a)
maybeP = undefined

listP :: Parser Char a -> Parser Char [a]
listP = undefined

listP' :: Parser Char a -> Parser Char [a]
listP' = undefined

data Tree a b = Branch (Tree a b) a (Tree a b) | Leaf b deriving (Show, Eq)

treeP :: Parser Char a -> Parser Char b -> Parser Char (Tree a b)
treeP = undefined

main = fmap (const ()) $ runTestTT $ test
    $    label "pure"
    [ execParser (pure 4) "qwerty" ~?= Just (4, "qwerty")
    , execParser (pure 'x') "" ~?= Just ('x', "")
    ] ++ label "empty"
    [ execParser (empty :: Parser Char ()) "qwerty" ~?= Nothing
    , execParser (empty :: Parser Char ()) "" ~?= Nothing
    ] ++ label "satisfy"
    [ execParser (satisfy (/= 'x')) "qwerty" ~?= Just ('q', "werty")
    , execParser (satisfy (/= 'x')) "xwerty" ~?= Nothing
    ] ++ label "<*>"
    [ execParser (max <$> satisfy (== 'q') <*> satisfy (/= 'x')) "qwerty" ~?= Just ('w', "erty")
    , execParser ((+) <$> digit <*> digit) "5678" ~?= Just (11, "78")
    , execParser (undefined <$> satisfy (== 'q') <*> satisfy (== 'x') :: Parser Char ()) "qwerty" ~?= Nothing
    , execParser (undefined <$> satisfy (== 'x') <*> satisfy (== 'w') :: Parser Char ()) "qwerty" ~?= Nothing
    ] ++ label "<|>"
    [ execParser (satisfy (== 'q') <|> satisfy (== 'x')) "qwerty" ~?= Just ('q', "werty")
    , execParser (satisfy (== 'x') <|> satisfy (== 'q')) "qwerty" ~?= Just ('q', "werty")
    , execParser (satisfy (== 'x') <|> satisfy (== 'y')) "qwerty" ~?= Nothing
    ] ++ label "eof"
    [ execParser eof "qwerty" ~?= Nothing
    , execParser eof "" ~?= Just ((), "")
    ] ++ label "lexeme"
    [ execParser (lexeme 'q') "qwerty" ~?= Just ((), "werty")
    , execParser (lexeme 'x') "qwerty" ~?= Nothing
    ] ++ label "anyLexeme"
    [ execParser anyLexeme "qwerty" ~?= Just ('q', "werty")
    , execParser anyLexeme "" ~?= Nothing
    ] ++ label "digit"
    [ execParser digit "qwerty" ~?= Nothing
    , execParser digit "123qwerty" ~?= Just (1, "23qwerty")
    , execParser digit "" ~?= Nothing
    ] ++ label "string"
    [ execParser (string "qwerty") "qwerty" ~?= Just ((), "")
    , execParser (string "qwerty") "qwertyuiop" ~?= Just ((), "uiop")
    , execParser (string "qwerty") "qwerryuiop" ~?= Nothing
    , execParser (string "qwerty") "qwert" ~?= Nothing
    ] ++ label "oneOf"
    [ execParser (oneOf "xyz") "qwerty" ~?= Nothing
    , execParser (oneOf "xyz") "xwerty" ~?= Just ('x', "werty")
    , execParser (oneOf "xyz") "ywerty" ~?= Just ('y', "werty")
    , execParser (oneOf "xyz") "zwerty" ~?= Just ('z', "werty")
    ] ++ label "many"
    [ execParser (many (lexeme 'q')) "qwerty" ~?= Just ([()], "werty")
    , execParser (many (lexeme 'q')) "qqqwerty" ~?= Just ([(),(),()], "werty")
    , execParser (many (lexeme 'q')) "werty" ~?= Just ([], "werty")
    , execParser (many (lexeme 'q')) "" ~?= Just ([], "")
    ] ++ label "many1"
    [ execParser (many1 (lexeme 'q')) "qwerty" ~?= Just ([()], "werty")
    , execParser (many1 (lexeme 'q')) "qqqwerty" ~?= Just ([(),(),()], "werty")
    , execParser (many1 (lexeme 'q')) "werty" ~?= Nothing
    , execParser (many1 (lexeme 'q')) "" ~?= Nothing
    ] ++ label "natural"
    [ execParser natural "qwerty" ~?= Nothing
    , execParser natural "123qwerty" ~?= Just (123, "qwerty")
    , execParser natural "-123qwerty" ~?= Nothing
    , execParser natural "" ~?= Nothing
    ] ++ label "integer"
    [ execParser integer "qwerty" ~?= Nothing
    , execParser integer "123qwerty" ~?= Just (123, "qwerty")
    , execParser integer "-123qwerty" ~?= Just (-123, "qwerty")
    , execParser integer "-qwerty" ~?= Nothing
    ] ++ label "spaces"
    [ execParser spaces "qwerty" ~?= Just ((), "qwerty")
    , execParser spaces "    qwerty" ~?= Just ((), "qwerty")
    , execParser spaces "" ~?= Just ((), "")
    ] ++ label "try"
    [ execParser (try natural) "123qwerty" ~?= Just (Just 123, "qwerty")
    , execParser (try natural) "qwerty" ~?= Just (Nothing, "qwerty")
    , execParser (try (lexeme 'q')) "qwerty" ~?= Just (Just (), "werty")
    , execParser (try (lexeme 'x')) "qwerty" ~?= Just (Nothing, "qwerty")
    , execParser (try (lexeme 'x')) "" ~?= Just (Nothing, "")
    , execParser (try eof) "qwerty" ~?= Just (Nothing, "qwerty")
    , execParser (try eof) "" ~?= Just (Just (), "")
    ] ++ label "endBy"
    [ execParser (natural `endBy` lexeme ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], "xyz;")
    , execParser (natural `endBy` lexeme ';') "1;2;3;456" ~?= Just ([1,2,3], "456")
    , execParser (natural `endBy` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , execParser (natural `endBy` spaces) "qwerty" ~?= Just ([], "qwerty")
    , execParser (natural `endBy` spaces) "" ~?= Just ([], "")
    ] ++ label "endBy1"
    [ execParser (natural `endBy1` lexeme ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], "xyz;")
    , execParser (natural `endBy1` lexeme ';') "1;2;3;456" ~?= Just ([1,2,3], "456")
    , execParser (natural `endBy1` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , execParser (natural `endBy1` spaces) "qwerty" ~?= Nothing
    , execParser (natural `endBy1` spaces) "" ~?= Nothing
    ] ++ label "sepBy"
    [ execParser (natural `sepBy` lexeme ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], ";xyz;")
    , execParser (natural `sepBy` lexeme ';') "1;2;3;456" ~?= Just ([1,2,3,456], "")
    , execParser (natural `sepBy` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , execParser (natural `sepBy` spaces) "qwerty" ~?= Just ([], "qwerty")
    , execParser (natural `sepBy` spaces) "" ~?= Just ([], "")
    ] ++ label "sepBy1"
    [ execParser (natural `sepBy1` lexeme ';') "1;2;3;456;xyz;" ~?= Just ([1,2,3,456], ";xyz;")
    , execParser (natural `sepBy1` lexeme ';') "1;2;3;456" ~?= Just ([1,2,3,456], "")
    , execParser (natural `sepBy1` spaces) "12 25   300" ~?= Just ([12,25,300], "")
    , execParser (natural `sepBy1` spaces) "qwerty" ~?= Nothing
    , execParser (natural `sepBy1` spaces) "" ~?= Nothing
    ] ++ label "between"
    [ execParser (between (lexeme 'a') (lexeme 'b') (lexeme 'c')) "abc" ~?= Nothing
    , execParser (between (lexeme 'a') (lexeme 'b') (lexeme 'c')) "acb" ~?= Just ((), "")
    ] ++ label "brackets"
    [ execParser (brackets (string "qwerty")) "[qwerty]uiop" ~?= Just ((), "uiop")
    , execParser (brackets (string "qwerty")) "[qwertyu]iop" ~?= Nothing
    ] ++ label "parens"
    [ execParser (parens spaces) "(   )qwerty" ~?= Just ((), "qwerty")
    , execParser (parens spaces) "(q)werty" ~?= Nothing
    ] ++ label "braces"
    [ execParser (braces natural) "{123}" ~?= Just (123, "")
    , execParser (braces natural) "{}" ~?= Nothing
    ] ++ label "angles"
    [ execParser (angles digit) "<1>" ~?= Just (1, "")
    , execParser (angles digit) "<1 >" ~?= Nothing
    ] ++ label "boolP"
    [ execParser boolP "Trueqwerty" ~?= Just (True, "qwerty")
    , execParser boolP "False" ~?= Just (False, "")
    , execParser boolP "qwerty" ~?= Nothing
    ] ++ label "maybeP"
    [ execParser (maybeP natural) "Nothingqwerty" ~?= Just (Nothing, "qwerty")
    , execParser (maybeP natural) "Just 123qwerty" ~?= Just (Just 123, "qwerty")
    , execParser (maybeP natural) "Just123qwerty" ~?= Nothing
    ] ++ label "listP"
    [ runParser (listP integer) "[1,-23,25,347]" ~?= Just [1,-23,25,347]
    , runParser (listP integer) "[1 ,  -23,  25   ,347]" ~?= Nothing
    ] ++ label "listP'"
    [ runParser (listP' integer) "[1,-23,25,347]" ~?= Just [1,-23,25,347]
    , runParser (listP' integer) "[1 ,  -23,  25   ,347]" ~?= Just [1,-23,25,347]
    ] ++ label "treeP"
    [ runParser (treeP integer integer) "100" ~?= Just (Leaf 100)
    , runParser (treeP integer integer) "<1{2}3>" ~?= Just (Branch (Leaf 1) 2 (Leaf 3))
    , runParser (treeP integer integer) "<1{2}<3{4}5>>>" ~?= Just (Branch (Leaf 1) 2 (Branch (Leaf 3) 4 (Leaf 5)))
    , runParser (treeP integer integer) "<1{2}<3{4}5>" ~?= Nothing
    ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
