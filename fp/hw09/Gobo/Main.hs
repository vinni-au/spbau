import Data.Char
import Data.Maybe
import Control.Monad
import qualified Data.Map as M
import Test.HUnit

import Gobo

getList :: Prog String
getList = undefined

getLength :: Prog Int
getLength = undefined

getInt :: Prog Int
getInt = undefined

findChar :: Char -> Prog Coord
findChar = undefined

-- Если считать, что Just _ - это суша, а Nothing - это вода, то getArea возвращает площадь острова, на котором стоит Гобо
getArea :: Prog Int
getArea = undefined

-- tests

board1 = M.fromList $ map (\x -> (Coord (fromIntegral x) 0, intToDigit (x + 1))) [0..8]

fromLists :: Coord -> [String] -> Board
fromLists (Coord x y) b = M.unions $ zipWith (\s j -> M.fromList $ zipWith (\c i -> (Coord i j, c)) s [x..]) b [y..]

board2 = fromLists (Coord (-6) (-5))
    [ "123456789"
    , "9qwertyui"
    , "0op[]asdf"
    , "5ghjkl;zx"
    , "6cvbnm,./"
    , "4QWERTYUI"
    , "8OP{}ASDF"
    , "3GHJKL:ZX"
    , "7CVBNM<>?"
    ]

board3 = M.fromList $ do
    let d = 50
    i <- [-d..d]
    j <- [-d..d]
    guard $ i * i + j * j < d * d
    return (Coord i j, 'x')

board4 = M.delete (Coord 0 0) $ fromLists (Coord (-1) (-1))
    [ "xxx"
    , "xxx"
    , "xxx"
    ]

board5 = M.insert (Coord (-1) (-3)) 'x' $ M.delete (Coord 1 (-1)) $ M.delete (Coord 1 0) $ fromLists (Coord 0 (-2))
    [ "xxx"
    , "xxx"
    , "xxx"
    ]

board6 = M.insert (Coord (-1) (-2)) 'x' $ M.delete (Coord 1 (-1)) $ M.delete (Coord 1 0) $ fromLists (Coord 0 (-2))
    [ "xxx"
    , "xxx"
    , "xxx"
    ]

board7 = fromLists (Coord 0 0)
    [ "xxxxx"
    , "xxxxx"
    , "xxxxx"
    , "xxxxx"
    , "xxxxx"
    ] M.\\ M.delete (Coord 2 2) (fromLists (Coord 1 1)
        [ "xxx"
        , "xxx"
        , "xxx"
        ])

board8 = M.filter isLetter board2

main = fmap (const ()) $ runTestTT $ test
    $    label "getLength"
    [ runGobo getLength board1 ~?= 9
    , runGobo getLength board2 ~?= 3
    , runGobo getLength board3 ~?= 50
    , runGobo getLength board4 ~?= 0
    , runGobo getLength board5 ~?= 1
    , runGobo getLength board6 ~?= 1
    , runGobo getLength board7 ~?= 5
    , runGobo getLength board8 ~?= 3
    ] ++ label "getInt"
    [ runGobo getInt board1 ~?= 123456789
    ] ++ label "findChar"
    [ runGobo (findChar 'Y') board2 ~?= Coord 0 0
    , runGobo (findChar '2') board2 ~?= Coord (-5) (-5)
    , runGobo (findChar '>') board2 ~?= Coord 1 3
    ] ++ label "getArea"
    [ runGobo getArea board1 ~?= 9
    , runGobo getArea board2 ~?= 81
    , runGobo getArea board3 ~?= 7825
    , runGobo getArea board4 ~?= 0
    , runGobo getArea board5 ~?= 7
    , runGobo getArea board6 ~?= 8
    , runGobo getArea board7 ~?= 16
    , runGobo getArea board8 ~?= 52
    ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
