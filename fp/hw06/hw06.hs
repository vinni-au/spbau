import qualified Data.Map as M

import Prelude hiding (lookup)
import Test.HUnit

------------------------------------------------------------------------------
-- 1

data Tree a = Node { value :: a, children :: [Tree a] }

-- (a) Возвращает высоту дерева
height :: Tree a -> Int
height (Node _ []) = 1
height (Node v c) = 1 + maximum (map height c)

-- (b) Возвращает среднее арифметическое значений во всех узлах дерева
-- Необходимо вычислить эту функцию, выполнив один проход по дереву
avg :: Tree Int -> Int
avg t = fst p `div` snd p
  where 
    p = browse t
    browse (Node v []) = (v, 1)
    browse (Node v c) = (0, 0)

-- (c) Возвращает ширину дерева
-- Ширина дерева определяется следующим образом:
-- Количество вершин на определенном уровне называется шириной уровня.
-- Ширина дерева - это максимальная ширина уровня по всем уровням.
width :: Tree a -> Int
width = undefined

-- tests

(tree1, tree2, tree3) =
    ( b [b [l [b []],
            l [b [],
               l [b [l [],
                     l [],
                     b []],
                  l []]]],
         b [],
         b [],
         l []]
    , b [b [b [],
            b [b [],
               b []]],
         b [b [],
            l [b [],
               b []]],
         l [b []]]
    , b [tree1, tree2]
    )
  where l = Node 500; b = Node 300

(testsHeight, testsAvg, testsWidth) = (
    [ height tree1 ~?= 6
    , height tree2 ~?= 4
    , height tree3 ~?= 7
    ],
    [ avg tree1 ~?= 393
    , avg tree2 ~?= 330
    , avg tree3 ~?= 362
    ],
    [ width tree1 ~?= 4
    , width tree2 ~?= 5
    , width tree3 ~?= 7
    ])

------------------------------------------------------------------------------
-- 2. Робот Гобо

data Cmd
    = Go Direction -- Гобо может пойти куда-нибудь
    | Do Action -- или сделать что-нибудь

-- Гобо может пойти налево, направо, наверх или вниз
data Direction = L | R | U | D

data Action
    = Take -- Гобо может взять то, что лежит в данной ячейке
           -- (если там ничего нет или руки заняты, Гобо ничего не делает)
    | Drop -- или положить то, что у него в руках
           -- (если руки пусты, или ячейка занята, Гобо ничего не делает)
    | Inspect -- или изучить то, что находится в ячейке

type Program = [Cmd]

type Board a = M.Map (Integer, Integer) a
    -- Если по ключу (i, j) в мапе ничего нет, значит ячейка пуста

-- runGobo p b i j запускает программу p на карте b с начальной позициец Гобо (0, 0)
-- и с пустыми руками. Возвращает список изученных объектов.
runGobo :: Program -> Board a -> [a]
runGobo = undefined

-- tests

board1 = M.fromList [((0,0),"hi"), ((0,1),"hello"), ((1,1),"salute")]
program1 = cycle [Go R, Go D, Go L, Do Take, Go U, Go R, Go D, Do Inspect
    , Go L, Go U, Go R, Do Drop, Go D, Go L, Go U, Do Take, Go R, Go D
    , Go L, Do Inspect, Go U, Go R, Go D, Do Drop, Go L, Go U, Go R, Do Take
    , Go D, Go L, Go U, Do Inspect, Go R, Go D, Go L, Do Drop, Go U, Go R
    , Go D, Do Take, Go L, Go U, Go R, Do Inspect, Go D, Go L, Go U, Do Drop]

board2 = M.fromList [((0,0),'к'), ((0,1),'о'), ((1,0),'т'), ((1,1),'у'), ((-1,0),' ')]
program2 = [Do Inspect, Go R, Do Inspect, Do Take, Go D]
    ++ p ++ p ++ [Go D, Go D, Do Drop, Do Inspect, Go R, Do Take, Go R, Go R]
    ++ p ++ [Do Drop, Go D, Do Take, Go L, Do Drop]
    ++ p ++ [Do Inspect, Go R, Go D, Do Inspect]
    ++ p ++ [Go R, Do Inspect, Go R, Do Inspect]
  where p = [Go L, Do Inspect, Go U]

testsGobo = [ take 3 (runGobo program1 board1) ~?= ["salute","hello","salute"]
            , runGobo program2 board2 ~?= "кто тут утко" ]

------------------------------------------------------------------------------
-- 3. Expr

data Value = I Integer | B Bool deriving (Eq, Show)
data BinOp = Plus | Mul | Minus | Less | Greater | Equals
data UnOp = Neg | Not
data Expr = BinOp BinOp Expr Expr | UnOp UnOp Expr | Const Value
    | Let String Expr Expr | If Expr Expr Expr | Var String

type Error = String

-- eval m e интерпретирует выражение e в контексте m.
-- eval возвращает либо успешно вычисленный результат, либо список ошибок.
-- Ошибки бывают двух видов: необъявленная переменная и несоответствие типов.
eval :: M.Map String Value -> Expr -> Either [Error] Value
eval = undefined

-- tests

max' x y = If (BinOp Less x y) y x
expr1 = Let "x" (BinOp Plus (Const (I 3)) (Const (I 4)))
    $ Let "y" (BinOp Mul (Var "x") (Const (I 6)))
    $ UnOp Neg $ max' (Var "x") (Var "y")
expr2 = BinOp Plus (Var "x") (Const (I 3))
expr3 = If (Var "x") (BinOp Minus (Var "y") (Const (I 3))) (Const (I 2))

testsExpr = [ eval M.empty expr1 ~?= Right (I (-42))
            , errorsCount (eval M.empty expr2) ~?= 1
            , eval (M.fromList [("x", B True), ("y", I 5)]) expr3 ~?= Right (I 2)
            , eval (M.fromList [("x", B False), ("y", B False)]) expr3 ~?= Right (I 2)
            , errorsCount (eval (M.fromList [("x", B True), ("y", B False)]) expr3) ~?= 1 ]
  where errorsCount = either length (const 0)

------------------------------------------------------------------------------
-- 4. Реализовать двоичное дерево поиска без балансировки.

data Map k v = Leaf | Branch k v (Map k v) (Map k v)

lookup :: Ord k => k -> Map k v -> Maybe v
lookup = undefined

insert :: Ord k => k -> v -> Map k v -> (Map k v, Maybe v)
insert = undefined

delete :: Ord k => k -> Map k v -> Maybe (Map k v)
delete = undefined

fromList :: Ord k => [(k, v)] -> Map k v
fromList = undefined

toList :: Map k v -> [(k, v)]
toList = undefined

-- tests

sort :: Ord a => [a] -> [a]
sort = map fst . toList . fromList . map (\x -> (x, ()))

------------------------------------------------------------------------------
-- main

main = fmap (\_ -> ()) $ runTestTT $ test
    $    label "height" testsHeight
      ++ label "avg" testsAvg
      ++ label "width" testsWidth
      ++ label "Gobo" testsGobo
      ++ label "Expr" testsExpr
      ++ label "fourth task" -- можете сами написать тесты на каждую функцию :)
    [ sort [10,24,13,56,35,13,6,23] ~?= [6,10,13,23,24,35,56] ]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
