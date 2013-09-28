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
avg t = let p = browse t in fst p `div` snd p
  where 
    browse tr = case children tr of
      [] -> (value tr, 1)
      (x:xs) -> (sum + value tr, count + 1)
        where
          (sum, count) = foldr browseh (0, 0) (x:xs)
          browseh t1 (sm, cnt) = (sm + smc, cnt + cntc)
            where
              (smc, cntc) = browse t1

-- (c) Возвращает ширину дерева
-- Ширина дерева определяется следующим образом:
-- Количество вершин на определенном уровне называется шириной уровня.
-- Ширина дерева - это максимальная ширина уровня по всем уровням.
width :: Tree a -> Int
width t = maximum (helper [0] (children t))
  where
    helper ws [] = ws
    helper ws ts = helper ((length ts):ws) (childrenlist ts)
    childrenlist ts = concat (map children ts) 

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
runGobo p b = runh p b Nothing 0 0 
  where
    runh [] _ _ _ _= []
    runh (c:cmds) b items x y = case c of
      Go L -> runh cmds b items (x-1) y
      Go R -> runh cmds b items (x+1) y
      Go U -> runh cmds b items x (y-1)
      Go D -> runh cmds b items x (y+1)
      Do Take -> case (M.lookup(x, y) b, items) of
	(Just item, Nothing) -> runh cmds (M.delete (x, y) b) (Just item) x y
	_ -> runh cmds b items x y
      Do Drop -> case (M.lookup (x,y) b, items) of
	(Nothing, Just item) -> runh cmds (M.insert (x,y) item b) Nothing x y
	_ -> runh cmds b items x y
      Do Inspect -> case (M.lookup(x,y) b) of
	Nothing -> runh cmds b items x y
	Just item -> item:(runh cmds b items x y)

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
eval m (BinOp Plus el er)     = execWithInteger m (+) el er
eval m (BinOp Mul el er)      = execWithInteger m (*) el er 
eval m (BinOp Minus el er)    = execWithInteger m (-) el er
eval m (BinOp Less el er)     = execWithBoolean m (<) el er
eval m (BinOp Greater el er)  = execWithBoolean m (>) el er
eval m (BinOp Equals el er)   = execWithBoolean m (==) el er

eval m (UnOp Neg e) = case eval m e of 
  Left error  -> Left error
  Right (B _) -> Left["Type mismatch"]
  Right (I v) -> Right (I (-v))

eval m (UnOp Not e) = case eval m e of 
  Left error  -> Left error
  Right (I _) -> Left["Type mismatch"]
  Right (B v) -> Right (B (not v))

eval m (Const v) = Right v

eval m (Let x el er) = case eval m el of 
  Left error  -> Left error
  Right v   -> eval (M.insert x v m) er

eval m (If cond el er) = case eval m cond of
  Left error  -> Left error
  Right (I _) -> Left["Type mismatch"]
  Right (B v) -> if v then eval m el else eval m er

eval m (Var v) = case M.lookup v m of
  Nothing -> Left ["Undefined variable"]
  Just v  -> Right v
  
execWithInteger m op el er = case eval m el of
  Left error   -> Left error
  Right (B _)  -> Left ["Type mismatch"]
  Right (I v1) -> case eval m er of
    Left error1   -> Left error1
    Right (B _)  -> Left ["Type mismatch"]
    Right (I v2) -> Right (I (op v1 v2))
   
execWithBoolean m op el er = case eval m el of
  Left error        -> Left error
  Right (B _ )  -> Left ["Type mismatch"]
  Right (I v1)  -> case eval m er of
    Left error1      -> Left error1
    Right (B _)   -> Left ["Type mismatch"]
    Right (I v2)  -> Right (B (op v1 v2))

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
lookup k Leaf = Nothing
lookup k (Branch key val l r) 
  | k == key  = Just val
  | k < key = lookup k l
  | otherwise = lookup k r

insert :: Ord k => k -> v -> Map k v -> (Map k v, Maybe v)
insert k v m = (inserth k v m, lookup k m)
  where
    inserth k v Leaf = Branch k v Leaf Leaf
    inserth k v (Branch key value l r) 
      | k == key = Branch key value l r
      | k < key = Branch key value (inserth k v l) r
      | otherwise = Branch key value l (inserth k v r)

delete :: Ord k => k -> Map k v -> Maybe (Map k v)
delete k Leaf = Just Leaf
delete k (Branch key value l r) 
  | k < key = deleteFromLeft $ delete k l
  | k > key = deleteFromRight $ delete k r
  | otherwise = Just $ merge l r
    where
      deleteFromLeft (Just t) = Just $ Branch key value t r
      deleteFromLeft _ = Nothing
      deleteFromRight (Just t) = Just $ Branch key value l t
      deleteFromRight _ = Nothing
      merge t Leaf = t
      merge Leaf t = t
      merge (Branch k1 v1 l1 r1) t = Branch k1 v1 l1 $ merge r1 t

fromList :: Ord k => [(k, v)] -> Map k v
fromList ps = fromlisth ps Leaf
  where
    fromlisth (p:ps) t = fromlisth ps $ fst $ insert (fst p) (snd p) t
    fromlisth [] t = t

toList :: Map k v -> [(k, v)]
toList Leaf = []
toList (Branch k v l r) = (toList l) ++ [(k,v)] ++ (toList r)

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
