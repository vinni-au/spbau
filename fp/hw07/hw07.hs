import Control.Applicative
import Test.HUnit
import qualified Data.List as L

------------------------------------------------------------------------------
-- 1

data Nat = Zero | Suc Nat

instance Eq Nat where
    (==) Zero Zero = True
    (==) Zero (Suc a) = False
    (==) (Suc a) Zero = False
    (==) (Suc a) (Suc b) = (==) a b

instance Ord Nat where
    compare Zero Zero = EQ
    compare Zero (Suc a) = LT
    compare (Suc a) Zero = GT
    compare (Suc a) (Suc b) = compare a b

multh :: Nat -> Nat -> Nat -> Nat
multh a (Suc Zero) acc = acc+a
multh a (Suc (Suc b)) acc = multh a (Suc b) acc+a
instance Num Nat where
    (+) a Zero = a
    (+) a (Suc b) = (+) (Suc a) b
    (*) Zero a = Zero
    (*) a Zero = Zero
    (*) a b = multh a b Zero
    
    signum Zero = 0
    signum (Suc a) = 1
    
    fromInteger 0 = Zero
    fromInteger i = if i < 0 then error "Nat: negative value" else  (Suc Zero) + fromInteger (i - 1)
    
    negate Zero = Zero
    negate _ = error "Nat: negative value"
    
    abs = id

showh Zero i = show i
showh (Suc a) i = showh a (i+1)
instance Show Nat where
    show a = showh a 0

-- tests

one = Suc Zero
two = Suc one
three = Suc two

testsNat =
    [ two ~?= two
    , three ~?/= two
    , (two < three) ~?= True
    , (one > three) ~?= False
    , show two ~?= "2"
    , show three ~?= "3"
    , fromInteger 3 ~?= three
    , three + fromInteger 7 ~?= fromInteger 10
    , three * fromInteger 7 ~?= fromInteger 21
    ]

------------------------------------------------------------------------------
-- 2

data Tree a = Node { value :: a, children :: [Tree a] }

eqh [x] [y] = x == y
eqh (x:xs) (y:ys) = x == y && (eqh xs ys)
instance Eq a => Eq (Tree a) where
    (==) (Node a []) (Node b []) = a == b
    (==) (Node a c) (Node b []) = False
    (==) (Node a []) (Node b c) = False
    (==) (Node a ca) (Node b cb) = a == b && eqh ca cb

showth [x] = (show x)
showth (x:xs) = (show x) ++ "," ++ (showth xs)
instance Show a => Show (Tree a) where
    show (Node a []) = show a
    show (Node a c) = (show a) ++ ":[" ++ (showth c) ++ "]"

instance Functor Tree where
    fmap f (Node a cs) = Node (f a) (map (fmap f) cs)
    
instance Applicative Tree where
    pure x = Node x []
    (<*>) (Node f fs) (Node v vs) = Node (f v) (helper fs vs) 
      where
        helper f [] = []
        helper [] x = []
        helper (f:fs) (x:xs) = (f <*> x) : (helper fs xs)

-- tests

tree1 = Node "a" [Node "b" [Node "f" []], Node "c" [Node "d" []], Node "e" []]
tree2 = Node 1 [Node 2 [Node 4 [], Node 5 []], Node 3 []]
tree3 = Node "1" [Node "2" [Node "4" [], Node "5" []], Node "3" []]
tree4 = Node "a1" [Node "b2" [Node "f4" []], Node "c3" []]
tree8 = Node 600 [Node 1200 [Node 3200 [], Node 4000 []], Node 1800 []]

(tree5, tree6, tree7) =
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
    , b [b [l [],
            l [b [],
               l []]],
         b [],
         l []]
    )
  where l = Node 500; b = Node 300

testsTree =
    [ tree2 ~?= tree2
    , tree1 ~?/= tree3
    , show tree1 ~?= "\"a\":[\"b\":[\"f\"],\"c\":[\"d\"],\"e\"]"
    , show tree2 ~?= "1:[2:[4,5],3]"
    , fmap show tree2 ~?= tree3
    , (\x y -> x ++ show y) <$> tree1 <*> tree2 ~?= tree4
    , max <$> tree5 <*> tree6 ~?= tree7
    , (\x5 x6 x2 -> (x5 + x6) * x2) <$> tree5 <*> tree6 <*> tree2 ~?= tree8
    ]

------------------------------------------------------------------------------
-- 3
--

tupleFunctor :: Functor f => f (a1, a2) -> (f a1, f a2)
tupleFunctor p  = (fmap fst p, fmap snd p)

eitherFunctor :: Functor f => Either (f a1) (f a2) -> f (Either a1 a2)
eitherFunctor (Left x) = fmap (\y -> Left y) x
eitherFunctor (Right x) = fmap (\y -> Right y) x

-- ifsum cs xs ys ps qs действует поэлементно. Если i-ый элемент списка cs есть True,
-- то i-ый элемент результата будет Left от суммы i-ых элементов xs ys, иначе Right от суммы i-ых элементов ps и qs.
-- Для реализации используйте instance Applicative для ZipList.
ifsum :: (Num a, Num b) => [Bool] -> [a] -> [a] -> [b] -> [b] -> [Either a b]
ifsum cs xs ys ps qs = 
  getZipList $ getziplisth <$>
  ZipList cs <*> ZipList xs <*> ZipList ys <*> ZipList ps <*> ZipList qs
    where 
      getziplisth c x y p q | c = Left (x+y)
                            | otherwise = Right (p+q)

tupleApplicative :: Applicative f => (f a1, f a2) -> f (a1, a2)
tupleApplicative (x, y) = (fmap (,) x) <*> y

-- tests

testsIfsum =
    [ ifsum [True, False, True] [1,2,3] [4,5,6] [7,8,9] [10,11,12] ~?= [Left 5, Right 19, Left 9]
    , ifsum [True, True, True] [1,2,3] [4,5,6] [7,8,9] [] ~?= []
    ]

------------------------------------------------------------------------------
-- 4

-- Map k v можно определить просто как функцию k -> Maybe v.
-- Если по ключу k возвращается Nothing, значит в мапе ничего не хранится по этому ключу.
-- Если по ключу k возвращается Just v, значит в мапе по ключу k хранится v.
newtype Map k v = Map { lookupMap :: k -> Maybe v }

insert :: Eq k => k -> v -> Map k v -> (Map k v, Maybe v)
insert k v m = (Map lookuph, lookupMap m k)
  where
    lookuph x | x == k = Just v
              | otherwise = (lookupMap m x)

delete :: Eq k => k -> Map k v -> Maybe (Map k v)
delete k m | isnothing (lookupMap m k) = Nothing
           | otherwise = Just (Map lookuph)
  where
    lookuph x | x == k = Nothing
              | otherwise = lookupMap m x
    isnothing Nothing = True
    isnothing _ = False

fromList :: Eq k => [(k, v)] -> Map k v
fromList l = fromlisth l (Map stub)
  where
    stub _ = Nothing
    fromlisth (x:xs) m = fromlisth xs (fst (insert (fst x) (snd x) m))
    fromlisth [] m = m

toList :: Map k v -> [k] -> [v]
toList m ks = tolisth [] ks
  where
    tolisth vs (k:ks) = tolisth (vs ++ mbtolist (lookupMap m k)) ks
    tolisth vs [] = vs
    mbtolist Nothing = []
    mbtolist (Just v) = [v]

instance Functor (Map k) where
    fmap f (Map g) = Map (fmap f . g)

instance Applicative (Map k) where
    pure x = Map (\y -> Just x)
    (<*>) (Map f) (Map g) = Map (\x -> (f x) <*> (g x))

-- tests

mapTest1 = fromList [(1,"a"),(2,"b"),(3,"c"),(4,"d")]
mapTest2 k v = let (a,b) = insert k v mapTest1 in (toList a [1..10], b)
mapTest3 = fromList $ map (\i -> (i, i * 2)) [3..6]
mapTest4 = Map $ const (Just True)

testsMap =
    [ toList mapTest1 [4,1,5,2] ~?= ["d","a","b"]
    , toList mapTest3 [1..10] ~?= [6,8,10,12]
    , mapTest2 5 "e" ~?= (["a","b","c","d","e"], Nothing)
    , mapTest2 2 "e" ~?= (["a","e","c","d"], Just "b")
    , fmap (\m -> toList m [1..10]) (delete 3 mapTest1) ~?= Just ["a","b","d"]
    , fmap (\m -> toList m [1..10]) (delete 5 mapTest1) ~?= Nothing
    , toList (fmap (++ " уже слишком поздно") mapTest1) [1..10] ~?=
        ["a уже слишком поздно","b уже слишком поздно","c уже слишком поздно","d уже слишком поздно"]
    , toList (fmap length mapTest1) [1..10] ~?= [1,1,1,1]
    , flip toList [1..10]
        ((\x1 x3 x4 x3' -> x1 ++ " " ++ show x3 ++ " " ++ show x4 ++ " " ++ show x3') <$> mapTest1 <*> mapTest3 <*> mapTest4 <*> mapTest3)
        ~?= ["c 6 True 6", "d 8 True 8"]
    ]

------------------------------------------------------------------------------
-- main

(~?/=) :: (Eq a, Show a) => a -> a -> Test
x ~?/= y = TestCase $ assertBool (show x ++ " shoud not be equal to " ++ show y) (x /= y)

main = fmap (\_ -> ()) $ runTestTT $ test
    $  label "Nat" testsNat
    ++ label "Tree" testsTree
    ++ label "ifsum" testsIfsum
    ++ label "Map" testsMap
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
