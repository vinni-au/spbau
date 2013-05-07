{-# LANGUAGE TupleSections #-}

import Control.Monad
import qualified Data.Map as M
import Test.HUnit

import Expr
import Eval
import Utils

getInt :: Eval Value -> Eval Integer
getInt m = undefined

getBool :: Eval Value -> Eval Bool
getBool m = undefined

if' :: Eval Value -> Eval a -> Eval a -> Eval a
if' c t e = undefined

evalExpr :: Expr -> Eval Value
evalExpr = undefined

evalStat :: Stat -> Eval ()
evalStat = undefined

evalProg :: Prog -> Eval ()
evalProg (Prog ss) = evalStat (Comp ss)

------------------------------------------------------------------------------------------------
-- tests
------------------------------------------------------------------------------------------------

-- test1 = "!x || (y < 3 && z == y) && 5 < y + 7 * (z + y * 3)"
test1 = BinOp Or (UnOp Not $ Var "x") $ BinOp And (BinOp And (BinOp Less (Var "y") (Const $ I 3)) (BinOp Equals (Var "z") (Var "y")))
    $ BinOp Less (Const $ I 5) $ BinOp Plus (Var "y") $ BinOp Mul (Const $ I 7) $ BinOp Plus (Var "z") $ BinOp Mul (Var "y") (Const $ I 3)

-- test2 = "-5 + -3 * 2 - 7"
test2 = BinOp Minus (BinOp Plus (UnOp Neg $ Const $ I 5) (BinOp Mul (UnOp Neg $ Const $ I $ 3) (Const $ I 2))) (Const $ I 7)

-- test3 = "r = 1;\n"
--     ++ "while (n > 0) {\n"
--     ++ "\tr = r * n;\n"
--     ++ "\tn = n - 1;\n"
--     ++ "}"
test3 = Prog
    [ Assign "r" (Const (I 1))
    , While (BinOp Greater (Var "n") (Const (I 0))) $ Comp
        [ Assign "r" (BinOp Mul (Var "r") (Var "n"))
        , Assign "n" (BinOp Minus (Var "n") (Const (I 1)))
        ]
    ]

main = fmap (\_ -> ()) $ runTestTT $ test
    [ errorsCount (runEval (evalExpr test1) $ M.fromList [("x",I 3),("y",I 5),("f",I 5)]) ~?= 2
        -- 2 ошибки: неизвестная переменная z и несоответствие типов (в том месте, где вычисляется "!x")
    , let m = M.fromList [("x",B True),("y",I 5),("z",I 5)] in runEval (evalExpr test1) m ~?= (Just (B False), [], m)
    , let m = M.fromList [("x",B True),("y",I 2),("z",I 2)] in runEval (evalExpr test1) m ~?= (Just (B True ), [], m)
    , runEval (evalExpr test2) M.empty ~?= (Just (I (-18)), [], M.empty)
    , runEval (evalProg test3) (M.fromList [("n",I 5)]) ~?= (Just (), [], M.fromList [("n",I 0),("r",I 120)])
    ]
  where
    errorsCount (_,es,_) = length es
