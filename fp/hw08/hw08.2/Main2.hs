{-# LANGUAGE TupleSections #-}

import qualified Data.Map as M
import Data.Maybe
import Test.HUnit

import Expr
import Parsing

type Store = M.Map String Value

type Error = String

evalExpr :: Store -> Expr -> Either [Error] Value
evalExpr = undefined

evalStat :: Store -> Stat -> ([Error], Store)
evalStat = undefined

evalProg :: Store -> Prog -> ([Error], Store)
evalProg = undefined

inp1 = "!x || (y < 3 && z == y) && 5 < y + 7 * (z + y * 3)"
inp2 = "-5 + -3 * 2 - 7"
inp3 = "r = 1;\n"
    ++ "while (n > 0) {\n"
    ++ "\tr = r * n;\n"
    ++ "\tn = n - 1;\n"
    ++ "}"
inp4 = "if (x > 0) y = 1; else y = 2;"
inp5 = "if (x > 0) y = 1;"

prog3 = Prog
    [ Assign "r" (Const (I 1))
    , While (BinOp Greater (Var "n") (Const (I 0))) (Comp
        [ Assign "r" (BinOp Mul (Var "r") (Var "n"))
        , Assign "n" (BinOp Minus (Var "n") (Const (I 1)))
        ])
    ]
prog4 = Prog [ If (BinOp Greater (Var "x") (Const (I 0))) (Assign "y" (Const (I 1))) (Assign "y" (Const (I 2))) ]
prog5 = Prog [ If (BinOp Greater (Var "x") (Const (I 0))) (Assign "y" (Const (I 1))) (Comp []) ]

main = fmap (\_ -> ()) $ runTestTT $ test
    [ parseProg inp3 ~?= Just prog3
    , parseProg inp4 ~?= Just prog4
    , parseProg inp5 ~?= Just prog5
    , errorsCount (evalExpr (M.fromList [("x",B True),("t",I 5),("f",I 5)]) . fromJust . parseExpr $ inp1) ~?= 2
    , (evalExpr (M.fromList [("x",B True),("y",I 5),("z",I 5)]) . fromJust . parseExpr $ inp1) ~?= Right (B False)
    , (evalExpr (M.fromList [("x",B True),("y",I 2),("z",I 2)]) . fromJust . parseExpr $ inp1) ~?= Right (B True)
    , (evalExpr M.empty . fromJust . parseExpr $ inp2) ~?= Right (I (-18))
    , (evalProg (M.fromList [("n",I 5)]) . fromJust . parseProg $ inp3) ~?= ([], M.fromList [("n",I 0),("r",I 120)])
    ]
  where
    errorsCount (Left es) = length es
    errorsCount _ = 0
