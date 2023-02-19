module Expressions where
import Data.List
import Data.Char
import Data.Maybe
import Data.Tuple

data BinOp = Plus | Minus | Times | Div | No
    deriving (Show, Eq, Enum)
data Expr = Const Int | Op BinOp Expr Expr
    deriving (Show, Eq)

matchOp :: BinOp -> String
matchOp op = case op of 
                Plus -> " + "
                Minus -> " - "
                Div -> " / "
                Times -> " * "

-- Part A
expr2String :: Expr -> String
expr2String (Const n) = show n
expr2String (Op op exp1 exp2) =  expr1 ++ matchOp op ++ expr2
        where expr1 | isGreaterPriority op exp1 = "(" ++ expr2String exp1 ++ ")"
                    | otherwise = expr2String exp1
              expr2 | isGreaterPriority op exp2 = "(" ++ expr2String exp2 ++ ")"
                    |  otherwise = if getOp exp2 == op then "(" ++ expr2String exp2 ++ ")"
                        else expr2String exp2

-- increasing priority
operatorsPriority = [Plus,Minus,Times,Div]

isGreaterPriority :: BinOp -> Expr -> Bool
isGreaterPriority _ (Const n) = False
isGreaterPriority op1 (Op op2 _ _) = elemIndex op2 operatorsPriority < elemIndex op1 operatorsPriority

getOp :: Expr -> BinOp
getOp (Const _) = No
getOp (Op op _ _) = op

-- TESTS
sampleExpr = Op Div (Const 32) (Op Times (Const 63) (Const 54))
-- >>> expr2String sampleExpr
-- "32 / (63 * 54)"

sampleExpr2 = Op Plus (Const 6) (Op Plus (Const 3)(Const 6))
-- >>> expr2String sampleExpr2
-- "6 + (3 + 6)"

sampleExpr3 = Op Plus (Const 93) (Op Plus (Const 8) (Const 83))
-- >>> expr2String sampleExpr3
-- "93 + (8 + 83)"

sampleExpr4 = Op Plus (Op Plus (Const 8) (Const 83)) (Const 93) 
-- >>> expr2String sampleExpr4
-- "8 + 83 + 93"

sampleExpr5 = Op Times (Op Plus (Const 15) (Const 64)) (Op Times (Const 65) (Const 50))
-- >>> expr2String sampleExpr5
-- "(15 + 64) * (65 * 50)"


-- Part B
rewriteAssoc :: Expr -> Expr
rewriteAssoc (Const n) = Const n
rewriteAssoc (Op Minus exp1 exp2) = Op Minus (rewriteAssoc exp1) (rewriteAssoc exp2)
rewriteAssoc (Op Div exp1 exp2) = Op Div (rewriteAssoc exp1) (rewriteAssoc exp2)
rewriteAssoc (Op Plus exp1 exp2) = if getOp exp2 == Plus
    then rewriteAssoc (Op Plus (Op Plus (rewriteAssoc exp1)(rewriteAssoc (leftExpr exp2))) (rewriteAssoc (rightExpr exp2)))
    else Op Plus (rewriteAssoc exp1) (rewriteAssoc exp2)
rewriteAssoc (Op Times exp1 exp2) = if getOp exp2 == Times
    then rewriteAssoc (Op Times (Op Times (rewriteAssoc exp1)(rewriteAssoc (leftExpr exp2))) (rewriteAssoc (rightExpr exp2)))
    else Op Times (rewriteAssoc exp1) (rewriteAssoc exp2)


leftExpr :: Expr -> Expr
leftExpr (Op op exp1 _) = exp1

rightExpr :: Expr -> Expr
rightExpr (Op op _ exp2) = exp2

-- TESTS
sampleExpr11 = Op Minus (Op Plus (Op Plus (Const 69) (Op Plus (Const 67) (Const 80))) (Op Plus (Const 99) (Op Minus (Const 83) (Const 3)))) (Op Plus (Const 11) (Op Times (Const 35) (Const 55)))
-- Input    ((69 + (67 + 80) + (99 + (83 - 3))) - (11 + 35 * 55))
-- Solution (((((69 + 67) + 80) + 99) + (83 - 3)) - (11 + 35 * 55))

-- >>> expr2String(rewriteAssoc sampleExpr11)
-- "(((((69 + 67) + 80) + 99) + (83 - 3)) - (11 + 35 * 55))"
-- correct

sampleExpr12 = Op Plus (Const 69) (Op Plus (Op Plus (Const 59) (Const 29)) (Const 32))
-- Input     69 + ((59 + 29) + 32)
-- Solution ((69 + 59) + 29) + 32"
--Op Plus (Op Plus (Op Plus (Const 69) (Const 59)) (Const 29)) (Const 32)

-- >>> rewriteAssoc sampleExpr12
-- Op Plus (Op Plus (Op Plus (Const 69) (Const 59)) (Const 29)) (Const 32)

sampleExpr13 = Op Plus (Op Plus (Const 4)(Const 2)) (Const 3)
-- >>> rewriteAssoc sampleExpr13
-- Op Plus (Op Plus (Const 4) (Const 2)) (Const 3)
