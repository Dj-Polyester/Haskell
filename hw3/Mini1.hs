module Mini1 (
    gridMap,
    gridMapIf,
    evalExpr,
    getVars,
    evalDeriv,
    parse -- reexported to allow use
    ) where

import Expression
import Parser
import Data.List

-- Do not modify the module declaration and imports above!
-- Also do not change the function signatures and do not
-- remove the dummy implementations of the functions if
-- you want your code to compile.

-- Feel free to import anything else here

-- gridMap (20 points), map function over grid elements
gridMap :: (a -> b) -> [[a]] -> [[b]]
gridMap f [] = []
gridMap f (first:alist) = (map f first):(gridMap f alist) 

-- gridMapIf (20 points), map functions over grid elements 
-- that satisfy the predicate provided as the first arg.
gridMapIf :: (a -> Bool) -> (a -> a) -> [[a]] -> [[a]]
gridMapIf boolf f [] = []
gridMapIf boolf f (first:alist) = (mapIf boolf f first):(gridMapIf boolf f alist)

mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf boolf f [] = []
mapIf boolf f (first:alist) = 
    if (boolf first) then 
        (f first):(mapIf boolf f alist)
    else
        (first):(mapIf boolf f alist)

-- evalExpr (20 points), evaluate the expression by
-- substituting (var, value) pairs given in the first arg.
evalExpr :: [(String, Int)] -> ExprV -> Int
evalExpr alist exprv = refine (eval exprv True alist)

refine :: ExprV -> Int
refine (Leaf (Constant a)) = a

eval :: ExprV -> Bool -> [(String, Int)] -> ExprV
eval (Leaf (Variable (a))) bool [] = (Leaf (Variable (a)))
eval (Leaf (Variable (a))) bool ((c,d):alist) =
    if a==c then 
        Leaf (Constant (d)) 
    else 
        eval (Leaf (Variable (a))) bool (alist) 

eval (Leaf (Constant (a))) bool alist = Leaf (Constant (a))

eval (UnaryOperation unarop (Leaf (Constant a))) bool alist =
     (Leaf (Constant (-a)))
eval (UnaryOperation unarop (Leaf (Variable a))) bool alist =
     if bool == True then
          eval (x) False alist
     else
          (x)
     where x = UnaryOperation unarop ( eval (Leaf (Variable a)) True alist )

eval (UnaryOperation unarop (a)) bool alist =
     if bool == True then
          eval (x) False alist
     else
          (x)
     where x = UnaryOperation unarop (eval a True alist)

eval (BinaryOperation binarop (a) (Leaf (Variable b))) bool alist = 
     if bool == True then
          (eval x False alist)
     else
          (x)
     where x = BinaryOperation binarop (eval (a) True alist) (eval (Leaf (Variable b)) True alist)

eval (BinaryOperation binarop (Leaf (Variable a)) (b)) bool alist = 
     if bool == True then
          (eval x False alist)
     else
          (x)
     where x = BinaryOperation binarop (eval (Leaf (Variable a)) True alist) (eval (b) True alist)
     
eval (BinaryOperation binarop (Leaf (Constant a)) (Leaf (Constant b))) bool alist =
    if binarop == Plus then (Leaf (Constant (a+b)))
    else  (x) where x = Leaf (Constant (a*b))

eval (BinaryOperation binarop (a) (Leaf (Constant b))) bool alist = 
     if bool == True then
          (eval x False alist)
     else
          x
     where x = BinaryOperation binarop (eval (a) True alist) (Leaf (Constant b))

eval (BinaryOperation binarop (Leaf (Constant a)) (b)) bool alist =
     if bool == True then
          (eval x False alist)
     else
          x
     where x = BinaryOperation binarop (Leaf (Constant a)) (eval (b) True alist)

eval (BinaryOperation binarop (a) (b)) bool alist =
     if bool == True then
          (eval x False alist)
     else
          (x)
     where x = BinaryOperation binarop (eval (a) True alist) (eval (b) True alist)

-- getVars (20 points), return the variables contained
-- in the expression in a list (ordered, no duplicates)
getVars :: ExprV -> [String]
getVars exprv = sort (getVarsTmp exprv)

getVarsTmp :: ExprV -> [String]
getVarsTmp (Leaf (Variable a)) = [a]
getVarsTmp (Leaf (Constant a)) = []
getVarsTmp (UnaryOperation unarop (a)) = (getVarsTmp a)
getVarsTmp (BinaryOperation binarop (a) (b)) = union (getVarsTmp a) (getVarsTmp b)

-- evalDeriv (20 points), evaluate the first derivative
-- with respect to the variable given in the second
-- arg. using (var, value) pairs given in the first arg.
evalDeriv :: [(String, Int)] -> String -> ExprV -> Int
evalDeriv alist str (Leaf (Variable a)) =
    if a == str then 1 else 0

evalDeriv alist str (Leaf (Constant a)) = 0

evalDeriv alist str (UnaryOperation unarop (a)) = -(evalDeriv alist str a)

evalDeriv alist str (BinaryOperation binarop (a) (b)) =
    if binarop == Plus then
        (evalDeriv alist str a) + (evalDeriv alist str b)
    else 
        (evalDeriv alist str a)*(evalExpr alist b) + (evalDeriv alist str b)*(evalExpr alist a) 

-- Looks like that's all! 
