module HW2 (
    parse, -- reexport for easy terminal use
    foldAndPropagateConstants,
    assignCommonSubexprs,
    reducePoly
) where

import Expression
import Parser
import Data.List
import Pprint
-- Do not change the module definition and imports above! Feel free
-- to modify the parser, but please remember that it is just a helper
-- module and is not related to your grade. You should only submit
-- this file. Feel free to import other modules, especially Data.List!

eval :: ExprV -> Bool -> (String, ExprV) -> ExprV

eval (Leaf (Variable (a))) bool (c, Leaf (Constant (d))) =
    if a==c then Leaf (Constant (d)) else Leaf (Variable (a))  

eval (Leaf (Variable (a))) bool (c, d) = (Leaf (Variable (a)))

eval (Leaf (Constant (a))) bool (c, d) = Leaf (Constant (a))

eval (UnaryOperation unarop (Leaf (Constant a))) bool (c, d) =
     (Leaf (Constant (-a)))
eval (UnaryOperation unarop (Leaf (Variable a))) bool (c, d) =
     if bool == True then
          eval (x) False (c, d)
     else
          (x)
     where x = UnaryOperation unarop ( eval (Leaf (Variable a)) True (c, d) )

eval (UnaryOperation unarop (a)) bool (c, d) =
     if bool == True then
          eval (x) False (c, d)
     else
          (x)
     where x = UnaryOperation unarop (eval a True (c, d))

eval (BinaryOperation binarop (a) (Leaf (Variable b))) bool (c, d) = 
     if bool == True then
          (eval x False (c, d))
     else
          (x)
     where x = BinaryOperation binarop (eval (a) True (c, d)) (eval (Leaf (Variable b)) True (c, d))

     

eval (BinaryOperation binarop (Leaf (Variable a)) (b)) bool (c, d) = 
     if bool == True then
          (eval x False (c, d))
     else
          (x)
     where x = BinaryOperation binarop (eval (Leaf (Variable a)) True (c, d)) (eval (b) True (c, d))
     
eval (BinaryOperation binarop (Leaf (Constant a)) (Leaf (Constant b))) bool (c, d) =
    if binarop == Plus then (Leaf (Constant (a+b)))
    else  (x) where x = Leaf (Constant (a*b))

eval (BinaryOperation binarop (a) (Leaf (Constant b))) bool (c, d) = 
     if bool == True then
          (eval x False (c, d))
     else
          x
     where x = BinaryOperation binarop (eval (a) True (c, d)) (Leaf (Constant b))

eval (BinaryOperation binarop (Leaf (Constant a)) (b)) bool (c, d) =
     if bool == True then
          (eval x False (c, d))
     else
          x
     where x = BinaryOperation binarop (Leaf (Constant a)) (eval (b) True (c, d))

eval (BinaryOperation binarop (a) (b)) bool (c, d) =
     if bool == True then
          (eval x False (c, d))
     else
          (x)
     where x = BinaryOperation binarop (eval (a) True (c, d)) (eval (b) True (c, d))

foldAndPropagateConstants :: [(String, ExprV)] -> [(String, ExprV)]
foldAndPropagateConstants [] = []
foldAndPropagateConstants ((str,exprv):others) = 
     (str,x):(foldAndPropagateConstants y)
     where x = eval exprv True ("",Leaf (Variable ""))
           y = [(s,eval z True (str,x)) | (s,z) <- others]

assignCommonSubexprs :: ExprV -> ([(String, ExprV)], ExprV)
assignCommonSubexprs exprv = fst (transformExprs (([],exprv),0))

-- sum is length alist
transformExprs :: (([(String, ExprV)], ExprV),Int) -> (([(String, ExprV)], ExprV),Int)
transformExprs ((alist, exprv),sum) = 
     if null x then ((alist, exprv), sum) else transformExprs ((s,newexprv), sum+len)
     where x = refineCommonExprs exprv sum
           s = alist ++ x
           len = length x
           newexprv = assignCommonExprs exprv x 

--assign values to the exps
assignCommonExprs :: ExprV -> [(String, ExprV)] -> ExprV
assignCommonExprs a [] = a
assignCommonExprs a (firs:b) = assignCommonExprs x b
     where x = (assignCommonExpr a) firs

--refine the list
refineCommonExprs :: ExprV -> Int -> [(String, ExprV)]
refineCommonExprs a i = tuplify e i
    where e = findCommonExprs a
          tuplify [] b = []
          tuplify (x:xs) b = 
              if elem x xs then tuplify xs b
              else ("$"++ (show b),x):(tuplify xs (b+1))
          findCommonExprs (Leaf (a)) = []
          findCommonExprs (UnaryOperation unarop (a)) = findCommonExprs (a)
          findCommonExprs (BinaryOperation binarop (a) (b)) = 
               x ++ (findCommonExprs a) ++ (findCommonExprs b)
               where x = intersect (listify a) (listify b)

assignCommonExpr :: ExprV -> (String, ExprV) -> ExprV
assignCommonExpr e@(Leaf a) (c,d) =
      if e == d then Leaf (Variable c) else e

assignCommonExpr e@(UnaryOperation unarop (Leaf a)) (c,d) =  
    if e == d then Leaf (Variable c) else e
assignCommonExpr (UnaryOperation unarop (a)) c = 
     (UnaryOperation unarop (assignCommonExpr a c))

assignCommonExpr e@(BinaryOperation binarop (Leaf a) (Leaf b)) (c,d)= 
     if e == d then Leaf (Variable c) else e
assignCommonExpr (BinaryOperation binarop (a) (Leaf b)) c = 
     (BinaryOperation binarop (assignCommonExpr a c) (Leaf b))
assignCommonExpr (BinaryOperation binarop (Leaf a) (b)) c = 
     (BinaryOperation binarop (Leaf a) (assignCommonExpr b c))

assignCommonExpr (BinaryOperation binarop (a) (b)) c=
     BinaryOperation binarop (assignCommonExpr a c) (assignCommonExpr b c)

listify :: ExprV -> [ExprV]
listify (Leaf a) = []

listify (UnaryOperation unarop (Leaf a)) =  
     [(UnaryOperation unarop (Leaf a))]
listify (UnaryOperation unarop (a)) = 
     listify a

listify (BinaryOperation binarop (Leaf a) (Leaf b)) = 
     [(BinaryOperation binarop (Leaf a) (Leaf b))]
listify (BinaryOperation binarop (a) (Leaf b)) = 
     listify (a)
listify (BinaryOperation binarop (Leaf a) (b)) = 
     listify (b)
listify (BinaryOperation binarop (a) (b)) = 
     (listify a) ++ (listify b)

reducePoly :: ExprV -> ExprV
reducePoly s@(Leaf a) = (Leaf a)
reducePoly s@(UnaryOperation unarop (a)) =
     polyNegate x 
     where x = reducePoly a 
reducePoly s@(BinaryOperation Times a (Leaf (Constant (-1)))) =
     polyNegate x
     where x = reducePoly a
reducePoly s@(BinaryOperation Times (Leaf (Constant (-1))) a) =
     polyNegate x 
     where x = reducePoly a    
reducePoly s@(BinaryOperation binarop a b) =
     if binarop == Plus then (polySum x y) else (polyMult x y) 
     where x = reducePoly (a)
           y = reducePoly (b)

--negate polynomial
polyNegate :: ExprV -> ExprV

polyNegate (Leaf (Constant a)) = (Leaf (Constant (-a)))
polyNegate (Leaf (Variable a)) = UnaryOperation Minus (Leaf (Variable a))
polyNegate (UnaryOperation unarop a) = a 

polyNegate (BinaryOperation binarop a b) = 
     if binarop == Plus then 
          BinaryOperation Plus x y 
     else 
          BinaryOperation Times x b
     where x = polyNegate a
           y = polyNegate b

--sum of two polynomials
polySum :: ExprV -> ExprV -> ExprV
--special cases
polySum (Leaf (Constant 0)) (a) = a
polySum (a) (Leaf (Constant 0)) = a

polySum (BinaryOperation Times (Leaf (Constant a)) (Leaf (Variable b))) (Leaf (Variable c)) =
     BinaryOperation Times (Leaf (Constant (a+1))) (Leaf (Variable b))
polySum (Leaf (Variable c)) (BinaryOperation Times (Leaf (Constant a)) (Leaf (Variable b))) =
     BinaryOperation Times (Leaf (Constant (a+1))) (Leaf (Variable b))

polySum (BinaryOperation Times (Leaf (Constant a)) (Leaf (Variable b))) (UnaryOperation unarop (Leaf (Variable c))) =
     BinaryOperation Times (Leaf (Constant (a-1))) (Leaf (Variable b))
polySum (UnaryOperation unarop (Leaf (Variable c))) (BinaryOperation Times (Leaf (Constant a)) (Leaf (Variable b))) =
     BinaryOperation Times (Leaf (Constant (a-1))) (Leaf (Variable b))

--var-const, const-const, var- var
polySum (Leaf (Variable a)) (Leaf (Constant b)) = 
     BinaryOperation Plus (Leaf (Constant b))  (Leaf (Variable a)) 
polySum (Leaf (Constant b)) (Leaf (Variable a)) = 
     BinaryOperation Plus (Leaf (Constant b))  (Leaf (Variable a))
polySum (Leaf (Constant a)) (Leaf (Constant b)) = (Leaf (Constant (a+b)))
polySum (Leaf (Variable a)) (Leaf (Variable b)) = 
     BinaryOperation Times (Leaf (Constant 2))  (Leaf (Variable a)) 

--uleaf-uleaf, uleaf-const, uleaf-var
polySum (UnaryOperation unarop1 (Leaf (Variable a))) (UnaryOperation unarop2 (Leaf (Variable b))) =
     (BinaryOperation Times (Leaf (Constant (-2))) (Leaf (Variable a)))
polySum (UnaryOperation unarop (Leaf (Variable a))) (Leaf (Constant b))=
     BinaryOperation Plus (Leaf (Constant b)) (UnaryOperation unarop (Leaf (Variable a)))
polySum (Leaf (Constant b)) (UnaryOperation unarop (Leaf (Variable a)))=
     BinaryOperation Plus (Leaf (Constant b)) (UnaryOperation unarop (Leaf (Variable a)))
polySum (Leaf (Variable b)) (UnaryOperation unarop (Leaf (Variable a)))=
     (Leaf (Constant 0))
polySum (UnaryOperation unarop (Leaf (Variable a))) (Leaf (Variable b))=
     (Leaf (Constant 0))

--binarop with leaf
polySum f@(BinaryOperation binarop c d) e@(Leaf a) =
     if binarop == Plus then
          if (varEqual d e == Equal) then 
               BinaryOperation Plus (c) (polySum d e)
          else if (varEqual d e == MinusEqual) then 
               (c)
          else if (varEqual d e == Less) then 
               BinaryOperation Plus (f) (e)
          else if (varEqual d e == Greater) then 
               BinaryOperation Plus (polySum c e) (d)
          else notImpl
     else BinaryOperation Plus (e) (f)

polySum e@(Leaf a) f@(BinaryOperation binarop c d) =
     if binarop == Plus then
          if (varEqual d e == Equal) then 
               BinaryOperation Plus (c) (polySum d e)
          else if (varEqual d e == MinusEqual) then 
               (c)
          else if (varEqual d e == Less) then 
               BinaryOperation Plus (f) (e)
          else if (varEqual d e == Greater) then 
               BinaryOperation Plus (polySum c e) (d)
          else notImpl
     else BinaryOperation Plus (e) (f)
--binarop with unaryleaf
polySum f@(BinaryOperation binarop c d) e@(UnaryOperation unarop(Leaf a)) =
     if binarop == Plus then
          if (varEqual d e == Equal) then 
               BinaryOperation Plus (c) (polySum d e)
          else if (varEqual d e == MinusEqual) then 
               (c)
          else if (varEqual d e == Less) then 
               BinaryOperation Plus (f) (e)
          else if (varEqual d e == Greater) then 
               BinaryOperation Plus (polySum c e) (d)
          else notImpl
     else BinaryOperation Plus (e) (f)

polySum e@(UnaryOperation unarop(Leaf a)) f@(BinaryOperation binarop c d) =
     if binarop == Plus then
          if (varEqual d e == Equal) then 
               BinaryOperation Plus (c) (polySum d e)
          else if (varEqual d e == MinusEqual) then 
               (c)
          else if (varEqual d e == Less) then 
               BinaryOperation Plus (f) (e)
          else if (varEqual d e == Greater) then 
               BinaryOperation Plus (polySum c e) (d)
          else notImpl
     else BinaryOperation Plus (e) (f)
--General
polySum e@(BinaryOperation binarop1 a b) f@(BinaryOperation binarop2 c d)
     | (binarop1 == Plus && binarop2 == Plus) =
          if (varEqual b d == Equal) then 
               BinaryOperation Plus (polySum a c) (polySum b d)
          else if (varEqual b d == MinusEqual) then 
               (polySum a c)
          else if (varEqual b d == Less) then 
               BinaryOperation Plus (polySum e c) (d)
          else if (varEqual b d == Greater) then 
               BinaryOperation Plus (polySum a f) (b)
          else notImpl
     | (binarop1 == Plus && binarop2 == Times) =
          if (varEqual b f == Equal) then 
               BinaryOperation Plus (a) (polySum b f)
          else if (varEqual b f == MinusEqual) then 
               (a)
          else if (varEqual b f == Less) then 
               BinaryOperation Plus (e) (f)
          else if (varEqual b f == Greater) then 
               BinaryOperation Plus (polySum a f) (b)
          else notImpl
     | (binarop1 == Times && binarop2 == Plus) =
          if (varEqual e d == Equal) then 
               BinaryOperation Plus (c) (polySum e d)
          else if (varEqual e d == MinusEqual) then 
               (c)
          else if (varEqual e d == Less) then 
               BinaryOperation Plus (polySum e c) (d)
          else if (varEqual e d == Greater) then 
               BinaryOperation Plus (f) (e)
          else notImpl
     | otherwise =
          if (varEqual e f == Equal) then 
               BinaryOperation Times (polySum a c) (b)
          else if (varEqual e f == MinusEqual) then 
               (Leaf (Constant 0))
          else if (varEqual e f == Less) then 
               BinaryOperation Plus (e) (f)
          else if (varEqual e f == Greater) then 
               BinaryOperation Plus (f) (e)
          else notImpl
--------------------------------------------------------------------------------------------
--product of two polynomials
polyMult :: ExprV -> ExprV -> ExprV
--special cases
polyMult (Leaf (Constant 0)) (a) = (Leaf (Constant 0))
polyMult (a) (Leaf (Constant 0)) = (Leaf (Constant 0))
polyMult (Leaf (Constant 1)) (a) = (a)
polyMult (a) (Leaf (Constant 1)) = (a)
polyMult (Leaf (Constant (-1))) (a) = polyNegate (a)
polyMult (a) (Leaf (Constant (-1))) = polyNegate (a)
--var-const, const-const, var- var
polyMult (Leaf (Variable a)) (Leaf (Constant b)) = 
     BinaryOperation Times (Leaf (Constant b))  (Leaf (Variable a)) 
polyMult (Leaf (Constant b)) (Leaf (Variable a)) = 
     BinaryOperation Times (Leaf (Constant b))  (Leaf (Variable a))
polyMult (Leaf (Constant a)) (Leaf (Constant b)) = (Leaf (Constant (a*b)))
polyMult (Leaf (Variable a)) (Leaf (Variable b)) =
     BinaryOperation Times (Leaf (Variable a)) (Leaf (Variable b))
--uleaf-uleaf, uleaf-const, uleaf-var
polyMult (UnaryOperation unarop1 d@(Leaf b)) (UnaryOperation unarop2 c@(Leaf a)) = polyMult c d

polyMult (Leaf (Constant b)) (UnaryOperation unarop c@(Leaf a)) =
     BinaryOperation Times (Leaf (Constant (-b))) c
polyMult (UnaryOperation unarop c@(Leaf a)) (Leaf (Constant b)) =
     BinaryOperation Times (Leaf (Constant (-b))) c

polyMult d@(Leaf (Variable b)) c@(UnaryOperation unarop (Leaf a)) =
     BinaryOperation Times c d
polyMult c@(UnaryOperation unarop (Leaf a)) d@(Leaf (Variable b)) =
     BinaryOperation Times c d
--binarop with leaf
polyMult f@(BinaryOperation binarop c d) e@(Leaf a) =
     if binarop == Plus then
          polySum (polyMult c e) (polyMult d e)
     else 
          BinaryOperation Times (polyMult c e) d

polyMult e@(Leaf a) f@(BinaryOperation binarop c d) =
     if binarop == Plus then
          polySum (polyMult c e) (polyMult d e)
     else 
          BinaryOperation Times (polyMult c e) d
--binarop with unaryleaf
polyMult f@(BinaryOperation binarop c d) e@(UnaryOperation unarop(Leaf a)) =
     if binarop == Plus then
          polySum (polyMult c e) (polyMult d e)   
     else 
          BinaryOperation Times (polyMult c e) d

polyMult e@(UnaryOperation unarop(Leaf a)) f@(BinaryOperation binarop c d) =
     if binarop == Plus then
          polySum (polyMult c e) (polyMult d e)
     else 
          BinaryOperation Times (polyMult c e) d
--General
polyMult e@(BinaryOperation binarop1 a b) f@(BinaryOperation binarop2 c d) =
     if (binarop1 == Plus && binarop2 == Plus) then
          polySum (polyMult (a) (polySum c d)) (polyMult (b) (polySum c d))
     else if (binarop1 == Plus && binarop2 == Times) then
          polySum (polyMult (a) (f)) (polyMult (b) (f))
     else if (binarop1 == Times && binarop2 == Plus) then
          polySum (polyMult e d) (polyMult e c)
     else 
          BinaryOperation Times (polyMult e c) d
--------------------------------------------------------------------------------------------
data Equality = Less | Equal | Greater | MinusEqual | None  deriving (Eq, Read, Show)
varEqual :: ExprV -> ExprV -> Equality

varEqual (BinaryOperation binarop1 a b) (BinaryOperation binarop2 c d) = varEqual a c
varEqual (Leaf (Constant a)) (Leaf (Constant b)) = 
     if (a+b)==0 then MinusEqual else Equal
varEqual (Leaf (Variable a)) (Leaf (Variable b)) = Equal

varEqual (a) (Leaf (Constant b)) = Greater
varEqual (Leaf (Constant b)) (a) = Less

varEqual (Leaf (Variable a)) (UnaryOperation unarop (Leaf (Variable b)))  = MinusEqual
varEqual (Leaf (Variable a)) (BinaryOperation binarop (Leaf (Constant c)) (Leaf (Variable b))) = Equal
varEqual c@(Leaf (Variable a)) (b) =
     if c==b then Equal else Less

varEqual (UnaryOperation unarop (Leaf (Variable b))) (Leaf (Variable a)) = MinusEqual
varEqual (BinaryOperation binarop (Leaf (Constant c)) (Leaf (Variable b))) (Leaf (Variable a)) = Equal
varEqual (b) c@(Leaf (Variable a)) =
     if c==b then Equal else Greater

varEqual (UnaryOperation unarop (Leaf (Variable a))) (BinaryOperation binarop (Leaf (Constant c)) (Leaf (Variable b))) = Equal
varEqual c@(UnaryOperation unarop (Leaf (Variable a))) (b) =
     if c==b then Equal else Less
varEqual (BinaryOperation binarop (Leaf (Constant c)) (Leaf (Variable b))) (UnaryOperation unarop (Leaf (Variable a)))  = Equal
varEqual (b) c@(UnaryOperation unarop (Leaf (Variable a))) =
     if c==b then Equal else Greater

varEqual d@(BinaryOperation binarop (Leaf (Constant a)) (Leaf (Variable b))) (c) =
     if d==c then Equal else Less
varEqual (c) d@(BinaryOperation binarop (Leaf (Constant a)) (Leaf (Variable b))) =
     if d==c then Equal else Greater

-- an extra dummy variable, so as to not crash the GUI
notImpl :: ExprV
notImpl = Leaf $ Variable "Not Implemented"

