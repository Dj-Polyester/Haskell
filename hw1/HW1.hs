module HW1 (
    form,
    constGrid,
    flatten,
    access,
    slice,
    vcat,
    hcat,
    without,
    matches2d
) where
import Data.List
-- do not modify the module declaration above!
-- this will ensure that you cannot load (compile)
-- the module without implementing all of the functions.

-- If you have functions you do not want to implement,
-- leave them as undefined or make them have another
-- default value. If you fully remove any of their definitions,
-- that will be a compilation error during evaluation,
-- and you will be eligible for (yay!) a 5 point deduction
-- (that's bad for your grade). Runtime errors in your code 
-- (rather than compilation errors) are acceptable and will simply
-- result in you getting zero from the specific test case causing
-- an error.

-------------------------
-- Fellowship of the Grid (25, 5, 5, 5 points)
form :: [a] -> (Int, Int) -> [[a]] 
form alist (row,column) 
                        |row==1 = [alist]
                        |otherwise = let firstPart = take column alist
                                         secondPart = drop column alist
                                     in (firstPart): form secondPart (row-1,column)

constGrid :: a -> (Int, Int) -> [[a]]
constGrid anumero (row,column) 
                        |row==1 = [bumero]
                        |otherwise = bumero: constGrid anumero (row-1,column)
                        where bumero = take column (repeat anumero)

flatten :: [[a]] -> [a]
flatten (firstlm:anestedlist) 
                            |null anestedlist = firstlm 
                            |otherwise = firstlm++flatten anestedlist 

access :: [[a]] -> (Int, Int) -> a
access agrid (i,j) = agrid !! i !! j
----------------------------
-- The Two Signatures (10, 5, 5, 10 points)
slice :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
slice (firstlm:agrid) (r1,r2) (c1,c2)
                        |(r1/=0 && r2==1) = [[]]
                        |r1/=0 && r2/=1 = slice agrid ((r1-1),(r2-1)) (c1,c2)
                        |r1==0 && r2/=1 = [slicy firstlm (c1,c2)] ++ (slice  agrid (0,(r2-1))) (c1,c2) 
                        |r1==0 && r2==1 = [slicy firstlm (c1,c2)]
                        where slicy alist (c1,c2) = drop c1 (take c2 alist)

vcat :: [[a]] -> [[a]] -> [[a]]
vcat list1 list2 = list1 ++ list2

hcat :: [[a]] -> [[a]] -> [[a]]
hcat bgrid1@(fst1:agrid1) bgrid2@(fst2:agrid2)
                            |(length bgrid1 /=1) && (length bgrid2 /= 1) 
                                =(fst1++fst2):((hcat agrid1) agrid2)
                            |(length bgrid1 ==1) && (length bgrid2 /=1) 
                                =(fst1++fst2):agrid2
                            |(length bgrid1 /=1) && (length bgrid2 ==1)  
                                =(fst1++fst2):agrid1
                            |otherwise = [fst1++fst2]

without :: [[a]] -> (Int, Int) -> (Int, Int) -> [[a]]
without (firstlm:agrid) (r1,r2) (c1,c2)
                        | r1/=0 && r2/=0 
                            = [slicy firstlm (c1,c2)] ++ without agrid (r1,r2) (c1,c2)
                        | r1==0 && r2/=0 
                            = (without agrid (0,(r2-1))) (c1,c2) 
                        | r1==0 && r2==0 && not (null agrid) 
                            = [slicy firstlm (c1,c2)] ++ without agrid (0,0) (c1,c2)
                        | r1==0 && r2==0 && null agrid 
                            = [slicy firstlm (c1,c2)]
                        where slicy firstlm (c1,c2) = (take c1 firstlm)++(drop c2 firstlm)
----------------------------
-- Return of the Non-trivial (30 points, 15 subject to runtime constraints)
matches2d :: Eq a => [[a]] -> [[a]] -> [(Int, Int)]
matches2d grid pattern = matches2dHelper pattern grid 0

matches1dOcc [] _ _ = []
matches1dOcc _ [] _ = []
matches1dOcc (patternFirst:apattern) (firstlm:alist) index = 
    if patternFirst==firstlm 
        then if null apattern
                 then [index]
             else if null alist
                 then []
             else ((matches1dOcc apattern) alist) index
    else []

matches1d _ [] _ = []
matches1d apattern fullList@(firstlm:alist) index = 
    (matches1dOcc apattern fullList index)++(matches1d apattern alist (index+1))



matches2dHelper _ [] _ = []
matches2dHelper [] _ _ = []
matches2dHelper fullPattern@(patternFirst:apattern) fullList@(firstlm:alist) index = 
    (map (\x -> (index,x)) indices) ++ (matches2dHelper fullPattern alist (index+1))
    where indices = matches2dOcc fullPattern fullList [] True 

matches2dOcc [] _ indices _ = indices
matches2dOcc _ [] _ _ = []
matches2dOcc (patternFirst:apattern) (firstlm:alist) indices started = 
    if started 
        then matches2dOcc apattern alist firstindices False
    else 
        matches2dOcc apattern alist newindices False
    where newindices = intersect indices firstindices
          firstindices = matches1d patternFirst firstlm 0


----------------------------
-- What is undefined? Just a value that will cause an error
-- when evaluated, from the GHC implementation:
-- undefined = error "Prelude.undefined"
-- But it allows your module to be compiled
-- since the function definitions will exist.

