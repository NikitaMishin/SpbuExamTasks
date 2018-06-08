module Solve where

import Data.List
import Data.Tree hiding (Tree )
import Data.List.Split
import Control.Monad
data Term = Prop String
    | Neg Term
    | And Term Term
    | Or Term Term
    | Flw Term Term deriving Eq

data Tree  = Branch String (Tree) (Tree) 
    | Leaf String
    | Branch1 String (Tree)  deriving (Eq,Ord,Show)
            
toDataTree (Leaf a) = Node a []
toDataTree (Branch1 b  s) = Node b [toDataTree s] 
toDataTree (Branch b cs ds) = Node b [toDataTree cs, toDataTree ds]

instance Show Term  where
    show (Prop x) = x 
    show (Neg x) = "("++"~"++show x++")" 
    show (And x y) =  "("++show x ++"&" ++ show y  ++")"
    show (Or x y) =  "("++ show x ++ "v" ++ show y  ++")"
    show (Flw x y) =  "("++show x ++"->" ++ show y  ++")"

        
output::[Term]->[Term]->String
output [] [] = undefined
output ls [] = foldr (\x xs-> (show x)++',':xs ) "" ls ++ "|-" 
output [] rs = "|-" ++ foldr (\x xs-> (show x)++',':xs ) "" rs    
output ls rs = foldr (\x xs-> (show x)++',':xs ) "" ls ++ "|-" ++ foldr (\x xs-> (show x)++',':xs ) "" rs


solveTree::[Term]->[Term]->Tree
solveTree [] [] = Leaf "tautology"
solveTree [] rs = if rs'==[] then Leaf (output [] rs) else  helperFalseTree (head rs') [] rs where
    rs' = filter (isNotProp) rs
solveTree ls [] = if ls'==[] then Leaf (output ls [] ) else helperTrueTree (head ls') ls [] where
      ls' = filter (isNotProp) ls

solveTree ls rs | intersect ls rs /= [] = Leaf ((output ls rs) ++ "-tautology") 
    | rs' == [] && ls'== [] =  Leaf (output ls rs)
    | rs' == [] && ls' /=[] = helperTrueTree (head ls') ls rs
    | rs' /=[] = helperFalseTree (head rs') ls rs where
        ls' = (filter (isNotProp) ls)
        rs' = (filter (isNotProp) rs)



isNotProp::Term->Bool
isNotProp (Prop x) = False
isNotProp _  = True


helperTrueTree:: Term->[Term]->[Term]->Tree
helperTrueTree (Neg x) ls rs   = Branch1 (output ls rs)  (solveTree (exclude ls (Neg x)) (x:rs)) 
helperTrueTree (And x y) ls rs = Branch1 (output ls rs) (solveTree (x : y : (exclude ls (And x y) ) )   rs) 
helperTrueTree (Or x y ) ls rs = Branch (output ls rs) (solveTree (x : (exclude ls  (Or x y) )) rs)  (solveTree (y: (exclude ls  (Or x y) )) rs )
helperTrueTree (Flw x y ) ls rs= Branch (output ls rs) (solveTree (exclude ls  (Flw x y) ) (x:rs))  (solveTree (y : (exclude ls  (Flw x y) ))rs)

helperFalseTree:: Term->[Term]->[Term]->Tree
helperFalseTree (Neg x) ls rs    = Branch1 (output ls rs) (solveTree (x:ls) (exclude rs (Neg x)))
helperFalseTree (And x y) ls rs  = Branch (output ls rs) (solveTree ls  (x:(exclude rs (And x y))))  (solveTree ls (y:(exclude rs (And x y))))
helperFalseTree (Or x y) ls rs   = Branch1 (output ls rs) (solveTree ls  (x:y:(exclude  rs (Or x y)))) 
helperFalseTree (Flw x y) ls rs  = Branch1 (output ls rs) (solveTree (x:ls) (y:(exclude rs (Flw x y)))) 


exclude::[Term]->Term->[Term]
exclude xs term = filter (\x-> x/=term) xs

solverTree term = do
    tree <- return $ solveTree [] [term]
    example <- return $ counterExample tree
    xs <-return  $ map (clear )  $ map (splitOn ",") $ splitOn "|-" example
    putStrLn $ example 
    putStrLn $ "Your input: "++ (show term)
    putStrLn $ "Output:\n"
    putStrLn  $ drawTree (toDataTree tree)
    putStrLn $ "Counter:"
    if length xs == 2 
    then do
        left <- return $ interpetate  (xs !! 0) "true"
        right<- return $ interpetate (xs!! 1 ) "false"
        forM_ left putStrLn
        forM_ right putStrLn
    else 
        putStrLn "That's tautology"


clear = filter(/="|") . filter(/="") . filter(/=" ")

isInfixOfNot  = not. (isInfixOf "tautology")

counterExample::Tree->String
counterExample (Leaf res) = if isInfixOfNot res then res else "tautology"
counterExample (Branch1 x subtree) = counterExample subtree
counterExample (Branch x left right ) | leftBranch /= "tautology" = leftBranch
    | rightBranch/="tautology" = rightBranch
    | otherwise = "tautology" where
        leftBranch = counterExample left
        rightBranch = counterExample right 

       




-- axioms examples
a1 = Or (Prop "a") (Neg (Prop "a")) 
a2 = Flw (Or (Prop "a")(Prop "b") ) (Or (Prop "a")(Prop "b")) 
a3 = Or (Flw (Prop "x") (Prop "y") )  (Flw (Prop "y") (Prop "x"))
a4 = Flw (Prop "l") (Flw (Prop "b") (Prop "l")) 

-- not axioms examples

akxiomS =  Flw (Flw (Prop "l") (Flw (Prop"b") (Prop "g")) )   (Flw (Flw (Prop"l") (Prop"b")) (Flw (Prop "l") (Prop "g")) )
n1 = Or  ( Neg (Flw (Prop "a") (Prop "b") ) )  (Prop ("b"))
n2 = Or    (Flw (Prop "a") (Prop "b") )   (Prop ("b"))
n3 = Or (Or (Neg ( Or (Prop "a")  (Prop "b")) ) (Flw (Prop "a") (Prop"c")) ) (Prop "d") 
n4 = Or (Neg (Or (Prop"a") (Prop "b") )) (Prop "d")

interpetate::[String]->String->[String]
interpetate [] _ = [""]
interpetate xs value =   map(\x->x++"="++value ) (nub xs) 
