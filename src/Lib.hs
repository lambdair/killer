module Lib
    ( Exp(..)
    , Formula(..)
    , cageFormula
    )
where

import Data.Char


data Exp = Var Int Int | Val Int | Plus [Exp]

data Formula = And [Formula]
             | Or [Formula]
             | Distinct [Exp]
             | Geq Exp Exp
             | Eq Exp Exp


instance Show Exp where
    show (Var i j)  = "x" ++ show i ++ show j
    show (Val  i)   = show i
    show (Plus es)  = "(+" ++ trim es ++ ")"

instance Show Formula where
    show (And fs)      = "(and" ++ trim fs ++ ")"
    show (Or fs)       = "(or" ++ trim fs ++ ")"
    show (Distinct es) = "(distinct" ++ trim es ++ ")"
    show (Geq e1 e2)   = "(>= " ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Eq  e1 e2)   = "(= " ++ show e1 ++ " " ++ show e2 ++ ")"


trim :: Show a => [a] -> String
trim []     = ""
trim (x:xs) = " " ++ show x ++ trim xs

cageFormula :: [[Int]] -> Formula
cageFormula xss = And (map (f . lIntToLExp) xss ++ max ++ min ++ distinct)
    where f [x, y] = Eq x y
          max      = [Geq (Val 9) (Var i j) | i <- [1 .. 9], j <- [1 .. 9]]
          min      = [Geq (Var i j) (Val 1) | i <- [1 .. 9], j <- [1 .. 9]]
          distinct = Distinct <$>
                ([[ Var i j | j <- [1 .. 9] ] | i <- [1 .. 9]]
              ++ [[ Var j i | j <- [1 .. 9] ] | i <- [1 .. 9]]
              ++ [[ Var i j | i <- [1 .. 3], j <- [1 .. 3] ]]
              ++ [[ Var i j | i <- [4 .. 6], j <- [1 .. 3] ]]
              ++ [[ Var i j | i <- [7 .. 9], j <- [1 .. 3] ]]
              ++ [[ Var i j | i <- [1 .. 3], j <- [4 .. 6] ]]
              ++ [[ Var i j | i <- [4 .. 6], j <- [4 .. 6] ]]
              ++ [[ Var i j | i <- [7 .. 9], j <- [4 .. 6] ]]
              ++ [[ Var i j | i <- [1 .. 3], j <- [7 .. 9] ]]
              ++ [[ Var i j | i <- [4 .. 6], j <- [7 .. 9] ]]
              ++ [[ Var i j | i <- [7 .. 9], j <- [7 .. 9] ]])

lIntToLExp :: [Int] -> [Exp]
lIntToLExp (x:xs) = Val x : [Plus (f xs)]
  where
      f []     = []
      f (x:xs) = let i = digitToInt $ show x !! 0
                     j = digitToInt $ show x !! 1
                 in Var i j : f xs
