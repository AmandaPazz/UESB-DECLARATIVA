module Aula01 where

import Data.Char ()
-- not 
-- /\ (e) 
-- \/ (ou)
-- p --> q (se p então q)
-- p <--> q (p equivalente q) 

-- De Morgan
-- (p /\ q) <--> not (not p \/ not q)
 
f :: Bool -> Bool -> Bool
f p q = (p /\ q) <--> not (not p \/ not q)

ehTautologia :: String
ehTautologia = if f True True && f True False && f False True && f False False
               then "Tautologia"
               else "Nao eh tautologia"
 
(/\) :: Bool -> Bool -> Bool
(/\) a b = a && b

(\/) :: Bool -> Bool -> Bool
(\/) a b = a || b

(-->) :: Bool -> Bool -> Bool
(-->) a b = not a || b

(<-->) :: Bool -> Bool -> Bool
(<-->) a b = (a && b) || (not a && not b)

-- Int, Integer
-- Float, Double

type Base = Float
type Altura = Float
type Area = Float

area :: Base -> Altura -> Area
area b h =  (b*h) / 2.0

ehDigito :: Char -> Bool
ehDigito ch = (ch >= '0') && (ch <= '9')

juntar :: String -> String -> String
juntar s1 s2 = s1 ++ "----" ++ s2


todosIguais :: Int -> Int -> Int -> Bool
todosIguais x y z = (x == y) && (y == z)

testeTodosIguais :: Bool
testeTodosIguais =
   (todosIguais 2 2 2 == True) &&
   (todosIguais 2 3 4 == False) &&
   (todosIguais 2 2 3 == False) &&
   (todosIguais 3 2 2 == False) &&
   (todosIguais 2 3 2 == False)

quadrado :: Int -> Int
quadrado x = x*x

cubo :: Int -> Int
cubo x = quadrado x * x

maior :: Int -> Int -> Int
maior x y 
    | x >= y = x
    | x <  y = y

maior' :: Int -> Int -> Int
maior' x y
    | x >= y = x
    | otherwise = y

maior'' :: Int -> Int -> Int
maior'' x y =
    if x>=y then x else y

