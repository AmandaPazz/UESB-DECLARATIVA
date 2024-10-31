{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use sum" #-}
{-# HLINT ignore "Use product" #-}


module LISTA4 where

import Data.Char hiding (isDigit, isLetter)
import Prelude hiding (concat, zip)


-- 1 Utilizando compreensão de listas

-- (a) Gere uma lista dos quadrados dos números pares de 1 a 20
quadradosPares :: [Int]
quadradosPares = gerarPares 2
  where
    gerarPares n
      | n > 20 = []
      | otherwise = (n ^ 2) : gerarPares (n + 2)

-- (b) Gere uma lista de todos os números ímpares de 1 a 50.
numerosImpares :: [Int]
numerosImpares = gerarImpares 1
  where
    gerarImpares n
      | n > 50 = []
      | otherwise = n : gerarImpares (n + 2)

-- (c) Gere uma lista de números de 1 a 100 que são múltiplos de 3 e 5.
listac :: [Int]
listac = gerar 1
  where
    gerar n
      | n > 100 = []
      | mod n 3 == 0 && mod n 5 == 0 = n : gerar (n + 1)
      | otherwise = gerar (n + 1)

-- (e) Gere uma lista de números palíndromos de 1 a 1000. Um número
-- palíndromo é aquele que é igual ao seu reverso
listae :: [Int]
listae = gerar 1
  where
    gerar n
      | n > 1000 = []
      | show n == reverse (show n) = n : gerar (n + 1)
      | otherwise = gerar (n + 1)

{-
map :: (t -> u) -> [t] -> [u]
map f [] = []
map f (a:x) = f a : map f x


map :: (t -> u) -> [t] -> [u]
map f l = [ f a | a <- l ]


fold :: (t -> t -> t) -> [t] -> t
fold f [a] = a
fold f (a:b:x) = f a (fold f (b:x))


filter :: (t -> Bool) -> [t] -> [t]
filter p [] = []
filter p (a:x)
| p a = a : filter p x
| otherwise = filter p x



filter :: (t -> Bool) -> [t] -> [t]
filter p l = [ a | a <- l, p a ]

composicao de funcao:
ghci> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c
-}

--2. Utilizando funções de alta ordem map, foldl, foldr e lter
--(a) Crie uma função que dobra cada elemento de uma lista
dobra :: [Int] -> [Int]
dobra = map (*2)

--(b) Escreva uma função que filtra apenas os números pares de uma
--lista de 1 a 20.
filtrapares :: [Int]
filtrapares = filter even [1..20]

--(c) Use foldl para somar todos os elementos de uma lista
somalista :: [Int] -> Int
somalista = foldl (+) 0

--(d) Combine map e filter para criar uma função que primeiro filtra os
--números pares e depois os dobra.
dobropar :: [Int] -> [Int]
dobropar l = map (*2) (filter even l)

--(e) Use foldl para calcular o produto de todos os elementos de uma lista.
produto :: [Int] -> Int
produto = foldl (*) 1

--(f) Escreva uma função que primeiro filtra os números maiores que 5 e depois adiciona 1 a cada um deles.
filtra5 :: [Int] -> [Int]
filtra5 l = map (+1) (filter (>5) l)

--3. Implemente uma função polimórca myAll que verica se todos os elementos de uma lista satisfazem um predicado.
myAll :: (a -> Bool) -> [a] -> Bool
myAll pred = foldr (\x acc -> pred x && acc) True

--4. Crie as seguintes funções polimórcas
--(a) Inverter uma lista
inverter :: [a] -> [a]
inverter [] = []
inverter (a:x) = inverter x ++ [a]

--(b) Remover o último elemento da lista
removerultimo :: [Int] -> [Int]
removerultimo [] = []
removerultimo [x] = []
removerultimo (a:x) = a : removerultimo x


--(c) Obter o segundo elemento da lista
segundoelemento :: [Int] -> Int
segundoelemento [] = 0
segundoelemento [x] = x
segundoelemento (a:b:x) = b

{-}
5. Avalie cada uma das expressões
(a) map odd [1,2,3,4,5]
[true, false, true, false, true]


(b) filter odd [1,2,3,4,5]
[1,3,5]


(c) filter (7<) [1,3..15]
[7,8,9,10,11,12,13,14,15]


(d) map (7:) [[2,3],[1,5,3]]
[[7,2,3], [7,1,5,3]]


(e) map (:[]) [1..5]
[[1],[2],[3],[4],[5]]


(f) map succ (filter odd [1..20])
[1,3,5,7,9,11,13,15,17,19]
[2,4,6,8,10,12,14,16,18,20]

(g) lter odd (map succ [1..20])
[3,5,7,9,11,13,15,17,19,21]

(h) foldr (-) 0 [8,7,6,5]
[8,-1,7,2]
2

(i) foldl (-) 0 [8,7,6,5]
[-8,-15,-21,-26]
-26

(j) foldr ((++) . map (* 2)) [] [[1,2,3],[4,5,6],[7,8,9]]
foldr (++) [] [2,4,6],[8,10,12],[14,16,18]
[2,4,6,8,10,12,14,16,18]


(k) foldr ((++) . reverse) [] ["haskell", "java", "python"]
foldr ((++)) [] ["lleksah", "avaj", "nohtyp"]
[lleksahavajnohtyp]


(l) map (map (+2)) [[1,2],[3,4,5,6],[7,8]]
[[3,4],[5,6,7,8],[9,10]]

-}

--6. Investigue o tipo e funcionamento da função concat em Haskell. Implemente essa função usando a função foldr.
concat :: [[a]] -> [a]
concat = foldr (++) []

--7. A função mapish recebe uma lista de funções e um único elemento x.
--Em seguida, ele retorna uma lista dos resultados da aplicação de cada
--função para x. Implemente a função mapish.
--ghci> mapish [(+1), (*3)] 10 
--[11, 30]

mapish :: [a -> b] -> a -> [b]
mapish fs x = map aplicar fs
  where
    aplicar f = f x




main :: IO ()
main = do
  putStrLn "\n\nQUESTAO 1 =  *****************************************************"
  putStrLn $ "a) A lista dos quadrados dos números pares de 1 a 20: " ++ show quadradosPares
  putStrLn $ "b) A lista de todos os números ímpares de 1 a 50: " ++ show numerosImpares
  putStrLn $ "c) A lista de números de 1 a 100 que são múltiplos de 3 e 5: " ++ show listac
  putStrLn $ "d) A lista de números de 1 a 100 que são divisíveis por 7:  " ++ show listae

  putStrLn "\n\nQUESTÃO 2 =  *****************************************************"
  putStrLn $ "a) Crie uma função que dobra cada elemento de uma lista:  " ++ show (dobra [1,2,3,4])
  putStrLn $ "b) Escreva uma função que filtra apenas os números pares de uma lista de 1 a 20.  " ++ show filtrapares
  putStrLn $ "c) Use foldl para somar todos os elementos de uma lista:  " ++ show (somalista [1,2,3,4])
  putStrLn $ "d) Combine map e filter para criar uma função que primeiro filtra os números pares e depois os dobra: " ++ show (dobropar [1,2,3,4])
  putStrLn $ "e) Use foldl para calcular o produto de todos os elementos de uma lista:  " ++ show (produto [1,2,3,4])
  putStrLn $ "f) Escreva uma função que primeiro filtra os números maiores que 5 e depois adiciona 1 a cada um deles: " ++ show (filtra5 [1,5,3,2,6,7,8])

  putStrLn "\n\nQUESTÃO 3 =  *****************************************************"
  putStrLn $ "Implemente uma função polimórca myAll que verica se todos os elementos de uma lista satisfazem um predicado:  " ++ show (myAll even [1,2,3,4])
  
  putStrLn "\n\nQUESTÃO 4 =  *****************************************************"
  putStrLn $ "a) Inverter uma lista:  " ++ show (inverter [1,2,3,4])
  putStrLn $ "b) Remover o último elemento da lista: " ++ show (removerultimo [1,2,3,4])
  putStrLn $ "c) Obter o segundo elemento da lista: " ++ show (segundoelemento [1,2,3,4])
  
  putStrLn "\n\nQUESTÃO 6 =  *****************************************************"
  putStrLn $ "Concat:  " ++ show (concat [[1, 2], [3, 4], [5]] )

  putStrLn "\n\nQUESTÃO 7 =  *****************************************************"
  putStrLn $ "Mapish:  " ++ show (mapish [(+1), (*3)] 10)

































