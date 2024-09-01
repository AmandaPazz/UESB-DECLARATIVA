module Relatorio(relatorio) where

import Data.Char () 

tamanhoLinha :: Int
tamanhoLinha = 30

relatorio :: Int -> IO()
relatorio n = putStrLn ("\n\n" ++ cabecalho ++ imprimirLinhas n ++ rodape n ++ "\n\n")

rodape :: Int -> String
rodape n = "\n" ++ impSimbolo tamanhoLinha "-" ++ "\n" ++ imprimirSoma n
               ++ "\n" ++ imprimirMaior n

imprimirSoma :: Int -> String
imprimirSoma n = "Soma = " ++ show(somavendas n)


imprimirMaior :: Int -> String
imprimirMaior n = "Maior = " ++ show (maiorVenda n)

maiorVenda :: Int -> Int
maiorVenda 0 = vendas 0
maiorVenda n = max (vendas n) (maiorVenda (n-1))


titulo :: String
titulo = "Relatorio de Vendas"

cabecalho :: String
cabecalho = tracos ++ titulo ++ "\n" ++ tracos 
              where
                 tracos = impSimbolo tamanhoLinha "-" ++ "\n"
          

imprimirLinhas :: Int -> String
imprimirLinhas 0 = imprimirLinha 0 
imprimirLinhas n = imprimirLinhas (n-1) ++ "\n" ++ imprimirLinha n 

imprimirLinha :: Int -> String
imprimirLinha n = dia n ++ "\t" ++ show (vendas n)

impSimbolo :: Int -> String -> String
impSimbolo 0 s = ""
impSimbolo n s = s ++ impSimbolo (n-1) s


vendas :: Int -> Int
vendas n 
   | n == 0 = 23
   | n == 1 = 34
   | n == 2 = 58
   | n == 3 = 12
   | n == 4 = 56
   | otherwise = 0

dia :: Int -> String
dia 0 = "Segunda"
dia 1 = "Terca"
dia 2 = "Quarta"
dia 3 = "Quinta"
dia 4 = "Sexta"



-- Guardas
somavendas :: Int -> Int
somavendas n
   | n == 0 = vendas 0
   | otherwise = vendas n + somavendas (n-1)

-- pattern matching
somavendas' :: Int -> Int
somavendas' 0 = vendas 0
somavendas' n = vendas n + somavendas' (n-1)

------------------------------------

-- Fatorial - tail recursion

fatorial :: Int -> Int
fatorial n = fatAux n 1

fatAux :: Int -> Int -> Int
fatAux 0 acc = acc
fatAux n acc = fatAux (n-1) (n*acc)

fibo :: Int -> Int
fibo 1 = 0
fibo 2 = 1
fibo n = fibo (n-1) + fibo (n-2)

main :: IO()
main = do 
   relatorio 3