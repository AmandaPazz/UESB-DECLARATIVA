module Lista02 where

import Data.Char (isDigit, toLower, toUpper)

-- 1 = Denir uma função para somar todos os pares de 1 até N.
somaPares :: Int -> Int
somaPares 0 = 0
somaPares n
  | n < 0 = error "Numero menor que 1"
  | even n = n + somaPares (n - 2)
  | odd n = somaPares (n - 1)

-- 2. Função para calcular x elevado a y (x^y)
potencia :: Int -> Int -> Int
potencia _ 0 = 1
potencia x y
  | y < 0 = error "expoente negativo"
  | otherwise = x * potencia x (y - 1)

-- 3. Função para somar todos os números em um intervalo entre A e B, inclusive
somaIntervalo :: Int -> Int -> Int
somaIntervalo a b
  | a > b = error "intervalo invalido"
  | a == b = a
  | otherwise = b + somaIntervalo a (b - 1)

-- 4. Função para somar os dígitos de um número inteiro positivo N
somaDigitos :: Int -> Int
somaDigitos 0 = 0
somaDigitos n
  | n < 0 = error "Numero negativo"
  | otherwise = mod n 10 + somaDigitos (div n 10)

-- 5. Função para somar os quadrados de todos os inteiros de 0 a N
somaQuadrados :: Int -> Int
somaQuadrados 0 = 0
somaQuadrados n
  | n < 0 = error "numero negativo"
  | otherwise = n * n + somaQuadrados (n - 1)

-- 6. Função para somar os fatoriais de todos os inteiros de 0 a N
somaFatoriais :: Int -> Int
somaFatoriais 0 = 1
somaFatoriais n
  | n < 0 = error "numero negativo"
  | otherwise = fatorial n + somaFatoriais (n - 1)

fatorial :: Int -> Int
fatorial 0 = 1
fatorial n = n * fatorial (n - 1)

-- 7. Função para calcular uma aproximação de ? usando a série de Leibniz
approxPi :: Integer -> Double
approxPi 0 = 4.0
approxPi n = 4 * ((-1) ^ n / fromIntegral (2 * n + 1)) + approxPi (n - 1)

-- 8. Função para somar dois números utilizando a função sucessor
somaComSucessor :: Int -> Int -> Int
somaComSucessor x y
  | y == 0 = x
  | otherwise = somaComSucessor (succ x) (pred y)

-- 9. Função para multiplicar dois números utilizando a função soma
multiplicaComSoma :: Int -> Int -> Int
multiplicaComSoma x y
  | y == 0 = x
  | otherwise = x + multiplicaComSoma x (y - 1)

-- 10. Função que determina quantas vezes um dígito K ocorre em um número natural N
contaDigito :: Int -> Int -> Int
contaDigito _ 0 = 0
contaDigito k n
  | k == mod n 10 = 1 + contaDigito k (div n 10)
  | otherwise = contaDigito k (div n 10)

-- 11. Função para converter um número decimal em binário
decimalParaBinario :: Int -> String
decimalParaBinario 0 = "0"
decimalParaBinario n
  | n < 0 = error "numero negativo"
  | n == 0 = ""
  | otherwise = decimalParaBinario (div n 2) ++ show (mod n 2)

-- 12. Função para concatenar uma palavra N vezes
concatenaPalavra :: String -> Int -> String
concatenaPalavra _ 0 = ""
concatenaPalavra palavra n = palavra ++ concatenaPalavra palavra (n - 1)

-- 13. Explicação sobre recursão de cauda
-- Recursão de cauda ocorre quando a chamada recursiva é a última coisa que acontece na função.
-- Isso permite otimizações no tempo de execução, evitando a criação de novas pilhas de chamadas.

-- 14. Função bonacci que calcula o n-ésimo número da sequência de Fibonacci usando recursão de cauda
fibonacci :: Int -> Int
fibonacci n = fiboAux n 0 1

fiboAux :: Int -> Int -> Int -> Int
fiboAux 0 a _ = a
fiboAux n a b = fiboAux (n - 1) b (a + b)

-- 15. Função para calcular o máximo divisor comum entre dois números usando o algoritmo de Euclides
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

-- 16. Função para receber um número N e devolver a tabuada de N
tabuada :: Int -> [String]
tabuada n = [show n ++ " x " ++ show i ++ " = " ++ show (n * i) ++ "  |  " | i <- [1..10]]

-- 17. Função para calcular o valor de e com n termos usando a série de Taylor
calculaE :: Int -> Double
calculaE 0 = 1.0
calculaE n = 1.0 / fromIntegral (fatorial n) + calculaE (n-1)

-- 18. Função para montar uma tabela com os valores de e utilizando de 1 até t termos
tabelaE :: Int -> [(Int, Double)]
tabelaE t = [(i, calculaE i) | i <- [1..t]]

-- 19. Função para calcular a soma dos primeiros n termos da série de Taylor para o seno
taylorSin :: Double -> Integer -> Double
taylorSin _ 0 = 0.0
taylorSin x n = ((-1) ^ n) * x^(2 * n + 1) / fromIntegral (fatorial (2 * fromIntegral n + 1)) + taylorSin x (n-1)

-- 20. Função para calcular a soma dos primeiros n termos da série de Taylor para o cosseno
taylorCos :: Double -> Integer -> Double
taylorCos _ 0 = 1.0
taylorCos x n = ((-1) ^ n) * x^(2 * n) / fromIntegral (fatorial (2 * fromIntegral n)) + taylorCos x (n-1)

-- 21. Função para calcular a média das vendas e o desvio padrão
tamanhoLinha :: Int
tamanhoLinha = 30

relatorio :: Int -> IO()
relatorio n = putStrLn ("\n\n" ++ cabecalho ++ imprimirLinhas n ++ rodape n ++ "\n\n")

rodape :: Int -> String
rodape n = "\n" ++ impSimbolo tamanhoLinha "-" ++ "\n" ++ imprimirSoma n
               ++ "\n" ++ imprimirMaior n ++ "\n" ++ imprimirMedia n
               ++ "\n" ++ imprimirDesvioPadrao n

imprimirSoma :: Int -> String
imprimirSoma n = "Soma = " ++ show (somavendas n)

imprimirMaior :: Int -> String
imprimirMaior n = "Maior = " ++ show (maiorVenda n)

imprimirMedia :: Int -> String
imprimirMedia n = "Media = " ++ show (mediaVendas n)

imprimirDesvioPadrao :: Int -> String
imprimirDesvioPadrao n = "Desvio Padrao = " ++ show (desvioPadraoVendas n)

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

-- Funções para calcular média e desvio padrão

mediaVendas :: Int -> Double
mediaVendas n = fromIntegral (somavendas n) / fromIntegral (n + 1)

desvioPadraoVendas :: Int -> Double
desvioPadraoVendas n = sqrt (variancia / fromIntegral (n + 1))
  where
    media = mediaVendas n
    variancia = sum [(fromIntegral (vendas i) - media) ^ 2 | i <- [0..n]]

-- Funções de soma de vendas

somavendas :: Int -> Int
somavendas n
   | n == 0 = vendas 0
   | otherwise = vendas n + somavendas (n-1)

-- Definindo a função recursiva para cálculo da soma das vendas

somavendas' :: Int -> Int
somavendas' 0 = vendas 0
somavendas' n = vendas n + somavendas' (n-1)



-- Exemplos de uso
main :: IO ()
main = do
  putStrLn "\n\nQUESTAO 1 =  *****************************************************"
  putStrLn $ "A soma dos pares do 10 eh: " ++ show (somaPares 10)
  putStrLn $ "A soma dos pares do 11 eh: " ++ show (somaPares 11)
  putStrLn "\n\nQUESTAO 2: *****************************************************"
  putStrLn $ "2 elevado a 3 eh: " ++ show (potencia 2 3)
  putStrLn "\n\nQUESTAO 3: *****************************************************"
  putStrLn $ "A soma dos numeros entre 1 e 3 eh: " ++ show (somaIntervalo 1 3)
  putStrLn "\n\nQUESTAO 4: *****************************************************"
  putStrLn $ "A soma dos digitos do numero 553 eh " ++ show (somaDigitos 553)
  putStrLn "\n\nQUESTAO 5: *****************************************************"
  putStrLn $ "A soma dos quadrado do 0 ao 3 eh  " ++ show (somaQuadrados 3)
  putStrLn "\n\nQUESTAO 6: *****************************************************"
  putStrLn $ "A soma dos fatoriais ate o 3: " ++ show (somaFatoriais 3)
  putStrLn "\n\nQUESTAO 7: *****************************************************"
  putStrLn $ "A aproximacao de pi usando a serie de Leibniz ate o 150: " ++ show (approxPi 150)
  putStrLn "\n\nQUESTAO 8: *****************************************************"
  putStrLn $ "Soma com sucessor de 5 e 6: " ++ show (somaComSucessor 5 6)
  putStrLn "\n\nQUESTAO 9: *****************************************************"
  putStrLn $ "Multipicacao com soma de 2x3: " ++ show (multiplicaComSoma 2 3)
  putStrLn "\n\nQUESTAO 10: *****************************************************"
  putStrLn $ "Contar digitos 3 do numero 121233345454: " ++ show (contaDigito 3 121233345454)
  putStrLn "\n\nQUESTAO 11: *****************************************************"
  putStrLn $ "13 em binario eh: " ++ show (decimalParaBinario 13)
  putStrLn "\n\nQUESTAO 12: *****************************************************"
  putStrLn $ "Concatenar paz 3 vezes: " ++ show (concatenaPalavra "paz" 3)
  putStrLn "\n\nQUESTAO 14: *****************************************************"
  putStrLn $ "O 7 numero da sequencia de Fibonacci eh: " ++ show (fibonacci 7)
  putStrLn "\n\nQUESTAO 15: *****************************************************"
  putStrLn $ "O maximo divisor comum de 8 e 32 eh " ++ show (mdc 8 32)
  putStrLn "\n\nQUESTAO 16: *****************************************************"
  putStrLn $ "Tabuda de 9: " ++ show (concat (tabuada 9))
  putStrLn "\n\nQUESTAO 17: *****************************************************"
  putStrLn $ "Valor de e com 10 termos: " ++ show (calculaE 10)
  putStrLn "\n\nQUESTAO 18: *****************************************************"
  putStrLn "Tabela de valores de e ate 5 termos:"
  mapM_ print (tabelaE 5)
  putStrLn "\n\nQUESTAO 19: *****************************************************"
  putStrLn $ "Seno aproximado de Pi/4 com 10 termos: " ++ show (taylorSin (pi/4) 10)
  putStrLn "\n\nQUESTAO 20: *****************************************************"
  putStrLn $ "Cosseno aproximado de Pi/4 com 10 termos: " ++ show (taylorCos (pi/4) 10)
  putStrLn "\n\nQUESTAO 21: *****************************************************"
  putStrLn $ "Relatorio de vendas de 5 dias"
  relatorio 4