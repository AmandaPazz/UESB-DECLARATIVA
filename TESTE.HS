import Data.Char (toUpper)

tamanho :: [Int] -> Int
tamanho [] = 0;
tamanho (a : x) = 1 + tamanho x 

fatorialCauda :: Int -> Int
fatorialCauda num = fatorialCaudaAux num 1

fatorialCaudaAux :: Int -> Int -> Int
fatorialCaudaAux 0 acc = acc
fatorialCaudaAux num acc = fatorialCaudaAux (num - 1) (num *acc)

-- 1 1 2 3 5 8 13
-- 0 1 2 3 4 5 6

fibonacci :: Int -> [Int]
fibonacci n = fibonacciAux n 1 1
    where
        fibonacciAux 0 acc1 acc2 = [acc1]
        fibonacciAux n acc1 acc2 = acc1 : fibonacciAux (n-1) acc2 (acc1 + acc2)

somaLista :: [Int] -> Int
somaLista lista = somaListaAux lista 0

somaListaAux :: [Int] -> Int -> Int
somaListaAux [] acc = acc
somaListaAux (a:x) acc = somaListaAux x (a+acc)

contarElementos :: [t] -> Int
contarElementos lista = contarElementosAux lista 0

contarElementosAux :: [t] -> Int -> Int
contarElementosAux [] acc = acc
contarElementosAux (a:x) acc = contarElementosAux x (acc + 1)

produtoLista :: [Int] -> Int
produtoLista lista = produtoListaAux lista 1
    where
        produtoListaAux [] acc = acc
        produtoListaAux (a:x) acc = produtoListaAux x (acc * a)


maximoLista :: [Int] -> Int
maximoLista lista = maximoListaAux lista 0
    where
        maximoListaAux [] acc = acc
        maximoListaAux (a:x) acc = maximoListaAux x (if a>= acc then a else acc)

reverter :: String -> String
reverter [] = []
reverter (a : x) = reverter x ++ [a]

getNumChar :: String -> Int
getNumChar [] = 0
getNumChar (a : x) = 1 + getNumChar x

max3 :: Int -> Int -> Int -> ((Int, Int), (Int, Int))
max3 x y z = ((maior, ocorr maior [x,y,z]), (menor, ocorr menor [x,y,z]))
    where
        maior = max x (max y z)
        menor = min x (min y z)

ocorr :: Int -> [Int] -> Int
ocorr _ [] = 0
ocorr maior (a:x)
    | maior == a = 1 + ocorr maior x
    | otherwise = ocorr maior x


-- Função para contar o número de palavras em uma string
contPalavras :: String -> Int
contPalavras str = contar (splitWord str)
    where
        -- Função que divide a string em palavras
        splitWord :: String -> [String]
        splitWord [] = []
        splitWord s = getWord s : splitWord (dropWord s)

        -- Função para obter a primeira palavra da string
        getWord :: String -> String
        getWord [] = []
        getWord (x:xs)
            | x == ' '  = []
            | otherwise = x : getWord xs

        -- Função para remover a primeira palavra da string
        dropWord :: String -> String
        dropWord [] = []
        dropWord (x:xs)
            | x == ' '  = xs
            | otherwise = dropWord xs

        -- Função para contar o número de palavras em uma lista de palavras
        contar :: [String] -> Int
        contar [] = 0
        contar (_:xs) = 1 + contar xs

ordenada :: [Int] -> Bool
ordenada [] = True  -- Lista vazia é considerada ordenada
ordenada [_] = True  -- Lista com um único elemento é considerada ordenada
ordenada (x:y:xs)
  | x <= y    = ordenada (y:xs)  -- Verifica se o restante da lista está ordenado
  | otherwise = False  -- Se x > y, a lista não está ordenada

divisores :: Int -> [Int]
divisores n = [d | d <- [1..n-1], n `mod` d == 0]

main :: IO()
main = do
    putStrLn (show (divisores 512))