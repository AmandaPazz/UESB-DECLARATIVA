module Lista03 where

import Data.Char (ord, chr, isAlpha, isLower, isUpper, toUpper, isDigit, isSpace, isLetter)

-- 1. Funcao para calcular a distancia entre dois pontos
type Ponto = (Float, Float)

distancia :: Ponto -> Ponto -> Float
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- 2. Elaborar uma funcao que teste se um ponto esta ou nao dentro de um circulo.
type Circulo = (Centro, Raio)
type Centro = Ponto
type Raio = Float

dentroDoCirculo :: Ponto -> Circulo -> Bool
dentroDoCirculo ponto (centro, raio) = distancia ponto centro <= raio

-- 3. Definir uma funcao minmax que calcula o minimo e o maximo de tres inteiros.
minmax :: Int -> Int -> Int -> (Int, Int)
minmax a b c = (minimum [a, b, c], maximum [a, b, c])

-- 4. Definir uma funcao maxocorre que calcula o maximo de tres numeros juntamente com o numero de ocorrencias do maximo.
maxocorre :: Int -> Int -> Int -> (Int, Int)
maxocorre a b c = (maximum [a, b, c], countMaximum)
    where
        countMaximum = length (filter (== maximum [a, b, c]) [a, b, c])

-- 5. Elaborar uma funcao para calcular o tamanho de uma lista de inteiros.
tamanhoListaInteiros :: [Int] -> Int
tamanhoListaInteiros a = length a

-- 6. Elaborar uma funcao para somar os elementos de uma lista de inteiros.
somarListaInteiros :: [Int] -> Int
somarListaInteiros [] = 0
somarListaInteiros (a : x) = a + somarListaInteiros x

-- 7. Elaborar uma funcao para somar o dobro dos elementos de uma lista de inteiros.
somarDobroInteiros :: [Int] -> Int
somarDobroInteiros [] = 0
somarDobroInteiros (a : xs) = 2 * a + somarDobroInteiros xs

-- 8. Elaborar uma funcao para verificar se um elemento e membro de uma lista de inteiros.
ehMembro :: Int -> [Int] -> Bool
ehMembro elem [] = False
ehMembro elem (a : x)
    | a == elem = True
    | otherwise = ehMembro elem x

-- 9. Ordenar uma lista de inteiros utilizando o metodo Insert Sort.
insertSort :: [Int] -> [Int]
insertSort [] = []
insertSort (x:xs) = insere x (insertSort xs)
  where
    insere x [] = [x]
    insere x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insere x ys

-- 10. Ordenar uma lista de inteiros utilizando o metodo Selection Sort.
selectionSort :: [Int] -> [Int]
selectionSort [] = []
selectionSort xs = minElem : selectionSort (remove minElem xs)
  where
    minElem = minimum xs
    remove _ [] = []
    remove y (z:zs)
      | y == z    = remove y zs
      | otherwise = z : remove y zs

-- 11. Ordenar uma lista de inteiros utilizando o metodo MergeSort.
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort left) (mergeSort right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    merge xs [] = xs
    merge [] ys = ys
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

-- 12. Ordenar uma lista de inteiros utilizando o metodo QuickSort.
quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort (pivo:xs) = quickSort [x | x <- xs, x <= pivo] ++ [pivo] ++ quickSort [x | x <- xs, x > pivo]

-- 13. Elaborar uma função para contar o número de vogais de uma string.
contVogais :: String -> Int
contVogais [] = 0
contVogais (a : x)
    | a `elem` "aeiouAEIOU" = 1 + contVogais x
    | otherwise = contVogais x

-- 14. Elaborar uma função que receba uma frase e devolva a quantidade de palavras na frase.
contPalavras :: String -> Int
contPalavras frase = length (words  frase)


-- 15. Elaborar uma função para criptografar uma mensagem utilizando a cifra de César.
cifraCesar :: Int -> String -> String
cifraCesar deslocamento mensagem = map (cifraChar deslocamento) mensagem

-- Função auxiliar para criptografar um caractere
cifraChar :: Int -> Char -> Char
cifraChar deslocamento c
    | isLower c = deslocar 'a' c deslocamento
    | isUpper c = deslocar 'A' c deslocamento
    | otherwise = c

-- Função auxiliar para deslocar um caractere, considerando o início do alfabeto
deslocar :: Char -> Char -> Int -> Char
deslocar base c deslocamento =
    chr ( ord base + ((ord c - ord base + deslocamento) `mod` 26))

-- 16. Elaborar uma função para inverter uma string.
inverter :: String -> String
inverter [] = []
inverter (a : x) =  inverter x ++ [a]

-- 17. Elaborar uma função que receba uma frase com letras minúsculas e converta a primeira letra de cada palavra da frase para maiúscula
primeiraMaiuscula :: String -> String
primeiraMaiuscula [] = []
primeiraMaiuscula (a : x) = toUpper a : capitalize x
    where 
        capitalize [] = []
        capitalize (' ' : x) = ' ' : primeiraMaiuscula x
        capitalize (a : x) = a : capitalize x

-- 18. Elaborar uma função para implementar a busca binária em uma lista de inteiros.
buscaBinaria :: Int -> [Int] -> Bool
buscaBinaria _ [] = False
buscaBinaria elem lista
    | elem == meio = True
    | elem < meio = buscaBinaria elem primeiraMetade
    | otherwise = buscaBinaria elem segundaMetade
  where
    meio = lista !! (length lista `div` 2)
    primeiraMetade = take (length lista `div` 2) lista
    segundaMetade = drop (length lista `div` 2 + 1) lista

-- 20. Elaborar uma função para computar o maior e o menor elemento de uma lista de inteiros.
minMaxElem :: [Int] -> (Int, Int)
minMaxElem lista = (minimum lista, maximum lista)

-- 21.  Função para contar o número de letras e palavras de uma frase
contCharString :: String -> (Int, Int)
contCharString frase = (contarLetras frase, contarPalavras frase)
  where
    contarLetras :: String -> Int
    contarLetras = length . filter isLetter
    
    contarPalavras :: String -> Int
    contarPalavras = length . words

-- 22. Elaborar uma função para vericar se uma string é uma palíndroma.
ehPalindromo :: String -> Bool
ehPalindromo str = inverter str == str

-- 23. Elaborar uma função que receba um nome completo de uma pessoa e mostre os nomes intermediários entre o primeiro e o último sobrenome
     --abreviados.
abreviar :: String -> String
abreviar nome = unwords (primeiroNome : abreviados ++ [ultimoNome])
    where
        palavras = words nome
        primeiroNome = primeiraMaiuscula (head palavras)
        ultimoNome = primeiraMaiuscula (last palavras)
        intermediarios = init (tail palavras)
        abreviados = map (\x -> toUpper (head x) : ".") intermediarios


-- 24. Elaborar uma função que determine se um número é perfeito, ou seja, é igual a soma dos seus divisores (exceto o próprio).
ehPerfeito :: Int -> Bool
ehPerfeito n = sum ([x | x <- [1..n-1], n `mod` x == 0]) == n

--25. Elaborar uma função que intercala duas lista de inteiros ordenadas.
intercala :: [Int] -> [Int] -> [Int]
intercala [] x = x
intercala a [] = a
intercala (a : x) (as : xs)
    |a <= as = a : intercala x (as : xs)
    | otherwise = as : intercala (a : x) xs


-- 26. Uma função que devolve True se uma dada lista tem mais que 10 elementos, False caso contrário.
maisQueDez :: [a] -> Bool
maisQueDez a = length a > 10 

-- 27. Utilizando recursão por cauda (tail recursion) criar uma função para somar os elementos de uma lista de inteiros.
somaLista :: [Int] -> Int
somaLista lista = somaAux lista 0

somaAux :: [Int] -> Int -> Int
somaAux [] acc = acc;
somaAux (a : x) acc = somaAux x (acc + a)

-- 28. Quantos elementos tem cada uma das seguintes listas?

-- a) ['a','b']
-- R: 2 elementos. A lista contém os caracteres 'a' e 'b'.

-- b) [['a','b']]
-- R: 1 elemento. A lista contém uma única lista ['a', 'b'].

-- c) [['a','b'], ['c','d']]
-- R: 2 elementos. A lista contém duas listas ['a','b'] e ['c','d'].

-- d) [[['a','b']]]
-- R: 1 elemento. A lista contém uma única lista que, por sua vez, contém outra lista ['a','b'].

-- e) []
-- R: 0 elementos. É uma lista vazia.

-- f) [[]]
-- R: 1 elemento. A lista contém uma única lista vazia.

-- g) [[],[]]
-- R: 2 elementos. A lista contém duas listas vazias.

--29. Baseado no banco de dados abaixo, elabore as seguintes funções:
--(a) Obter o número de doutores do banco de dados
--(b) Obter o número de mulheres
--(c) Obter o número de mulheres que são mestres
--(d) Obter o nome de todos os doutores

type Matricula = Int
type Nome = String
type Titulacao = String
type Sexo = Char
banco :: Int -> (Matricula,Nome,Titulacao,Sexo)
banco matricula
  | matricula == 1 = (1,"Roque","Doutor",'M')
  | matricula == 2 = (2,"Alzira","Doutor",'F')
  | matricula == 3 = (3,"Helio","Doutor",'M')
  | matricula == 4 = (4,"Maisa","Doutor",'F')
  | matricula == 5 = (5,"Carlos","Mestre",'M')
  | matricula == 6 = (6,"Rita","Mestre",'F')
  | otherwise = (0,"","",' ')

numMatriculasValidas :: Int
numMatriculasValidas = contarMatriculas 1
  where
    contarMatriculas n
      | matricula == 0 = n - 1 
      | otherwise      = contarMatriculas (n + 1)
      where (matricula,_,_,_) = banco n

getNumeroDoutores :: Int
getNumeroDoutores = contarDoutores numMatriculasValidas
  where
    contarDoutores 0 = 0
    contarDoutores n
      | titulacao == "Doutor" = 1 + contarDoutores (n-1)
      | otherwise             = contarDoutores (n-1)
      where (_,_,titulacao,_) = banco n

getNumeroMulheres :: Int
getNumeroMulheres = contarMulheres numMatriculasValidas
  where
    contarMulheres 0 = 0
    contarMulheres n 
      | sexo == 'M' = 1 + contarMulheres (n-1)
      |otherwise = contarMulheres (n-1)
      where
        (_,_,_, sexo) = banco n


getNumeroMulheresMestres :: Int
getNumeroMulheresMestres = contarMulheresMestres numMatriculasValidas
  where
    contarMulheresMestres 0 = 0
    contarMulheresMestres n 
      | sexo == 'M'  && titulacao == "Mestre" = 1 + contarMulheresMestres (n-1)
      | otherwise = contarMulheresMestres (n-1)
      where
        (_,_,titulacao, sexo) = banco n    

getNomeDoutores :: [Nome]
getNomeDoutores = buscarDoutores numMatriculasValidas
  where
    buscarDoutores 0 = []
    buscarDoutores n 
      | titulacao == "Doutor" = nome : buscarDoutores (n-1)
      | otherwise = buscarDoutores (n-1)
      where
        (_,nome,titulacao,_) = banco n       
   

main :: IO ()
main = do
    putStrLn "Teste das funcoes da lista 3:"
    putStrLn "\nQUESTAO 1 ********************************"
    let p1 = (3, 0)
    let p2 = (0, 4)
    putStrLn $ "Distancia entre " ++ show p1 ++ " e " ++ show p2 ++ ": " ++ show (distancia p1 p2)

    putStrLn "\nQUESTAO 2 ********************************"
    let circulo = ((0, 0), 5)
    let ponto = (3, 4)
    putStrLn $ "Ponto " ++ show ponto ++ " esta dentro do circulo: " ++ show (dentroDoCirculo ponto circulo)

    putStrLn "\nQUESTAO 3 ********************************"
    let (minimo, maximo) = minmax 3 6 1
    putStrLn $ "Minimo e maximo de 3, 6 e 1: " ++ show (minimo, maximo)
    let (minimo2, maximo2) = minmax (-5) 10 0
    putStrLn $ "Minimo e maximo de -5, 10 e 0: " ++ show (minimo2, maximo2)

    putStrLn "\nQUESTAO 4 ********************************"
    let (maximo1, ocorrencias1) = maxocorre 3 1 3
    putStrLn $ "Maximo e ocorrencias de 3, 1 e 3: " ++ show (maximo1, ocorrencias1)
    let (maximo2, ocorrencias2) = maxocorre 5 5 5
    putStrLn $ "Maximo e ocorrencias de 5, 5 e 5: " ++ show (maximo2, ocorrencias2)
    let (maximo3, ocorrencias3) = maxocorre 1 2 3
    putStrLn $ "Maximo e ocorrencias de 1, 2 e 3: " ++ show (maximo3, ocorrencias3)

    putStrLn "\nQUESTAO 5 ********************************"
    let lista = [1, 2, 3, 4, 5, 6, 7]
    putStrLn $ "Tamanho da lista " ++ show lista ++ ": " ++ show (tamanhoListaInteiros lista)

    putStrLn "\nQUESTAO 6 ********************************"
    let lista1 = [1, 1, 1]
    putStrLn $ "Somando lista " ++ show lista1 ++ ": " ++ show (somarListaInteiros lista1)

    putStrLn "\nQUESTAO 7 ********************************"
    putStrLn $ "Somando o dobro da lista " ++ show lista1 ++ ": " ++ show (somarDobroInteiros lista1)

    putStrLn "\nQUESTAO 8 ********************************"
    putStrLn $ "Verificar se 1 eh elemento da lista " ++ show lista1 ++ ": " ++ show (ehMembro 1 lista1)

    putStrLn "\nQUESTAO 9 ********************************"
    let lista2 = [4, 1, 3, 2, 5]
    putStrLn $ "Ordenacao Insert Sort: " ++ show (insertSort lista2)

    putStrLn "\nQUESTAO 10 ********************************"
    putStrLn $ "Ordenacao Selection Sort: " ++ show (selectionSort lista2)

    putStrLn "\nQUESTAO 11 ********************************"
    putStrLn $ "Ordenacao MergeSort: " ++ show (mergeSort lista2)

    putStrLn "\nQUESTAO 12 ********************************"
    putStrLn $ "Ordenacao QuickSort: " ++ show (quickSort lista2)

    putStrLn "\nQUESTAO 13 ********************************"
    putStrLn $ "Contar vogais da palavra Amandaeiou:  " ++ show (contVogais "Amandaeiou")

    putStrLn "\nQUESTAO 14 ********************************"
    putStrLn $ "Contar numero de palavras da frase amanda eh legal: " ++ show (contPalavras "amanda eh legal")

    putStrLn "\nQUESTAO 15 ********************************"
    putStrLn $ "A frase amanda paz na cifra de cesar comd deslocamento 27:  " ++ show (cifraCesar 27 "amanda paz")

    putStrLn "\nQUESTAO 16 ********************************"
    putStrLn $ "A frase amanda paz invertida: " ++ show (inverter "amanda paz")

    putStrLn "\nQUESTAO 17 ********************************"
    putStrLn $ "A frase amanda paz em maiuscule eh: " ++ show (primeiraMaiuscula "amanda paz")

    putStrLn "\nQUESTAO 18 ********************************"
    putStrLn $ "A busca binaria de 3 na lista eh: " ++ show (buscaBinaria 3 lista2)
    
    putStrLn "\nQUESTAO 20 ********************************"
    putStrLn $ "O minimo e o maximo da lista eh " ++ show (minMaxElem lista2)

    putStrLn "\nQUESTAO 21 ********************************"
    putStrLn $ "Qual o numero de letras e palarvas de amanda paz: " ++ show (contCharString "amanda paz")

    putStrLn "\nQUESTAO 22 ********************************"
    putStrLn $ "O nome reinier eh um palindromo? " ++ show (ehPalindromo "reinier")
    putStrLn $ "O nome amanda eh um palindromo? " ++ show (ehPalindromo "amanda")

    putStrLn "\nQUESTAO 23 ********************************"
    putStrLn $ "Abrevie meu nome amanda oliveira da paz santos" ++ show (abreviar "amanda oliveira da paz santos")

    putStrLn "\nQUESTAO 24 ********************************"
    putStrLn $ "O numero 28 eh perfeito? " ++ show (ehPerfeito 28)

    putStrLn "\nQUESTAO 25 ********************************"
    putStrLn $ "Intercale [1,4,8] e [2,3,7]: " ++ show (intercala [1,4,8] [2,3,7])

    putStrLn "\nQUESTAO 26 ********************************"
    putStrLn $ "A lista [a] tem mais de 10 elementos? " ++ show (maisQueDez ['a'])
    putStrLn $ "A lista [0,1,2,3,4,5,6,7,8,9,10] tem mais de 10 elementos? " ++ show (maisQueDez [0,1,2,3,4,5,6,7,8,9,10])

    putStrLn "\nQUESTAO 27 ********************************"
    putStrLn $ "Somar a lista [1,2,3,4]: " ++ show (somaLista [1,2,3,4])

    putStrLn "\nQUESTAO 29 ********************************"
    putStrLn $ "Quantas matriculas validas tem no BD?  " ++ show (numMatriculasValidas)
    putStrLn $ "Quantos doutores tem no BD? " ++ show (getNumeroDoutores)
    putStrLn $ "Quantas mulheres tem no BD? " ++ show (getNumeroMulheres)
    putStrLn $ "Quantas mulheres mestres tem no BD? " ++ show (getNumeroMulheresMestres)
    putStrLn $ "Lista de doutores: " ++ show (getNomeDoutores)
  





