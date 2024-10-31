{-

1) Considere os seguintes tipos de dados utilizados para a representação de expressões aritméticas
por árvores binárias:

data OP = SOMA | SUB | PROD | DIV deriving (Show, Eq)
data Expr = Folha Int | Nodo OP Expr Expr deriving (Show, Eq)

a) Escreva uma função aplica que aplica um operador binário a dois argumentos inteiros:
b) Escreva uma função avalia que procede ao cálculo do valor de uma expressão:
c) Escreva finalmente uma função imprime que produz uma string com a representação usual
de uma expressão representada por uma árvore:

-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}
{-# HLINT ignore "Use map once" #-}
{-# HLINT ignore "Redundant return" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

data OP = SOMA | SUB | PROD | DIV deriving (Show, Eq)
data Expr = Folha Int | Nodo OP Expr Expr deriving (Show, Eq)


aplica :: OP -> Int -> Int -> Int
aplica SOMA a b = a + b
aplica SUB a b = a - b
aplica PROD a b = a * b
aplica DIV a b = div a b

avalia :: Expr -> Int
avalia (Folha n) = n
avalia (Nodo op esq dir) = aplica op (avalia esq) (avalia dir)

imprime :: Expr -> String
imprime (Folha n) = show n
imprime (Nodo op esq dir) = "(" ++ imprime esq ++ " " ++ mostraOp op ++ " " ++ imprime dir ++ ")"


mostraOp :: OP -> String
mostraOp SOMA = "+"
mostraOp SUB  = "-"
mostraOp PROD = "*"
mostraOp DIV  = "/"

expr :: Expr
expr = Nodo PROD (Nodo SOMA (Folha 3) (Folha 5)) (Folha 2)


{-
2) Considere as seguintes declarações de tipo usadas para representar as horas de um dia nos
formatos usuais.

data Part = AM | PM deriving (Eq, Show)
data TIME = Local Int Int Part -- Formato em 12 horas | Total Int Int -- Formato em 24 horas

time1 :: TIME
time1 = Local 3 45 PM -- 3:45 PM
time2 :: TIME
time2 = Local 11 30 AM -- 11:30 AM
time3 :: TIME
time3 = Total 15 20 -- 15:20 (3:20 PM)
time4 :: TIME
time4 = Total 9 5 -- 09:05 AM

a. Defina a função totalMinutos :: TIME -> Int que conta o total de minutos de uma dada hora.
b. Defina TIME como instância da classe Eq de forma a que a igualdade entre horas seja
independente do formato em que hora está guardada.
c. Defina TIME como instância da classe Ord.

-}

data Part = AM | PM deriving (Eq, Show)
data TIME = Local Int Int Part | Total Int Int

time1 :: TIME
time1 = Local 3 20 PM -- 3:20 PM
time3 :: TIME
time3 = Total 15 20 -- 15:20 (3:20 PM)

totalMinutos :: TIME -> Int
totalMinutos (Local h m AM)
 | h == 12 = m
 | otherwise = (h * 60 )+ m
totalMinutos (Local h m PM)
  | h == 12 = 12 * 60 + m
  | otherwise = (h + 12) * 60 + m
totalMinutos (Total horas minutos) = horas * 60 + minutos

instance Eq TIME where
  (==) :: TIME -> TIME -> Bool
  (==) t1 t2 = totalMinutos t1 == totalMinutos t2

instance Ord TIME where
  (<=) :: TIME -> TIME -> Bool
  (<=) t1 t2 = totalMinutos t1 <= totalMinutos t2

  (<) :: TIME -> TIME -> Bool
  t1 < t2 = totalMinutos t1 < totalMinutos t2

  (>=) :: TIME -> TIME -> Bool
  t1 >= t2 = totalMinutos t1 >= totalMinutos t2

  (>) :: TIME -> TIME -> Bool
  t1 > t2 = totalMinutos t1 > totalMinutos t2

{-
3) Dado o tipo algébrico:

data Nat = Zero | Succ Nat deriving (Show)
Esse tipo representa números naturais, onde:
- Zero é o valor base (equivalente ao número 0).

- Succ n representa o sucessor de um número natural (por exemplo, Succ Zero é 1, Succ (Succ Zero)
é 2, e assim por diante).


Implemente as seguintes funções para operar com valores do tipo Nat:
a. Função para converter Nat para Int:
natToInt :: Nat -> Int
A função deve converter um número natural no tipo Nat para um valor do tipo Int.


b. Função para somar dois números naturais:
soma :: Nat -> Nat -> Nat
A função deve receber dois números naturais e retornar a sua soma, também no tipo Nat.


c. Incluir o tipo Nat na classe Eq e na classe Show

-}

data Nat = Zero | Succ Nat

nat3 :: Nat
nat3 = Succ (Succ (Succ Zero))

nat1 :: Nat
nat1 = Succ Zero

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + natToInt n

soma :: Nat -> Nat -> Nat
soma Zero n = n
soma (Succ n1) n2 = Succ (soma n1 n2)

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  Zero == Zero         = True
  (Succ n1) == (Succ n2) = n1 == n2
  _ == _               = False

instance Show Nat where
  show :: Nat -> String
  show n = show (natToInt n)


--provas diretas
swap :: (t,u) -> (u,t)
swap (x,y) = (y,x)
--swap (swap (a,b)) == (a,b)

cycl :: (t,u,v) -> (u,v,t)
cycl (a,b,c) = (b,c,a)

recycl :: (t,u,v) -> (v,t,u)
recycl (a,b,c) = (c,a,b)

--cycl (recycl (a,b,c)) == (a,b,c)
--recycl (cycl (a,b,c)) == (a,b,c)


--prova por inducao:
--Provar que para todos naturais n
--(sumPowers2 n) + 1 = power2 (n+1)

sumPowers2 :: Int -> Int
sumPowers2 0 = 1
sumPowers2 n = sumPowers2 (n-1) + power2 n


power2 :: Int -> Int
power2 0 = 1
power2 n = 2 * power2 (n-1)


--Provar que para toda lista x
--sumList (double x) = 2 * sumList x

sumList :: Num a => [a] -> a
sumList [] = 0
sumList (a:as) = a + sumList as

double :: Num a => [a] -> [a]
double [] = []
double (a:as) = 2*a : double as


--provas de propriedade

--map (f.g) x = (map f . map g) x
--map (sumList . double) [[1, 2]] == (map sumList . map double) [[1, 2]]

--map f (x++y) = map f x ++ map f y
--map (*2) ([1, 2] ++ [3, 4]) == (map (*2) [1, 2] ++ map (*2) [3, 4])

--map f (collapse tr) = collapse (mapTree f tr)









{-
--4) Prove por indução sobre listas:

--a) length (xs ++ ys) = length xs + length ys
Base: xs  e ys = []
provaBaseA = length ([] ++ []) == length [] + length []
lenght ([]) == 0 + 0
0 = 0

-- Passo indutivo: xs = x:xs
provaPassoA :: (Num a, Eq a) => a -> [a] -> [a] -> Bool
provaPassoA a xs ys = length ((a:xs) ++ ys) == 1 + (length xs + length ys)

LUCCA
passo indutivo: assumindo que é verdade para xs e ys, provamos para (x:xs) e (ys)
  length ((x:xs)++ ys) = length (x:xs) + length ys
  Usemos apenas o lado esquerdo por agora :
  Pela a definição de (++) temos:
    length (x:(xs++ys))
  Pela definiçção de length (a:b) temos -> 1 + length b, logo:
    1 + length(xs++ys)
  Usando nossa hipótese de indução, temos que length(xs++ys) = length xs + length ys:
    1 + length xs + length ys
  Como length (a:b) temos -> 1 + length b, a recíproba é verdadeira, logo:
    1 + length xs + length ys = length (x:xs) + length ys
  Igualuamos o lado esquerdo ao lado direito e provamos indutivamente



--b) sum (reverse xs) = sum xs
Base: xs = []
sum (reverse []) = sum []
sum [] = sum []
0 = 0

-- Passo indutivo: xs = x:xs
provaPassoB :: (Num a, Eq a) => a -> [a] -> Bool
provaPassoB a xs = sum (reverse (a:xs)) == sum (a:xs)

LUCCA
Passo indutivo: assumindo que é verdade para xs, demonstraremos para (x:xs)
  sum(reverse (x:xs)) = sum (x:xs)
  Comecemos pelo lado esquerdo:
    sum(reverse(x:xs)) = sum (xs ++ [x])
  Pela propriedade de sum, temos que sum(a ++ b) = sum a + sum b, logo:
    sum(xs ++ [x]) = sum xs + sum [x]
  Soma de uma lista com um único elemento é o próprio elemento, logo:
    sum xs + sum [x] = sum xs + x
  Agora para o lado direito, usamos a propriedade que (elemento:lista) = [elemento] ++ lista:
    sum(x:xs) = sum ([x] ++ xs)
  E com a mesma propriedade citada acima:
    sum([x] ++ xs) = sum [x] + sum xs
  Com a propriedade de sum para lista unica:
    sum [x] + sum xs = x + sum xs
  Agora, igualamos os lados e está Provado
  sum xs + x = x + sum xs 
  A soma é associativa para ambos os lados, então está provado





5) Prove por indução sobre os naturais
Para qualquer que seja n, somatorio n = div (n*(n+1)) 2
somatorio :: Int -> Int
somatorio 0 = 0
somatorio n = n + somatorio (n-1)


caso base: n = 0
somatorio n = div (n*(n+1)) 2
somatorio 0 = div (0 * (0 + 1)) 2
0 = div 0 2
0 = 0

LUCCA
Passo indutivo: assumindo que é verdade para n, testemos para (n+1)
  somatorio (n+1) = div((n+1)*(n+2)) 2
  Usemos o lado esquerdo aplicando na função:
    somatorio (n + 1) = (n+1) + somatorio n
  Sabemos que somatorio n = div (n*(n+1)) 2, logo
    (n+1) + somatorio n = (n+1) + div(n*(n+1)) 2
  Em haskell, div é apenas a operação sobre inteiros. Contudo, ela ainda representa uma fração.
  Dessa forma, (n+1) pode ser representado como a "fração" div (n+1) 1. Então, façamos a soma das frações:
    [div (n+1) 1] + [div(n*(n+1)) 2] =[div (2n + 2 + n^2 +n) 2]
    [div (n^2 + 3n + 2) 2] = [div ((n+1)*(n+2)) 2]
  Igualamos o lado esquerdo com o lado direito, então provamos





-- Imprimir uma string na saída padrão
putStr :: String -> IO()

-- Imprimir uma string na saída padrão
putStrLn :: String -> IO()

-- Ler uma string da entrada padrão
getLine :: IO String

--Elaborar um função para ler uma String a
--partir do teclado e imprimir o seu tamanho.

-}
imp :: IO ()
imp = do
  putStrLn "Digite uma string:"
  input <- getLine   -- Lê a string do teclado
  let tamanho = length input  -- Calcula o tamanho da string
  putStrLn $ "O tamanho da string é: " ++ show tamanho  -- Imprime o tamanho

loop :: IO ()
loop = do
  putStrLn "Digite um número (ou 'sair'):"
  input <- getLine
  if input == "sair"
  then putStrLn "Encerrando o programa."
  else do
    let numero = read input :: Int
    print (numero * 2)
    loop
{-
Elaborar uma função para realizar o cálculo do
fatorial de um número várias vezes. Elabore uma
função a fim de questionar ao usuário se este deseja
continuar ou não.
-}

fat :: Int -> Int
fat 0 = 1
fat x = x * fat (x-1)

fatorial :: IO ()
fatorial = do
  putStrLn "Digite um número para calcular fatorial (ou 'sair')"
  input <- getLine
  if input == "sair"
    then print "Encerrando Programa"
    else do
      let numero = read input :: Int
      print (fat numero)
      fatorial


{-
Como seriam funções de Entrada ? Funções que retornam IO alguma coisa
   Para escrever funções de entrada precisamos da função return

   return :: a -> IO a
-}

main2 :: IO()
main2 = do
  a <- lernumero
  b <- lernumero
  imprimesoma a b

lernumero :: IO Int
lernumero = do
  putStr "Digite um numero:"
  num <- readLn
  return num

imprimesoma :: Int -> Int -> IO()
imprimesoma a b = do
  putStr "Soma:"
  print (a+b)

{-Crie um programa em Haskell que lê
um arquivo, numera as linhas do
conteúdo e grava o resultado em um
novo arquivo-}

arquivo :: IO()
arquivo = do
   -- Lê o conteúdo do arquivo "entrada.txt"
    conteudo <- readFile "PROVA[02]/entrada.txt"

    -- Divide o conteúdo em linhas e numera as linhas
    let linhasNumeradas = zipWith (\n linha -> show n ++ ": " ++ linha) [1..] (lines conteudo)

    -- Junta as linhas numeradas em uma única string, separadas por quebras de linha
    let resultado = unlines linhasNumeradas

    -- Grava o resultado no arquivo "saida.txt"
    writeFile "PROVA[02]/saida.txt" resultado

    putStrLn "As linhas foram numeradas e gravadas em 'saida.txt'."


{-Funções monomórficas

 Definição funciona apenas para um tipo de
dados específico

capitalize :: Char -> Char
capitalize ch = chr (ord ch + offset)
where offset = ord ’A’ - ord ’a’


Funções polimórficas
 Função possui um tipo genérico
 Uso de variáveis de tipos
 Polimorfismo paramétrico

zip (a:as) (b:bs) = (a,b) : zip as bs
zip _ _ = []


Overloading
– Eq: tipos com igualdade
– Ord: tipos ordenados
– Show: tipos mostráveis
– Read: tipos legíveis
– Num: tipos numéricos

-}






{-6) Implemente uma classe chamada Calculavel em Haskell. Essa classe deve definir duas
operações:

area :: a -> Double – Calcula a área de uma figura geométrica
perimetro :: a -> Double – Calcula o perímetro da figura.

Em seguida:


a) Defina um tipo chamado Circulo para representar um círculo, utilizando o raio como parâmetro.
b) Inclua o tipo Circulo na classe Calculavel e implemente as funções area e perimetro para ele.
c) Crie outros tipos geométricos, como Retangulo e Quadrado, e inclua-os na classe Calculavel.
d) Adicione uma nova função à classe Calculavel, como descricao :: a -> String, que retorna uma
descrição textual da figura.-}

class Calculavel a where
  area :: a -> Double
  perimetro :: a -> Double
  descricao :: a -> String

data Circulo = C Double
data Retangulo = R Double Double
data Quadrado = Q Double

instance Calculavel Circulo where
  area :: Circulo -> Double
  area (C r) = r * r * 3.14

  perimetro :: Circulo -> Double
  perimetro (C r) = 2 * r * 3.14

  descricao :: Circulo -> String
  descricao (C r) = "Círculo de raio: " ++ show r

instance Calculavel Retangulo where
  area :: Retangulo -> Double
  area (R b h) = b * h

  perimetro :: Retangulo -> Double
  perimetro (R b h) = 2 * b + 2 * h

  descricao :: Retangulo -> String
  descricao (R b h) = "Retangulo de base: " ++ show b ++ " e de altura: " ++ show h


instance Calculavel Quadrado where
  area :: Quadrado -> Double
  area (Q l) = l * l

  perimetro :: Quadrado -> Double
  perimetro (Q l) = 4 * l

  descricao :: Quadrado -> String
  descricao (Q l) = "Quadrado de lado: " ++ show l


retangulo :: Retangulo
retangulo = R 2 3


{-7) Elabore uma função em Haskell para ler um arquivo e contar o números de linhas, número de
palavras e número de caracteres do arquivo-}

arquivo2 :: IO String
arquivo2 = do
   -- Lê o conteúdo do arquivo "entrada.txt"
    conteudo <- readFile "PROVA[02]/entrada.txt"

     -- Conta o número de linhas
    let numLinhas = length (lines conteudo)

    --Conta o número de palavras
    let numPalavras = length (words conteudo)

    --Conta o númeor de caracteres do arquivo
    let numChar = length conteudo

    let resultado = "\nNúmero de linhas: " ++ show numLinhas ++
                    "\nNúmero de palavras: " ++ show numPalavras ++
                    "\nNúmero de caracteres: " ++ show numChar

    return resultado

{--8) Implemente um jogo em Haskell no qual o jogador deve adivinhar um número secreto gerado
aleatoriamente entre 1 e 100. O programa deve orientar o jogador se o número é maior ou menor do
que a tentativa feita. Quando o número é adivinhado corretamente, o jogo termina e exibe a
quantidade de tentativas realizadas.-}





adivinhar :: Int -> Int -> IO Int
adivinhar numeroSecreto tentativas = do
  putStrLn "Digite sua tentativa: "
  tentativa <- readLn

  if tentativa < numeroSecreto
    then do
      putStrLn "O número secreto é maior!"
      adivinhar numeroSecreto (tentativas + 1)
  else if tentativa > numeroSecreto
    then do
      putStrLn "O número secreto é menor!"
      adivinhar numeroSecreto (tentativas + 1)
  else do
    putStrLn "Você acertou!"
    return tentativas

jogo :: IO ()
jogo = do
  putStrLn "\n\nBem-vindo ao jogo de adivinhar o número!"
  putStrLn "Tente adivinhar o número secreto entre 1 e 100."

  tentativas <- adivinhar 43 1  -- Escolhe manualmente o número secreto

  putStrLn $ "Parabéns! Você adivinhou o número em " ++ show tentativas ++ " tentativas."


{-9) Implemente uma agenda de contatos em Haskell que armazena nomes e telefones utilizando uma
lista de tuplas. Cada contato deve ser representado como uma tupla no formato (String, String),
onde o primeiro elemento é o nome e o segundo é o telefone.
A agenda deve permitir as seguintes operações:
1. Incluir: Adicionar um novo contato (nome e telefone).
2. Pesquisar: Buscar um contato pelo nome.
3. Listar: Exibir todos os contatos armazenados.
4. Excluir: Remover um contato pelo nome.
5. Sair: Encerrar o programa.
A aplicação deve apresentar um menu interativo que permite ao usuário escolher as operações.-}

-- Definições de tipo
type Nome = String
type Telefone = String
type Contato = (Nome, Telefone)
type Agenda = [Contato]

-- Agenda inicial vazia
agenda :: Agenda
agenda = []

-- Incluir um contato na agenda
incluir :: Contato -> Agenda -> Agenda
incluir contato agenda = contato : agenda

-- Pesquisar um contato pelo nome
pesquisar :: Nome -> Agenda -> Maybe Contato
pesquisar nome [] = Nothing
pesquisar nome ((n, t):resto)
  | nome == n = Just (n, t)
  | otherwise = pesquisar nome resto

-- Listar todos os contatos da agenda
listar :: Agenda -> String
listar [] = "Agenda vazia."
listar agenda = unlines (map show agenda)

-- Excluir um contato pelo nome
excluir :: Nome -> Agenda -> Agenda
excluir nome [] = []
excluir nome ((n, t):resto)
  | nome == n = resto
  | otherwise = (n, t) : excluir nome resto

-- Ler contato a partir da entrada do usuário
lerContato :: IO Contato
lerContato = do
  putStrLn "Digite o nome do contato: "
  nome <- getLine
  putStrLn "Digite o telefone do contato: "
  telefone <- getLine
  return (nome, telefone)

-- Imprimir todos os contatos
imprimirAgenda :: Agenda -> IO ()
imprimirAgenda agenda = putStrLn (imprimirContatos agenda)

imprimirContatos :: Agenda -> String
imprimirContatos = concatMap imprimirContato

imprimirContato :: Contato -> String
imprimirContato (n, t) = 
    let pontos = max 0 (tamanhoLinha - length n - length "Nome: ")
    in "Nome: " ++ n ++ repetir pontos "." ++ "Telefone: " ++ t ++ "\n"

tamanhoLinha :: Int
tamanhoLinha = 30

repetir :: Int -> String -> String
repetir 0 _ = ""
repetir n str = str ++ repetir (n-1) str

-- Função do menu interativo
menu :: Agenda -> IO ()
menu agenda = do
  putStrLn "\nEscolha a operação desejada: "
  putStrLn "1. Incluir: Adicionar um novo contato (nome e telefone)."
  putStrLn "2. Pesquisar: Buscar um contato pelo nome."
  putStrLn "3. Listar: Exibir todos os contatos armazenados."
  putStrLn "4. Excluir: Remover um contato pelo nome."
  putStrLn "5. Sair: Encerrar o programa."
  putStrLn "Digite a opcao: "

  opcao <- getLine

  case opcao of
    "1" -> do
      putStrLn "Inclusão de contato"
      contato <- lerContato
      let novaAgenda = incluir contato agenda
      menu novaAgenda

    "2" -> do
      putStrLn "Pesquisa de contato"
      nome <- getLine
      case pesquisar nome agenda of
        Nothing -> do
          putStrLn "Contato não existe!!"
        Just contato -> do
          putStr (imprimirContato contato)
      menu agenda

    "3" -> do
      putStrLn "Listar contatos armazenados"
      imprimirAgenda agenda
      menu agenda

    "4" -> do
      putStrLn "Excluir contato pelo nome"
      nome <- getLine
      let novaAgenda = excluir nome agenda
      menu novaAgenda

    "5" -> do
      putStrLn "Saindo do programa"
      writeFile "PROVA[02]/saida.txt" (show agenda)
      putStrLn "Dados salvos com sucesso!!!"

    _  -> do
      putStrLn "Opção inválida!!!"
      menu agenda






main :: IO ()
main = do

  agendasalva <- readFile "PROVA[02]/saida.txt"
  menu (read agendasalva :: Agenda)

  putStrLn "\n\nQUESTAO 1 =  *****************************************************"
  putStrLn $ "Avaliando a expressão: " ++ show (avalia expr)
  putStrLn $ "Imprimindo a expressão: " ++ imprime expr

  putStrLn "\n\nQUESTAO 2 =  *****************************************************"
  putStrLn $ "Verificar se 3:20 PM == 15:20: " ++ show (time1 == time3)
  putStrLn $ "Comparando3:20 PM == 15:20:  " ++ show (compare time1 time3)

  putStrLn "\n\nQUESTAO 3 =  *****************************************************"
  putStrLn $ "Convertendo Nat para Int: " ++ show (natToInt nat3)
  putStrLn $ "Somando 3 e 1: " ++ show (natToInt (soma nat3 nat1))


  putStrLn "\n\nQUESTAO 4 =  *****************************************************"


