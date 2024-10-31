-- Definição do tipo de dado Povo
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{-Tipos Enumerados

É a forma mais simples de definir um
novo tipo;


data Switch = On | Off
data Boolean = True | False
data Tempo = Frio | Quente
data Estacao = Primaveira | Verao | Outuno | Inverno


Tipos Produto

Definir um tipos com um certo número
de componentes

data Povo = Pessoa Nome Idade
type Nome = String
type Idade = Int
– exemplo : Pessoa “Joao” 32



Tipo Alternativa

Uma forma geométrica pode ser um
círculo ou um retângulo

data Forma = Circulo Float | Retangulo Float Float




Tipos Recursivos

 Tipos que são naturalmente descritos
por eles mesmos

 Exemplo : Árvore Binária de Busca

data Arvore = Vazia | No Arvore Int Arvore deriving Show





-}

data Povo = Pessoa Nome Idade
type Nome = String
type Idade = Int

-- Exemplo de uma pessoa
pessoa1 :: Povo
pessoa1 = Pessoa "Joao" 20

-- Função para imprimir os dados de uma pessoa
imprimir :: Povo -> String
imprimir (Pessoa n i) = "\nNome: " ++ n ++ "\nIdade: " ++ show i

-- Função para verificar a idade e classificar o status
check_idade :: Povo -> String
check_idade (Pessoa n i)
  | i <= 70   = "Trabalhador Ativo..."
  | otherwise = "Aposentado..."



data Forma = Circulo Float | Retangulo Float Float | Triangulo Float Float Float

area :: Forma -> Float
area (Circulo r) = 3.14*r*r
area (Retangulo b h) = b*h

ehRedondo :: Forma -> Bool
ehRedondo (Circulo _ ) = True
ehRedondo (Retangulo _ _ ) = False

circulo :: Forma
circulo = Circulo 1

perimetro :: Forma -> Float
perimetro (Circulo r) = 2 * 3.14 * r
perimetro (Retangulo b h) = (2.0*b) + (2.0*h)

ehRegular :: Forma -> Bool
ehRegular (Circulo _) = True
ehRegular (Retangulo largura altura) = largura == altura
ehRegular (Triangulo a b c) = a == b  && b == c

triangulo :: Forma
triangulo = Triangulo 1 1 1



--deriving (Show, Eq) permite que valores do tipo Complexo sejam exibidos e comparados.
data Complexo = Com Float Float deriving (Show,Eq)

somacomp :: Complexo -> Complexo -> Complexo
somacomp (Com r1 i1) (Com r2 i2) = Com (r1+r2) (i1+i2)








data Arvore a = Vazia | No (Arvore a )a (Arvore a) deriving Show


ar1 :: Arvore Int
ar1 = No (No Vazia 4 Vazia) 6 (No Vazia 7 Vazia)

somaArv :: Arvore Int -> Int
somaArv Vazia = 0
somaArv (No arvesq valor arvdir) = somaArv arvesq + valor + somaArv arvdir


inserir :: Int -> Arvore Int -> Arvore Int
inserir x Vazia = No Vazia x Vazia
inserir x (No esq valor dir)
 | x < valor = No (inserir x esq) valor dir
 | x > valor = No esq valor (inserir x dir)
 | otherwise = No esq valor dir  --valor já existe


caminhamentoEmOrdem :: Arvore Int-> [Int]
caminhamentoEmOrdem Vazia = []
caminhamentoEmOrdem (No esq valor dir) =
    caminhamentoEmOrdem esq ++ [valor] ++ caminhamentoEmOrdem dir

pertence :: Int -> Arvore Int -> Bool
pertence _ Vazia = False
pertence x (No esq valor dir)
 | x < valor = pertence x esq
 | x > valor = pertence x dir
 | otherwise = True

--A expressão Ord t => em Haskell é uma restrição de tipo que indica que o tipo t deve ser uma instância da classe de tipos Ord.
membro :: Ord t => Arvore t -> t -> Bool
membro Vazia elem = False
membro (No esq valor dir) elem
 | elem==valor = True
 | elem>valor = membro dir elem
 | otherwise = membro esq elem



main :: IO ()
main = do
  putStrLn "\nTESTE =  *****************************************************"
  putStrLn $ "TESTE: " ++ show (membro ar1 6 )