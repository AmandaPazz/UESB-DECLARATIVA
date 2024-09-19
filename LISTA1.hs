module Lista01 where

import Data.Char (isDigit, toLower, toUpper)

-- Definindo os tipos para base, altura e area
type Base = Float

type Altura = Float

type Area = Float

-- 1 = Fun��o para calcular a area do triangulo
areaTriangulo :: Base -> Altura -> Area
areaTriangulo b h = (b * h) / 2.0

-- 2 = Funcao para encontrar o menor entre tres inteiros
menorQue3 :: Int -> Int -> Int -> Int
menorQue3 a b c
  | a <= b && a <= c = a
  | b <= c = b
  | otherwise = c

-- 3 = Elaborar uma funcao que recebe quatro inteiros e verica se todos sao iguais
iguais4 :: Int -> Int -> Int -> Int -> Bool
iguais4 a b c d = (a == b) && (b == c) && (d == c)

-- 4 = Elaborar uma funcao que receba tres inteiros e verique se os tres sao diferentes.
diferentes3 :: Int -> Int -> Int -> Bool
diferentes3 a b c = (a /= b) && (a /= c) && (c /= b)

-- 5 = Elaborar uma fun��o que receba quatro n�meros e devolva a m�diaponderada, sabendo-se que os pesos s�o respectivamente 1,2,3 e 4.
mediaPonderada4 :: Int -> Int -> Int -> Int -> Double
mediaPonderada4 a b c d = fromIntegral (a + (b * 2) + (c * 3) + (d * 4)) / 10

-- 6 = Elaborar uma funcao que calcule do desconto de 20% do pre�o de um produto
desconto20 :: Double -> Double
desconto20 preco = preco - (0.2 * preco)

-- 7 = Elaborar uma funcao para calcular o XOR (ou exclusivo).
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

-- 8 = Elaborar uma funcao para entrar com a razao de uma progressao aritmetica e o valor do primeiro termo. Devolver o decimo termo da serie.
aritmetica :: Int -> Int -> Int
aritmetica a1 r = a1 + (r * 9)

-- 9 = Elaborar uma funcao para receber um n�mero e informar se ele eh positivo, negativo ou nulo.
verificaNumero :: (Ord a, Num a) => a -> String
verificaNumero n
  | n > 0 = "positivo"
  | n < 0 = "negativo"
  | n == 0 = "nulo"

-- 10 = Elaborar uma funcao para calcular o NAND (negacao do AND).
nand :: Bool -> Bool -> Bool
nand a b = not (a && b)

-- 11 = Elaborar uma funcao para vericar se um caractere eh um digito
ehDigitoChar :: Char -> Bool
ehDigitoChar = isDigit

-- 12 = Elaborar uma funcao para converter uma letra maiuscula para minuscula.
minuscula :: Char -> Char
minuscula = toLower

-- 13 = Elaborar uma funcao para converter uma letra minuscula para maiuscula.
maiuscula :: Char -> Char
maiuscula = toUpper

-- 14 = Elaborar uma funcao para somar os algarismos de um numero inteiro N. Considere que N >= 0 e N < 1000.
somarAlgarismosN :: Int -> Int
somarAlgarismosN n
  | n < 0 || n >= 1000 = error "O numero nao estah dentro do intervalo"
  | otherwise = mod n 10 + div (mod n 100) 10 + div n 100

-- 15 = Elaborar uma funcao para calcular a media aritmetica das tres notas.
mediaNotas :: Float -> Float -> Float -> Float
mediaNotas a b c = (a + b + c) / 3

-- 16 = Elaborar uma funcao para receber tres numeros e vericar se eles podem ou nao ser lados de um triangulo
ladosTriangulo :: Float -> Float -> Float -> Bool
ladosTriangulo a b c = (a + b) > c && (a + c) > b && (b + c) > a

-- 17 = Elaborar uma funcao para receber tres numeros e devolver o tipo de triangulo (segundo seus angulos)
angulosTriangulo :: Float -> Float -> Float -> String
angulosTriangulo a b c
  | (a + b + c) /= 180 = "Nao forma triangulo"
  | (a == 90) || (b == 90) || (c == 90) = "Retangulo"
  | (a > 90) || (b > 90) || (c > 90) = "Obtusangulo"
  | otherwise = "Acutangulo"

-- 18 = Elaborar uma funcao para calcular a soma de um progressao aritmetica finita.
somaAritmetica :: Int -> Int -> Int -> Int
somaAritmetica an a1 n = div (n * (a1 + an)) 2

-- 19 = Elaborar uma funcao para receber as coordenadas de um ponto e informar a localizacao no plano cartesiano. Testar os 9 casos possiveis.
localizacao :: Float -> Float -> String
localizacao x y
  | (x == 0) && (y == 0) = "Centro"
  | (x > 0) && (y > 0) = "Primeiro Quadrante"
  | (x < 0) && (y > 0) = "Segundo Quadrante"
  | (x < 0) && (y < 0) = "Terceiro Quadrante"
  | (x > 0) && (y < 0) = "Quarto Quadrante"
  | (x > 0) && (y == 0) = "Eixo x positivo"
  | (x < 0) && (y == 0) = "Eixo x negativo"
  | (x == 0) && (y > 0) = "Eixo Y positivo"
  | (x == 0) && (y < 0) = "Eixo Y Negativo"
  | otherwise = "erro"

-- 20 = Elaborar uma funcao para ler um numero inteiro N entre 1 e 12 e devolver o nome do mes.
mes :: Int -> String
mes m
  | m == 1 = "Janeiro"
  | m == 2 = "Fevereiro"
  | m == 3 = "Marco"
  | m == 4 = "Abril"
  | m == 5 = "Maio"
  | m == 6 = "Junho"
  | m == 7 = "Julho"
  | m == 8 = "Agosto"
  | m == 9 = "Setembro"
  | m == 10 = "Outubro"
  | m == 11 = "Novembro"
  | m == 12 = "Dezembro"
  | otherwise = "Numero invalido! Por favor, insira um numero entre 1 e 12."

-- 21 = Elaborar uma funcao que recebe um numero e retorna verdadeiro se for par.
ehPar :: Int -> Bool
ehPar n
  | even n = True
  | otherwise = False


-- 22 = Elaborar uma funcao para retornar o numero de raizes de uma equacao do segundo grau.
raizes :: Int -> Int -> Int -> String
raizes a b c
  | delta > 0 = "2 raizes"
  | delta == 0 = "1 raiz"
  | otherwise = "Nao possui raizes reais"
  where delta = b * b - 4 * a * c
  
-- 23 = Elaborar uma funcao que converte a temperatura de graus farenheit para centigrados.
celsius :: Float -> Float
celsius f = (5 / 9) * (f - 32)

--24 = Elaborar uma funcao para calcular o IMC (indice de Massa Corporal) de uma pessoa. Pesquise a tabela de IMC na Internet.
calcularIMC :: Float -> Float -> String
calcularIMC peso altura
  | imc < 18.5 = "IMC: " ++ show imc ++ " - Abaixo do peso"
  | imc >= 18.5 && imc < 24.9 = "IMC: " ++ show imc ++ " - Peso normal"
  | imc >= 25 && imc < 29.9 = "IMC: " ++ show imc ++ " - Sobrepeso"
  | imc >= 30 && imc < 34.9 = "IMC: " ++ show imc ++ " - Obesidade Grau I"
  | imc >= 35 && imc < 39.9 = "IMC: " ++ show imc ++ " - Obesidade Grau II"
  | imc >= 40 = "IMC: " ++ show imc ++ " - Obesidade Grau III (morbida)"
  | otherwise = "IMC invalido"
  where imc = peso / (altura * altura)

-- 25 = Escreva uma funcao classicarIdade :: Int -> String que classica umapessoa como "Crian�a", "Adolescente", "Adulto"ou "Idoso"com base
--      na idade fornecida.
classificarIdade :: Int -> String 
classificarIdade idade
  |idade < 0 = "Idade invalida"
  | idade < 14 = "Crianca"
  | idade < 20 = "Adolescente"
  | idade < 60 = "Adulto"
  | otherwise = "Idoso"

-- 26 = Escreva uma funcao classicarNota :: Int -> String que classica uma nota como "F"(0-49), "D"(50-59), "C"(60-69), "B"(70-79) ou "A"(80-100).
classificarNota :: Int -> String
classificarNota n 
  | n < 0 && n > 100 = "Nota invalida"
  | n >= 0 && n <= 49 = "F"
  | n >= 50 && n <= 59 = "D"
  | n >= 60 && n <= 69 = "C"
  | n >= 70 && n <= 79 = "B"
  | n >= 80 && n <= 100 = "A"

-- 27 = Escreva uma funcao proximoCaractere :: Char -> Char que retorna o proximo caractere na sequencia alfabetica. Se o caractere for 'z' ou 'Z', retorne 'a' ou 'A', respectivamente.
proximoCaractere :: Char -> Char
proximoCaractere c
  | c == 'z' = 'a'
  | c == 'Z' = 'A'
  | otherwise = succ c


-- 28 = Defina:
-- (a) Paradigma funcional
-- Um estilo de programa��o onde o foco est� em usar fun��es como elementos principais.
-- As fun��es em linguagens funcionais s�o tratadas como "cidad�os de primeira classe",
-- ou seja, podem ser passadas como argumentos, retornadas de outras fun��es e armazenadas em vari�veis.
-- O paradigma funcional enfatiza a imutabilidade e fun��es puras, evitando mudan�as de estado
-- e efeitos colaterais.

-- (b) Paradigma imperativo
-- Um estilo de programa��o que se baseia em comandos que alteram o estado do programa.
-- Aqui, o foco est� em descrever "como" fazer as coisas, usando sequ�ncias de instru��es que
-- modificam vari�veis e alteram o fluxo do programa. Este paradigma inclui o uso de loops,
-- vari�veis mut�veis e controle expl�cito de fluxo, como condicionais e la�os de repeti��o.

-- (c) Imutabilidade
-- O conceito de que, uma vez que uma vari�vel � definida, seu valor n�o pode ser alterado.
-- Em um contexto funcional, imutabilidade ajuda a evitar erros relacionados ao estado mut�vel,
-- tornando o comportamento do programa mais previs�vel e confi�vel. Qualquer "mudan�a" em um valor
-- resulta na cria��o de um novo valor, em vez de modificar o valor original.

-- (d) Efeito colateral
-- Quando uma fun��o faz mais do que apenas devolver um valor: ela pode modificar
-- o estado do sistema, como alterar uma vari�vel global, escrever em um arquivo ou
-- exibir algo na tela. Esses efeitos s�o "colaterais" porque n�o est�o relacionados
-- ao valor retornado pela fun��o e podem tornar o comportamento do programa menos previs�vel.

-- (e) Fun��es puras
-- Fun��es que, para o mesmo conjunto de entradas, sempre retornam o mesmo resultado
-- e n�o produzem efeitos colaterais. A pureza das fun��es facilita o entendimento e
-- a depura��o do c�digo, al�m de permitir otimiza��es, como a memoiza��o,
-- onde o resultado de uma fun��o pode ser armazenado e reutilizado.

-- (f) Transpar�ncia referencial
-- Uma propriedade de express�es em que uma express�o pode ser substitu�da pelo seu valor
-- sem alterar o comportamento do programa. Em outras palavras, se uma express�o E tem valor V,
-- em qualquer lugar do programa onde E aparece, pode ser substitu�da por V,
-- e o programa ainda funcionar� da mesma maneira.

-- (g) Lazy evaluation
-- Um modelo de avalia��o onde as express�es n�o s�o calculadas at� que seus valores
-- sejam realmente necess�rios. Isso permite a cria��o de estruturas de dados infinitas,
-- e pode melhorar o desempenho, pois evita c�lculos desnecess�rios.

-- (h) Eager evaluation
-- Um modelo de avalia��o onde as express�es s�o calculadas assim que s�o ligadas
-- a uma vari�vel. Este � o m�todo padr�o em muitas linguagens imperativas,
-- e pode ser mais simples de entender, mas pode levar a c�lculos desnecess�rios,
-- especialmente se o valor de uma express�o nunca for usado.

-- (i) Fun��o de Alta ordem
-- Fun��es que podem receber outras fun��es como argumentos ou retornar fun��es como resultado.
-- Isso permite um estilo de programa��o mais flex�vel e expressivo, onde fun��es podem ser
-- combinadas, aplicadas de forma condicional ou passadas como par�metros para outras fun��es,
-- permitindo abstra��es poderosas e reutiliza��o de c�digo.







-- 29 = Pesquisar o objetivo das fun��es abaixo em Haskell:
-- (a) abs
-- A fun��o `abs` recebe um n�mero e retorna o seu valor absoluto, ou seja,
-- o mesmo n�mero sem o sinal negativo. Por exemplo, `abs (-5)` retorna `5`.

-- (b) signum
-- A fun��o `signum` recebe um n�mero e retorna seu "sinal".
-- Ela retorna `-1` para n�meros negativos, `0` para zero, e `1` para n�meros positivos.
-- Exemplo: `signum (-5)` retorna `-1`, `signum 0` retorna `0`, e `signum 5` retorna `1`.

-- (c) sqrt
-- A fun��o `sqrt` calcula a raiz quadrada de um n�mero. Ela recebe um n�mero
-- positivo e retorna o valor cuja raiz quadrada � o n�mero fornecido.
-- Exemplo: `sqrt 25` retorna `5`.

-- (d) exp
-- A fun��o `exp` calcula a exponencial de um n�mero, ou seja, `e` elevado �
-- pot�ncia do n�mero fornecido, onde `e` � a base dos logaritmos naturais (aproximadamente 2.71828).
-- Exemplo: `exp 1` retorna aproximadamente `2.71828`.

-- (e) log
-- A fun��o `log` calcula o logaritmo natural (ou neperiano) de um n�mero,
-- que � o inverso da exponencia��o. Ou seja, se `y = exp(x)`, ent�o `log(y) = x`.
-- Exemplo: `log 2.71828` retorna aproximadamente `1`.

-- (f) negate
-- A fun��o `negate` inverte o sinal de um n�mero, transformando positivo em negativo
-- e negativo em positivo. Exemplo: `negate 5` retorna `-5`, e `negate (-5)` retorna `5`.

-- (g) logBase
-- A fun��o `logBase` calcula o logaritmo de um n�mero em uma base espec�fica.
-- Ela recebe dois argumentos: a base e o n�mero do qual se deseja calcular o logaritmo.
-- Exemplo: `logBase 10 100` retorna `2`, porque `10^2 = 100`.

-- (h) floor
-- A fun��o `floor` recebe um n�mero real e retorna o maior inteiro que � menor ou
-- igual a esse n�mero. Basicamente, ela arredonda o n�mero para baixo.
-- Exemplo: `floor 3.7` retorna `3`, e `floor (-3.7)` retorna `-4`.

-- (i) ceiling
-- A fun��o `ceiling` recebe um n�mero real e retorna o menor inteiro que � maior ou
-- igual a esse n�mero. Basicamente, ela arredonda o n�mero para cima.
-- Exemplo: `ceiling 3.7` retorna `4`, e `ceiling (-3.7)` retorna `-3`.

-- (j) round
-- A fun��o `round` recebe um n�mero real e o arredonda para o inteiro mais pr�ximo.
-- Se o n�mero estiver exatamente no meio (por exemplo, `2.5`), ela arredonda para
-- o inteiro par mais pr�ximo. Exemplo: `round 2.3` retorna `2`, `round 2.5` retorna `2`,
-- e `round 3.5` retorna `4`.

-- (k) truncate
-- A fun��o `truncate` remove a parte decimal de um n�mero real, retornando apenas a parte inteira.
-- Ela n�o arredonda o n�mero, apenas corta a parte fracion�ria.
-- Exemplo: `truncate 3.7` retorna `3`, e `truncate (-3.7)` retorna `-3`.






-- 30 = Qual o resultado da avalia��o das express�es abaixo em Haskell?
-- (a) truncate 5.4 + floor 6.7 + ceiling 8.9 + abs (-7) + signum 8 | R = 28
-- (b) 0x + 0b1111 + 0o12

-- Este � um exemplo de opera��es com diferentes bases num�ricas em Haskell.

-- 0x:
-- A express�o 0x por si s� est� incompleta, pois 0x � o prefixo para n�meros
-- hexadecimais (base 16). Deveria haver um valor ap�s o 0x, como por exemplo,
-- 0xA (que seria 10 em decimal).

-- 0b1111:
-- O 0b prefixa n�meros bin�rios (base 2).
-- 0b1111 � 15 em decimal.

-- 0o12:
-- O 0o prefixa n�meros octais (base 8).
-- 0o12 � 10 em decimal.

-- Mas como 0x est� incompleto, o c�digo em Haskell geraria um erro de sintaxe,
-- pois 0x precisa ser seguido por um n�mero hexadecimal v�lido.

-- Se quisermos corrigir isso, por exemplo, se a inten��o fosse usar 0xA:
-- 0xA � 10 em decimal.

-- Somando os valores:
-- 10 (0xA) + 15 (0b1111) + 10 (0o12) = 35





-- 31 = Baseado na deni��o da fun��o abaixo,
--              dobro x = x + x
--       Apresente algumas formas de avalia��o da express�o abaixo:
--       dobro (dobro 2)

-- Defini��o da fun��o dobro
--dobro :: Int -> Int
--dobro x = x + x

-- Avalia��o passo a passo
-- 1. Avalie a express�o interna dobro 2
-- dobro 2 = 2 + 2 = 4
--
-- 2. Substitua dobro 2 por 4 na express�o externa
-- dobro (dobro 2) = dobro 4
--
-- 3. Avalie dobro 4
-- dobro 4 = 4 + 4 = 8
--
-- Portanto, dobro (dobro 2) = 8

-- Avalia��o expandida
-- 1. Expanda dobro (dobro 2)
-- dobro (dobro 2) = dobro (2 + 2)
--
-- 2. Expanda dobro 4
-- dobro (2 + 2) = (2 + 2) + (2 + 2)
--
-- 3. Some os valores
-- (2 + 2) + (2 + 2) = 4 + 4 = 8
--
-- Portanto, dobro (dobro 2) = 8

-- Avalia��o por substitui��o
-- 1. Substitua dobro por x + x
-- dobro (dobro 2) = (dobro 2) + (dobro 2)
--
-- 2. Substitua dobro 2 por 2 + 2 em ambas as ocorr�ncias
-- (2 + 2) + (2 + 2)
--
-- 3. Some os valores
-- 4 + 4 = 8
--
-- Portanto, dobro (dobro 2) = 8








-- 32 = Como calcular a quantidade de digitos de 2 elevado a 100 em Haskell? (Dica: utilize fun��es prontas do Prelude)
quantidadeDigitos :: Int -> Int
quantidadeDigitos n = length (show (2^n))







-- Exemplo de uso
main :: IO ()
main = do
  putStrLn "\n\nQUESTAO 1 =  *****************************************************"
  putStrLn $ "A area do triangulo com base 10 e altura 5 eh " ++ show (areaTriangulo 10 5)

  putStrLn "\n\nQUESTAO 2 =  *****************************************************"
  putStrLn $ "O menor entre 7, 3 e 5 eh" ++ show (menorQue3 7 3 5)

  putStrLn "\n\nQUESTAO 3 =  *****************************************************"
  putStrLn $ "Os numeros 4 4 4 e 4 sao iguais? " ++ show (iguais4 4 4 4 4)

  putStrLn "\n\nQUESTAO 4 =  *****************************************************"
  putStrLn $ "Os numeros 4 4 e 5 sao diferentes" ++ show (diferentes3 4 4 5)

  putStrLn "\n\nQUESTAO 5 =  *****************************************************"
  putStrLn $ "Media Ponderada entre 4 4 4 e 4 eh: " ++ show (mediaPonderada4 4 4 4 4)
  putStrLn "\n\nQUESTAO 6 =  *****************************************************"
  putStrLn $ "O desconto sobre 10 reais eh: " ++ show (desconto20 10.0)

  putStrLn "\n\nQUESTAO 7 =  *****************************************************"
  putStrLn $ "XOR entre True e True eh: " ++ show (xor True True)
  putStrLn $ "XOR entre True e false eh: " ++ show (xor True False)
  putStrLn $ "XOR entre false e True eh: " ++ show (xor False True)
  putStrLn $ "XOR entre False e False eh: " ++ show (xor False False)

  putStrLn "\n\nQUESTAO 8 =  *****************************************************"
  putStrLn $ "O decimo termo da serie de 10 e primeiro termo 5 eh:  " ++ show (aritmetica 5 10)

  putStrLn "\n\nQUESTAO 9 =  *****************************************************"
  putStrLn $ "Verificar o numero -10.9: " ++ show (verificaNumero (-10.9))
  putStrLn $ "Verificar o numero 1: " ++ show (verificaNumero 1)
  putStrLn $ "Verificar o numero 0: " ++ show (verificaNumero 0)

  putStrLn "\n\nQUESTAO 10 =  *****************************************************"
  putStrLn $ "NAND entre True e True eh: " ++ show (nand True True)
  putStrLn $ "NAND entre True e false eh: " ++ show (nand True False)
  putStrLn $ "NAND entre false e True eh: " ++ show (nand False True)
  putStrLn $ "NAND entre False e False eh: " ++ show (nand False False)

  putStrLn "\n\nQUESTAO 11 =  *****************************************************"
  putStrLn $ "Verificar se 'a' eh digito: " ++ show (ehDigitoChar 'a')

  putStrLn "\n\nQUESTAO 12 =  *****************************************************"
  putStrLn $ "Transformar 'A' para minuscula: " ++ show (minuscula 'A')

  putStrLn "\n\nQUESTAO 13 =  *****************************************************"
  putStrLn $ "Transformar 'a' para maiuscula: " ++ show (maiuscula 'a')

  putStrLn "\n\nQUESTAO 14 =  *****************************************************"
  putStrLn $ "Somar os algarismos do numero 123: " ++ show (somarAlgarismosN 123)

  putStrLn "\n\nQUESTAO 15 =  *****************************************************"
  putStrLn $ "Media entre 3.3, 3.3 e 3.3 eh: " ++ show (mediaNotas 3.3 3.3 3.3)

  putStrLn "\n\nQUESTAO 16 =  *****************************************************"
  putStrLn $ "Verificar se lados 4.4, 1.2 e 2.1 podem formar um triangulo: " ++ show (ladosTriangulo 4.4 1.2 2.1)
  putStrLn $ "Verificar se lados 5, 6 e 7 podem formar um triangulo: " ++ show (ladosTriangulo 5 6 7)

  putStrLn "\n\nQUESTAO 17 =  *****************************************************"
  putStrLn $ "Verificar o tipo de triangulo com angulos 90 45 45: " ++ show (angulosTriangulo 90 45 45)
  putStrLn $ "Verificar o tipo de triangulo com angulos 105 60 32: " ++ show (angulosTriangulo 105 60 32)
  putStrLn $ "Verificar o tipo de triangulo com angulos 100 40 40: " ++ show (angulosTriangulo 100 40 40)
  putStrLn $ "Verificar o tipo de triangulo com angulos 50 80 50: " ++ show (angulosTriangulo 50 80 50)

  putStrLn "\n\nQUESTAO 18 =  *****************************************************"
  putStrLn $ "Soma da P.A onde o numero eh 100, o primeiro eh 1 a posicao do termo eh 100:  " ++ show (somaAritmetica 100 1 100)

  putStrLn "\n\nQUESTAO 19 =  *****************************************************"
  putStrLn $ "A localizacao do ponto (0, 9) eh:   " ++ show (localizacao 0 9)

  putStrLn "\n\nQUESTAO 20 =  *****************************************************"
  putStrLn $ "Qual eh o mes 5?  " ++ show (mes 5)

  putStrLn "\n\nQUESTAO 21 =  *****************************************************"
  putStrLn $ "O numero 284393123128 eh par? " ++ show (ehPar 284393123128)

  putStrLn "\n\nQUESTAO 22 =  *****************************************************"
  putStrLn $ "Verificar numero de raizes pra 2 -4 e -6: " ++ show (raizes 2 (-4) (-6))
  putStrLn $ "Verificar numero de raizes pra 1 -6 e 9: " ++ show (raizes 1 (-6) 9)
  putStrLn $ "Verificar numero de raizes pra 1 4 e 5: " ++ show (raizes 1 4 5)

  putStrLn "\n\nQUESTAO 23 =  *****************************************************"
  putStrLn $ "A temperatura 32F em celcius eh: " ++ show (celsius 32)
  putStrLn $ "A temperatura 212 em celcius eh: " ++ show (celsius 212)
  
  putStrLn "\n\nQUESTAO 24 =  *****************************************************"
  putStrLn $ "Qual o imc de uma pessoa de 50kg e 170cm:"  ++ show (calcularIMC 50 170)

  putStrLn "\n\nQUESTAO 25 =  *****************************************************"
  putStrLn $ "Eu estou em que fase da vida (19 anos): "  ++ show (classificarIdade 19)

  putStrLn "\n\nQUESTAO 26 =  *****************************************************"
  putStrLn $ "Eu tirei 7 na minha prova. Qual a minha nota? "  ++ show (classificarNota 7)

  putStrLn "\n\nQUESTAO 26 =  *****************************************************"
  putStrLn $ "Qual o sucesso de m? "  ++ show (proximoCaractere 'm')

  putStrLn "\n\nQUESTAO 32 =  *****************************************************"
  putStrLn $ "A quantidade de digitos de 2^100 eh "  ++ show (quantidadeDigitos 100)

