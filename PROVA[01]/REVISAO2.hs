import Data.Char (chr, isAlpha, isDigit, isSpace, ord, toLower, toUpper)

-- Função para converter uma string no formato "dd/mm/aaaa" para componentes inteiros
parseData :: String -> (Int, Int, Int)
parseData str = (read dia, read mes, read ano)
  where
    (dia, rest) = splitAt 2 str
    (mes, anoStr) = splitAt 2 (drop 1 rest)
    ano = drop 1 anoStr -- Remove o caractere '/' do final

-- Função para calcular a quantidade total de dias desde 01/01/0000 até uma data
diasDesdeInicio :: Int -> Int -> Int -> Int
diasDesdeInicio dia mes ano = diasAno + diasMes + dia
  where
    -- Total de dias por ano completo
    diasAno = ano * 360 -- Cada ano tem 360 dias (12 meses * 30 dias)
    -- Total de dias nos meses completos
    diasMes = (mes - 1) * 30 -- Cada mês tem 30 dias

-- Função para calcular a quantidade de dias entre duas datas
calcularDias :: [String] -> Int
calcularDias [data1, data2] = abs $ diasDesdeInicio d1 m1 a1 - diasDesdeInicio d2 m2 a2
  where
    (d1, m1, a1) = parseData data1
    (d2, m2, a2) = parseData data2

-- Função para aplicar a cifra de Vigenère
vigenere :: String -> String -> String
vigenere texto chave = zipWith cifra (cycle chave) texto
  where
    cifra :: Char -> Char -> Char
    cifra k t = chr $ (ord (toUpper k) - ord 'A' + ord (toUpper t) - ord 'A') `mod` 26 + ord 'A'

-- Função para converter números para texto por extenso
extenso :: Int -> String
extenso n
  | n < 0 || n >= 10000 = error "Numero fora do intervalo permitido (0 a 9999)"
  | n < 20 = unidades !! n
  | n < 100 = dezenas !! (n `div` 10) ++ (if n `mod` 10 > 0 then " e " ++ unidades !! (n `mod` 10) else "")
  | n < 1000 = centenas !! (n `div` 100) ++ (if n `mod` 100 > 0 then " e " ++ extenso (n `mod` 100) else "")
  | otherwise = unidades !! (n `div` 1000) ++ " mil" ++ (if n `mod` 1000 > 0 then " " ++ extenso (n `mod` 1000) else "")
  where
    unidades = ["zero", "um", "dois", "três", "quatro", "cinco", "seis", "sete", "oito", "nove", "dez", "onze", "doze", "treze", "quatorze", "quinze", "dezesseis", "dezessete", "dezoito", "dezenove"]
    dezenas = ["", "", "vinte", "trinta", "quarenta", "cinquenta", "sessenta", "setenta", "oitenta", "noventa"]
    centenas = ["", "cem", "duzentos", "trezentos", "quatrocentos", "quinhentos", "seiscentos", "setecentos", "oitocentos", "novecentos"]

-- Função para contar o número de caracteres, palavras, linhas, espaços, vogais e números
contar :: String -> [(Int, Int, Int, Int, Int, Int)]
contar texto = [(numCaracteres, numPalavras, numLinhas, numEspacos, numVogais, numNumeros)]
  where
    numCaracteres = length texto
    numLinhas = length (lines texto)
    numEspacos = length (filter isSpace texto)
    numPalavras = length (words texto)
    numVogais = length (filter (\c -> toLower c `elem` "aeiou") texto)
    numNumeros = length (filter isDigit texto)

main :: IO ()
main = do
  -- Teste da função calcularDias
  let data1 = "10/03/1950"
  let data2 = "28/11/1972"
  putStrLn "\nQUESTAO 1 ********************************"
  putStrLn $ "Calcular diferença de dias de 10/03/1950 e 28/11/1972: " ++ show (calcularDias [data1, data2])

  -- Teste da função contar
  let texto = "Alonzo Church"
  putStrLn "\nQUESTAO 2 ********************************"
  putStrLn $ "Contar 'Alonzo Church': " ++ show (contar texto)

  -- Teste da função vigenere
  let textoClaro = "atacar"
  let chave = "oi"
  putStrLn "\nQUESTAO 3 ********************************"
  putStrLn $ "Cifra de Vigenère de 'atacar' com chave 'oi': " ++ vigenere textoClaro chave

  -- Teste da função extenso
  let numero = 3453
  putStrLn "\nQUESTAO 4 ********************************"
  putStrLn $ "Número 3453 por extenso: " ++ extenso numero
