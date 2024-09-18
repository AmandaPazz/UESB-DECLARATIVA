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
    diasAno = ano * 360  -- Cada ano tem 360 dias (12 meses * 30 dias)
    -- Total de dias nos meses completos
    diasMes = (mes - 1) * 30  -- Cada mês tem 30 dias

-- Função para calcular a quantidade de dias entre duas datas
calcularDias :: [String] -> Int
calcularDias [data1, data2] = abs $ diasDesdeInicio d1 m1 a1 - diasDesdeInicio d2 m2 a2
  where
    (d1, m1, a1) = parseData data1
    (d2, m2, a2) = parseData data2

-- Função principal para teste
main :: IO ()
main = do
    let data1 = "10/03/1950"
    let data2 = "28/11/1972"
    print $ calcularDias [data1, data2]
