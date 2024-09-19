type Nome = String
type Preco = Int
type CodigoBarra = Int
type BancoDeDados = [(CodigoBarra, Nome, Preco)]

bd :: BancoDeDados
bd = [(1001, "Refrigerante", 450),
      (1002, "Leite", 320),
      (1003, "Biscoito", 200),
      (1004, "Suco", 989),
      (1005, "Arroz", 345),
      (1006, "Feijao", 780)]


{-
 - RESOLUCAO DA ATIVIDADE DE REVISAO!!!
 -}


-- Questao 01
buscarBDaux :: CodigoBarra -> BancoDeDados -> (Nome, Preco)
buscarBDaux _ [] = error "Produto nao cadastrado!!!"
buscarBDaux codigo ((codigoBarra, nome, preco) : resto)
  | codigo == codigoBarra = (nome, preco)
  | otherwise = buscarBDaux codigo resto

-- Questao 02
buscaBD :: CodigoBarra -> (Nome, Preco)
buscaBD codigo = buscarBDaux codigo bd

-- Questao 03
fazerConta :: [CodigoBarra] -> [(Nome, Preco)]
fazerConta = map buscaBD

-- Questao 04
dividir :: Int -> String
dividir numero = show (div numero 100) ++ "." ++ show (mod numero 100)

-- Questao 05
repetir :: Int -> String -> String
repetir 0 _ = ""
repetir numero caractere = caractere ++ repetir (numero -1) caractere

-- Questao 06 
tamanhoLinha :: Int
tamanhoLinha = 30

formatarLinha :: (Nome, Preco) -> String
formatarLinha (nome, preco) = nome ++ repetir quantidade "." ++ dividir preco ++ "\n"
  where
 -- a linha tem que ser de tamanho = 30, logo temos que desconsiderar o tamanho do nome e do preco  
  tamanhoNome = length nome
  tamanhoPreco = length (dividir preco)
  quantidade = tamanhoLinha - (tamanhoNome + tamanhoPreco)

-- Questao 07 
formatarLinhas :: [(Nome, Preco)] -> String
formatarLinhas [] = []
formatarLinhas (a : x) = formatarLinha a ++ formatarLinhas x

-- Questao 08
calcularTotal :: [ (Nome, Preco) ] -> Int
calcularTotal [] = 0
calcularTotal ((nome, preco) : x) = preco + calcularTotal x

-- Questao 09 
formatarTotal :: Int -> String
-- Um comportamento muito similar de uma funcao ja criada...
formatarTotal valorTotal = formatarLinha ("Total:", valorTotal)

{-
 - Caso queira desenvolver a funcao, segue abaixo comentada

formatarTotal valorTotal = "Total:" ++ repetir quantidade "." ++ dividir valorTotal ++ "\n" 
  where
  tamanhoPreco = length(dividir valorTotal) 
  quantidade = tamanhoLinha - (tamanhoPreco + length("Total:"))
-}

-- Questao 10 
formatarConta :: [(Nome, Preco)] -> String
formatarConta lista = formatarLinhas lista ++ formatarTotal (calcularTotal lista)

imprimirConta :: [CodigoBarra] -> IO()
imprimirConta lista = putStr (formatarConta (fazerConta lista))

main :: IO ()
main = do
    -- Testar buscarBDaux
    putStrLn "Teste da funcao buscarBDaux:"
    print $ buscarBDaux 1001 bd  -- Esperado: ("Refrigerante", 450)
    print $ buscarBDaux 1002 bd  -- Esperado: ("Leite", 320)
    -- A chamada abaixo deve gerar um erro, descomente para testar
    -- print $ buscarBDaux 9999 bd  -- Esperado: erro "Produto nao cadastrado!!!"

    -- Testar buscaBD
    putStrLn "\nTeste da funcao buscaBD:"
    print $ buscaBD 1003  -- Esperado: ("Biscoito", 200)
    print $ buscaBD 1004  -- Esperado: ("Suco", 989)

    -- Testar fazerConta
    putStrLn "\nTeste da funcao fazerConta:"
    print $ fazerConta [1005, 1006]  -- Esperado: [("Arroz", 345), ("Feijao", 780)]

    -- Testar dividir
    putStrLn "\nTeste da funcao dividir:"
    print $ dividir 450  -- Esperado: "4.50"
    print $ dividir 320  -- Esperado: "3.20"

    -- Testar repetir
    putStrLn "\nTeste da funcao repetir:"
    print $ repetir 5 "*"  -- Esperado: "*****"

    -- Testar formatarLinha
    putStrLn "\nTeste da funcao formatarLinha:"
    print $ formatarLinha ("Refrigerante", 450)  -- Esperado: "Refrigerante........4.50\n"
    print $ formatarLinha ("Leite", 320)         -- Esperado: "Leite................3.20\n"

    -- Testar formatarLinhas
    putStrLn "\nTeste da funcao formatarLinhas:"
    print $ formatarLinhas [("Arroz", 345), ("Feijao", 780)]
    -- Esperado:
    -- "Arroz................3.45\nFeijao...............7.80\n"

    -- Testar calcularTotal
    putStrLn "\nTeste da funcao calcularTotal:"
    print $ calcularTotal [("Arroz", 345), ("Feijao", 780)]  -- Esperado: 1125

    -- Testar formatarTotal
    putStrLn "\nTeste da funcao formatarTotal:"
    print $ formatarTotal 1125  -- Esperado: "Total...............11.25\n"

    -- Testar formatarConta
    putStrLn "\nTeste da funcao formatarConta:"
    let conta = [("Arroz", 345), ("Feijao", 780)]
    print $ formatarConta conta
    -- Esperado:
    -- "Arroz................3.45\nFeijao...............7.80\nTotal...............11.25\n"

    -- Testar imprimirConta
    putStrLn "\nTeste da funcao imprimirConta:"
    imprimirConta [1001, 1002, 1003]
    -- Esperado:
    -- "Refrigerante........4.50\nLeite................3.20\nBiscoito.............2.00\nTotal...............9.70\n"