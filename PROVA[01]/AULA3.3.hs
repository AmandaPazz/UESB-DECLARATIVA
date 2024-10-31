unidades :: [String]
unidades = ["zero","um","dois","tres","quatro","cinco","seis","sete","oito",
  "nove"]

dezena1 :: [String]
dezena1 = ["dez","onze","doze","treze","catorze","quinze","dezesseis",
  "dezessete","dezoito","dezenove" ]

dezena2 :: [String] 
dezena2 = ["vinte","trinta","quarenta","cinquenta","sessenta","setenta",
   "oitenta","noventa"]

-- extenso 23 = "vinte e tres" 
extenso :: Int -> String
extenso numero = tratar (dezena,unidade)
   where
     dezena = div numero 10
     unidade = mod numero 10

tratar :: (Int,Int) -> String
tratar (0,u) = unidades !! u
tratar (1,u) = dezena1 !! u
tratar (d,0) = dezena2 !! (d-2)
tratar (d,u) = dezena2 !! (d-2) ++ " e " ++ unidades !! u


-- Função principal para testar
main :: IO ()
main = loop

loop :: IO ()
loop = do
    putStrLn "Digite um numero de 0 a 99 (ou -1 para sair):"
    input <- getLine
    let numero = read input :: Int
    if numero == -1
       then putStrLn "Saindo..."
       else if numero >= 0 && numero < 100
            then do
                putStrLn $ "Numero por extenso: " ++ extenso numero
                loop
            else do
                putStrLn "Por favor, digite um numero entre 0 e 99."
                loop

   