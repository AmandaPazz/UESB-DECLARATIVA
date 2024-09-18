{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
import Data.Char
import Data.List

-- Palavras a serem removidas (como "do", "dos", "de", etc.)
palavrasRemover :: [String]
palavrasRemover = ["do", "dos", "da", "das", "de", "di", "du"]

-- Converter uma lista de nomes em uma lista de e-mails
gerarEmails :: [String] -> [String]
gerarEmails nomes = snd (mapAccumL criarEmail [] nomes)

-- Cria um email, verificando se já existem emails com as mesmas iniciais
criarEmail :: [String] -> String -> ([String], String)
criarEmail emailsExistentes nome =
    let baseEmail = converte nome
        novoEmail = if baseEmail `elem` emailsExistentes
                    then gerarNovoEmail emailsExistentes baseEmail 2
                    else baseEmail
    in (novoEmail : emailsExistentes, novoEmail)

-- Gera um novo email incrementando o número
gerarNovoEmail :: [String] -> String -> Int -> String
gerarNovoEmail emailsExistentes baseEmail n =
    let novoEmail = takeWhile (/= '@') baseEmail ++ show n ++ "@uesb.edu.br"
    in if novoEmail `elem` emailsExistentes
       then gerarNovoEmail emailsExistentes baseEmail (n+1)
       else novoEmail

-- Remover palavras como "do", "dos", "de", etc.
removerPalavras :: [String] -> [String]
removerPalavras = filter (`notElem` palavrasRemover)

-- Função auxiliar para dividir a string por vírgulas
splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy delim str =
    let (first, remainder) = span (/= delim) str
    in first : case remainder of
        [] -> []
        (_:rest) -> splitBy delim rest

-- getWord "Ricardo Souza Silva" = "Ricardo"
getWord :: String -> String
getWord [] = []
getWord (a:b)
    | a == ' ' = []
    | otherwise = a : getWord b

-- dropWord "Ricardo Souza Silva" = "Souza Silva"
dropWord :: String -> String
dropWord [] = []
dropWord (a:b)
    | a == ' ' = b
    | otherwise = dropWord b

-- splitWord "Ricardo Souza Silva" = ["Ricardo","Souza","Silva"]
splitWord :: String -> [String]
splitWord [] = []
splitWord str = getWord str : splitWord (dropWord str)

-- pegarInicial "Ricardo" = "r"
pegarInicial :: String -> String
pegarInicial (a:b) = [toLower a]

-- transformar ["Ricardo","Souza","Silva"] = "rss"
transformar :: [String] -> String
transformar [] = []
transformar (a:b) = pegarInicial a ++ transformar b

-- converte "Ricardo Souza Silva" = "rss@uesb.edu.br"
converte :: String -> String
converte str = transformar (removerPalavras (splitWord str)) ++ "@uesb.edu.br"

-- Função principal para testar
main :: IO ()
main = loop

loop :: IO ()
loop = do
    putStrLn "Digite a lista de nomes separados por virgula (ou digite 'sair' para encerrar):"
    nomesStr <- getLine
    if map toLower nomesStr == "sair"
        then putStrLn "Programa encerrado."
        else do
            let nomes = map (unwords . words) (splitBy ',' nomesStr)  -- Remove espaços extras
            let emails = gerarEmails nomes
            putStrLn "\n\nEmails gerados: *********************"
            mapM_ putStrLn emails
            putStrLn ""  -- Nova linha para espaçamento
            loop  -- Chama o loop novamente para continuar pedindo entrada


