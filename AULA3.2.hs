import Data.Char

-- TO DO
-- Converter uma lista de nomes em uma lista de e-mails
-- Remover palavras "do","dos","de",...
-- Verificar pessoas com as mesmas iniciais
-- rss@uesb.edu.br
-- rss2@uesb.edu.br
-- Verificar o tamanho e espaços
-- converte "Ricardo"  !!!
-- converte "Ricardo    Souza     Silva" !!!!




-- converte "Ricardo Souza Silva"
-- "rss@uesb.edu.br"

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
splitWord str = getWord str  :  splitWord (dropWord str)


-- pegarInicial "Ricardo" = "r"
pegarInicial :: String -> String
pegarInicial (a:b) = [toLower a]

-- transformar ["Ricardo","Souza","Silva"] = "rss"
transformar :: [String] -> String
transformar [] = []
transformar (a:b) = pegarInicial a ++ transformar b

converte :: String -> String
converte str = transformar (splitWord str) ++ "@uesb.edu.br"