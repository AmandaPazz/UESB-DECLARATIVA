-- Define os tipos
type Usuario = String
type Usuarios = [Usuario]

type Livro = String
type Livros = [Livro]

type Emprestimo = (Usuario, Livro)
type Emprestimos = [Emprestimo]

-- Dados de exemplo
emprestimos :: Emprestimos
emprestimos = [ ("Maria","Java"),("Maria","Haskell"),("Rita","Haskell")]

usuarios :: Usuarios
usuarios = ["Maria","Carlos","Rita"]

livros :: Livros
livros = ["Haskell","Java","JavaScript","Pascal"]

-- Funções para manipulação de empréstimos
obterLivros :: Usuario -> Emprestimos -> Livros
obterLivros usuario [] = []
obterLivros usuario ((n,l):b)
   | usuario == n = l : obterLivros usuario b
   | otherwise = obterLivros usuario b 

registrarEmprestimo :: Livro -> Usuario -> Emprestimos -> Emprestimos
registrarEmprestimo livro usuario emprestimos = (usuario,livro):emprestimos

devolucaoEmprestimo :: Livro -> Usuario -> Emprestimos -> Emprestimos
devolucaoEmprestimo livro usuario [] = []
devolucaoEmprestimo livro usuario ((n,l):b)
    | n == usuario && livro == l = b
    | otherwise = (n,l) : devolucaoEmprestimo livro usuario b

obterUsuarios :: Livro -> Emprestimos -> Usuarios
obterUsuarios livro [] = []
obterUsuarios livro (a:b)
   | livro == livro' = usuario' : obterUsuarios livro b
   | otherwise = obterUsuarios livro b
   where
     livro' = snd a
     usuario' = fst a

-- Função para repetir uma string `caractere` `numero` vezes
repetir :: Int -> String -> String 
repetir 0 _ = ""
repetir numero caractere = caractere ++ repetir (numero - 1) caractere

-- Tamanho da linha desejada
tamanhoLinha :: Int 
tamanhoLinha = 30 

-- Função para formatar a lista de empréstimos
formatarEmprestimos :: Emprestimos -> String
formatarEmprestimos [] = []
formatarEmprestimos ((u,l):b) =  u ++ repetir x "." ++  l ++ "\n" ++ formatarEmprestimos b
  where
    tamanhoUsuario = length u
    tamanhoLivro = length l
    x = tamanhoLinha - (tamanhoUsuario + tamanhoLivro)

-- Função principal
main :: IO ()
main = loop emprestimos -- Chama o loop inicial com os emprestimos atuais

-- Loop para interação com o usuário
loop :: Emprestimos -> IO ()
loop emprestimosAtuais = do
    putStrLn "\nSistema de Biblioteca"
    putStrLn "Escolha uma opcao:"
    putStrLn "1. Ver livros emprestados por um usuario"
    putStrLn "2. Registrar emprestimo"
    putStrLn "3. Devolver livro"
    putStrLn "4. Ver quem emprestou um livro"
    putStrLn "5. Ver todos os emprestimos"
    putStrLn "6. Sair"
    opcao <- getLine
    case opcao of
        "1" -> do
            putStrLn "Digite o nome do usuario:"
            usuario <- getLine
            let livrosEmprestados = obterLivros usuario emprestimosAtuais
            if null livrosEmprestados
                then putStrLn "Nenhum livro emprestado."
                else putStrLn $ "Livros emprestados por " ++ usuario ++ ": " ++ show livrosEmprestados
            loop emprestimosAtuais

        "2" -> do
            putStrLn "Digite o nome do usuario:"
            usuario <- getLine
            putStrLn "Digite o nome do livro:"
            livro <- getLine
            let novosEmprestimos = registrarEmprestimo livro usuario emprestimosAtuais
            putStrLn $ "Livro '" ++ livro ++ "' registrado para " ++ usuario
            loop novosEmprestimos

        "3" -> do
            putStrLn "Digite o nome do usuario:"
            usuario <- getLine
            putStrLn "Digite o nome do livro a devolver:"
            livro <- getLine
            let novosEmprestimos = devolucaoEmprestimo livro usuario emprestimosAtuais
            putStrLn $ "Livro '" ++ livro ++ "' devolvido por " ++ usuario
            loop novosEmprestimos

        "4" -> do
            putStrLn "Digite o nome do livro:"
            livro <- getLine
            let usuariosQuePegaram = obterUsuarios livro emprestimosAtuais
            if null usuariosQuePegaram
                then putStrLn "Nenhum usuario pegou este livro emprestado."
                else putStrLn $ "Usuarios que pegaram o livro '" ++ livro ++ "': " ++ show usuariosQuePegaram
            loop emprestimosAtuais

        "5" -> do
            putStrLn "Emprestimos atuais:"
            putStrLn $ formatarEmprestimos emprestimosAtuais
            loop emprestimosAtuais

        "6" -> putStrLn "Saindo do sistema. Ate mais!"

        _ -> do
            putStrLn "Opcao invalida, tente novamente."
            loop emprestimosAtuais
