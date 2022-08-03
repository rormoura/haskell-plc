import Data.List
------------------------------------------------------
{- Implementando uma biblioteca (com compreensão de listas). -}
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
type Pessoa = String
type Livro = String
type BancoDeDados = [(Pessoa, Livro)]

membros :: [Int] --id dos membros da biblioteca
membros = [1,2,3,4,5,6,7,8,9,10]

livrosEmprestados :: BancoDeDados --lista de empréstimos
livrosEmprestados = [
    ("Ezequiel","A Cadeira de Prata"),
    ("Sara","C++ handbook"),
    ("Tulio","Discrete Mathematics"),
    ("Kenneth Rosen","Apocalipse"),
    ("Daniel","Zoro Sola"),
    ("Jorginho","Zoro Sola"),
    ("Tulio", "Jonas")]
-- +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
livros :: BancoDeDados -> Pessoa -> [Livro]
-- função que retorna os livros emprestados pela biblioteca
livros bd fulano = [livro | (pessoa,livro) <- bd, pessoa == fulano]

emprestimos :: BancoDeDados -> Livro -> [Pessoa]
-- função que retorna quais pessoas estão pegando emprestado determinado livro
emprestimos bd livrinho = [pessoa | (pessoa, livro) <- bd, livrinho == livro]

emprestado :: BancoDeDados -> Livro -> Bool
-- função que retorna se determinado livro ja foi pego emprestado
emprestado ((a,b):bd) livroin
    | bd == [] && b /= livroin = False
    | bd == [] && b == livroin = True
    | b == livroin = True
    | otherwise = emprestado bd livroin

qtdEmprestimos :: BancoDeDados -> Pessoa -> Int
-- função que retorna a quantidade de livros emprestados pegos pelo
-- mesmo usuário
qtdEmprestimos ((a,b):bd) fulano
    | bd == [] && a /= fulano = 0
    | bd == [] && a == fulano = 1
    | a == fulano = 1 + qtdEmprestimos bd fulano
    | otherwise = qtdEmprestimos bd fulano

emprestar :: BancoDeDados -> Pessoa -> Livro -> BancoDeDados
-- função que adiciona um empréstimo
emprestar bd fulano livroin = (fulano, livroin) : bd

devolver :: BancoDeDados -> Pessoa -> Livro -> BancoDeDados
-- função que remove um empréstimo do banco de dados
devolver ((a,b):bd) fulano livroin
    | bd == [] && a == fulano && b == livroin = [] --  empréstimo procurado é o último
    | bd == [] && (a /= fulano || b /= livroin) = [(a,b)] -- o empréstimo não existe, então não exclui nd
    | a == fulano && b == livroin = bd -- o empréstimo é encontrado no meio da lista
    | a /= fulano || b /= livroin = (a,b) : devolver bd fulano livroin

{- Essa função retorna se um
    determinado id faz parte ou não da lista de membros da biblioteca
    utilizando de compreensão de lista. -}
membro :: [Int] -> Int -> Bool
membro bd id
    | [a | a <- bd, a == id] == [] = False
    | otherwise = True

emprestadoComp :: BancoDeDados -> Livro -> Bool
-- função emprestado só que com compreensão de lista
emprestadoComp bd livroin = elem livroin [b | (a, b) <- bd]

qtdEmprestimosComp :: BancoDeDados -> Pessoa -> Int
-- função qtdEmprestimos só que com compreensão de lista
qtdEmprestimosComp bd fulano = length([(a,b) | (a,b) <- bd, a == fulano])
--qtdEmprestimosComp bd fulano = length (findIndices (== fulano) [a | (a,b) <- bd])

devolverComp :: BancoDeDados -> Pessoa -> Livro -> BancoDeDados
-- função devolver só que com compreensão de lista
devolverComp bd fulano livroin = [(a,b) | (a,b) <- bd, not(a == fulano && b == livroin)]

------------------------------------------------------------------------------------------
-- implementando o quicksort só que com compreensão de lista
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [a] = [a]
quicksort (a:b) = quicksort [x | x <- b, x <= a] ++ [a] ++ quicksort [x | x <- b, x > a]
------------------------------------------------------------------------------------------
--Text processing

getWord :: String -> String
-- essa função retorna todos os caracteres antes do primeiro espaço
getWord (a:b)
    | b == [] && a == ' ' = []
    | b == [] && a /= ' ' = [a]
    | a == ' ' = []
    | a /= ' ' = a : getWord b

dropWord :: String -> String
-- essa função remove a string os caracteres antes do primeiro espaço
dropWord [] = []
dropWord (a:b)
    | a == ' ' = b
    | a /= ' ' = dropWord b

dropSpace :: String -> String
-- essa função remove todos os espaços anteriores ao primeiro caractere diferente de espaço
dropSpace (a:(b:c))
    | a /= ' ' && (b:c) /= [] = (a:(b:c))
    | a /= ' ' && (b:c) == [] = (a:(b:c))
    | a == ' ' && (b:c) == " " = []
    | a == ' ' && b /= ' ' = (b:c)
    | a == ' ' && b == ' ' = dropSpace (b:c)

splitWords :: String -> [String]
-- essa função utiliza das funções acima para criar uma lista de strings
-- a divisão é sempre um ou mais caracteres espaços. 
splitWords a
    | dropWord a == [] = [getWord a]
    | otherwise = getWord a : splitWords (dropSpace (dropWord a))

iterateList :: Int -> Int -> [String] -> String
iterateList i x (a:b)
    | i == x = a
    | otherwise = iterateList (i+1) x (a:b)

{-getLine :: Int -> [String] -> String
getLine x (a:b)
    | x > length (a:b) = []
    | otherwise = iterateList 0 x (a:b)-}
