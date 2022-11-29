--1)
findVal :: Ord t => t -> [t] -> Int
-- função que, dado um valor e uma lista, retorna a posição em que
-- o valor ocorre pela primeira vez na lista ou zero caso ele não ocorra
findVal _ [] = 0
findVal a b = encontraAi a b 1

encontraAi :: Ord t => t -> [t] -> Int -> Int
-- função que faz o que a findVal faz só que com um contador
encontraAi _ [] _ = 0
encontraAi a (b:c) d
    | a == b = d
    | otherwise = encontraAi a c (d+1)
------------------------------------------------------------------------
--2)
substr :: String -> String -> Bool
-- função que verifica se uma String é subtring da outra
substr [] _ = True
substr _ [] = False
substr a (b:c)
    | length a <= length (b:c) && confereAi a (b:c) = True
    | length a <= length (b:c) = substr a c
    | otherwise = False

confereAi :: String -> String -> Bool
-- função que verifica se uma String é substring da outra
confereAi [] _ = True
confereAi (a:b) (c:d)
    | a == c = confereAi b d
    | otherwise = False
------------------------------------------------------------------------
--3)
data Result = NotInWord -- letra não existe na resposta
            | WrongPos -- letra existe mas está na posição errada
            | OK -- letra e posição corretos
        deriving (Show)

tryWord :: String -> String -> [Result]
-- função que faz meio o que o jogo Wordle faz
tryWord [] _ = []
tryWord a b = tryWordIndex (indexaAi a 0) (indexaAi b 0)

indexaAi :: String -> Int -> [(Char, Int)]
-- função que adiciona índices aos elementos de uma String
indexaAi [] _ = []
indexaAi (a:b) x = (a, x) : indexaAi b (x+1)

tryWordIndex :: [(Char, Int)] -> [(Char, Int)] -> [Result]
-- função tryWord só que os elementos das String possuem índices
tryWordIndex [] _ = []
tryWordIndex (a:b) c = dizAi a c : tryWordIndex b c 

dizAi :: (Char, Int) -> [(Char, Int)] -> Result
-- função que determina a situação de um caractere em relação a uma string
-- ou seja, ou NotInWord ou WrongPos ou OK
dizAi (a, b) ((c, d):e)
    | elem a (remIndex ((c, d):e)) == False = NotInWord
    | naPosCerta (a, b) ((c, d):e) == True = OK
    | otherwise = WrongPos

remIndex :: [(Char, Int)] -> String
-- função que remove os índices de uma lista de tuplas (Char, Int)
-- e deixa somente a lista de Char
remIndex [] = []
remIndex ((a, b):c) = a : remIndex c

naPosCerta :: (Char, Int) -> [(Char, Int)] -> Bool
-- função que verifica se um Char está na posição correta em relação
-- a uma String
naPosCerta _ [] = False
naPosCerta (a, b) ((c, d):e)
    | a == c && b == d = True
    | otherwise = naPosCerta (a, b) e
-----------------------------------------------------------------------
--4)
data Tree t = Node t (Tree t) (Tree t) | Leaf t -- tipo algébrico que representa uma ávore

busca :: String -> Tree String -> Bool
-- função que realiza uma busca binária numa árvore de Strings
-- a fim de retornar se uma determinada String está na árvore ou não
busca a (Leaf b)
    | a == b = True
    | otherwise = False
busca a (Node b c d)
    | a == b = True
    | a < b = busca a c
    | otherwise = busca a d

arv2 :: Tree String -- árvore teste
arv2 = Node "PORTA" (Leaf "MESA") (Leaf "TELHADO")

arv1 :: Tree String -- árvore teste
arv1 = Node "CASA" (Node "ARVORE" (Leaf "AMARELO") (Leaf "AZUL"))
                    (Node "LIVRO" (Leaf "JANELA") arv2)
---------------------------------------------------------------------