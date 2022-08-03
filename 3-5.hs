import Data.Char
---------------------------------------------------------
sumList :: [Int] -> Int
sumList a
    | a == [] = 0
    | otherwise = head a + sumList (tail a)

----------------------------------------------------------
-- A seguinte função dobra os elementos de uma lista

double :: [Int] -> [Int]
double a
    | a == [] = []
    | otherwise = ((head a)*2) : (double (tail a))
-----------------------------------------------------------
{- A seguinte função verifica se um determinado número
    está na lista. -}
member :: [Int] -> Int -> Bool
member a b
    | a == [] = False
    | head a == b = True
    | otherwise = member (tail a) b
-----------------------------------------------------------
{- A seguinte função retorna uma lista (em ordem de aparição)
    dos números presentes numa string. -}
digits :: String -> String
digits s
    | s == [] = []
    | s == ('0':[]) = ['0']++digits (tail s)
    | s == ('1':[]) = ['1']++digits (tail s)
    | s == ('2':[]) = ['2']++digits (tail s)
    | s == ('3':[]) = ['3']++digits (tail s)
    | s == ('4':[]) = ['4']++digits (tail s)
    | s == ('5':[]) = ['5']++digits (tail s)
    | s == ('6':[]) = ['6']++digits (tail s)
    | s == ('7':[]) = ['7']++digits (tail s)
    | s == ('8':[]) = ['8']++digits (tail s)
    | s == ('9':[]) = ['9']++digits (tail s)
    | head s == '0' = ['0']++[',']++digits (tail s)
    | head s == '1' = ['1']++[',']++digits (tail s)
    | head s == '2' = ['2']++[',']++digits (tail s)
    | head s == '3' = ['3']++[',']++digits (tail s)
    | head s == '4' = ['4']++[',']++digits (tail s)
    | head s == '5' = ['5']++[',']++digits (tail s)
    | head s == '6' = ['6']++[',']++digits (tail s)
    | head s == '7' = ['7']++[',']++digits (tail s)
    | head s == '8' = ['8']++[',']++digits (tail s)
    | head s == '9' = ['9']++[',']++digits (tail s)
    | otherwise = digits (tail s)
-----------------------------------------------------------------
{- A seguinte função soma os elementos de duas listas. Para isso
    é necessário que as listas de entrada tenham o mesma tamanho.-}
sumPairs :: [Int] -> [Int] -> [Int]
sumPairs a b
    | a == [] = []
    | otherwise = (head a + head b) : sumPairs (tail a) (tail b)
-----------------------------------------------------------------
{- A seguinte função indexa uma lista. -}
indexar :: Int -> [Int] -> [[Int]]
indexar n a
    | a == [] = []
    | otherwise = [[n, head a]] ++ indexar (n+1) (tail a)
----------------------------------------------------------------
{- A seguinte função tira os índices de uma lista. -}
desindexar :: [[Int]] -> [Int]
desindexar a
    | a == [] = []
    | otherwise = tail (head a) ++ desindexar (tail a)
----------------------------------------------------------------
{- As seguintes funções trabalham em conjunto para implementarem
    o algoritmo de ordenação quicksort sobre uma lista de inteiros.-}
menores :: Int -> [Int] -> [Int]
menores a [] = []
menores a (b:c)
    | b <= a = b : menores a c
    | otherwise = menores a c

maiores :: Int -> [Int] -> [Int]
maiores a [] = []
maiores a (b:c)
    | b > a = b : maiores a c
    | otherwise = maiores a c

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [a] = [a]
quicksort (a:b) = quicksort (menores a b) ++ [a] ++ quicksort (maiores a b)
----------------------------------------------------------------
{- As seguintes funções trabalham em conjunto para retornar
    uma lista de inteiros com os N primeiros números pares da 
    sequência de Fibonacci. -}
printaPar :: [Int] -> [Int]
printaPar a
    | a == [] = []
    | (mod (head a) 2 == 0) = (head a):printaPar (tail a)
    | otherwise = printaPar (tail a)

fib :: Int -> Int
fib x
    | x == 0 = 1
    | x == 1 = 0 + fib 0
    | otherwise = fib (x-1) + fib (x-2)

criafibonacci :: Int -> Int -> Int -> [Int]
criafibonacci a b n
    | a == n = []
    | mod (fib b) 2 == 0 = fib b : criafibonacci (a+1) (b+1) n
    | otherwise = criafibonacci a (b+1) n

fibonacci :: Int -> [Int]
fibonacci n = criafibonacci 0 0  n
--------------------------------------------------------------------------------
{- As seguintes funções trabalham em conjunto para retornar
    uma lista ordenada (ordem crescente) conforme a soma
    de seus dígitos. -}
asciiToInt :: Int -> Int
asciiToInt a --transforma o valor em ascii para o valor inteiro
    | a == 48 = 0
    | a == 49 = 1
    | a == 50 = 2
    | a == 51 = 3
    | a == 52 = 4
    | a == 53 = 5
    | a == 54 = 6
    | a == 55 = 7
    | a == 56 = 8
    | a == 57 = 9

somadigitos :: String -> Int --essa função retorna a soma dos dígitos de um inteiro
somadigitos a
    | a == [] = 0
    | otherwise = asciiToInt (ord (head a)) + somadigitos (tail a)
 {- indexarsoma coloca a soma dos dígitos do elemento como seu
    índice (o número do lado esquerdo do par). -}
indexarsoma :: [Int] -> [[Int]]
indexarsoma a 
    | a == [] = []
    | otherwise = [somadigitos (show (head a)),head a] : indexarsoma (tail a)

-----
menListList :: [Int] -> [[Int]] -> [[Int]]
menListList a [] = []
menListList a (b:c)
    | head b <= head a = b : menListList a c
    | otherwise = menListList a c

maiListList :: [Int] -> [[Int]] -> [[Int]]
maiListList a [] = []
maiListList a (b:c)
    | head b > head a = b : maiListList a c
    | otherwise = maiListList a c

qsListList :: [[Int]] -> [[Int]]
qsListList [] = []
qsListList [a] = [a]
qsListList (a:b) = qsListList (menListList a b) ++ [a] ++ qsListList (maiListList a b)
-----

ordenar :: [Int] -> [Int]
ordenar a = desindexar (qsListList (indexarsoma a))