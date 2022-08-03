-- Algumas funções recursivas
import Data.Char
{-  Essas duas abaixo trabalham em conjunto. A função vendas
    retorna a quantidade de vendas de uma determinada semana.
    A função quantIguais retorna o número de semanas que tiveram
    a mesma quantidade de vendas que o parâmetro 's'.-}

vendas :: Int -> Int-- Recebe uma semana x e retorna o númmero de vendas
vendas x = mod x 7  -- dessa semana; só retorna valores de 0 a 6.
                    -- Desse modo, há repetição de valores.

quantIguais :: Int -> Int -> Int
quantIguais s n
    | (n == 0 && s == 0) = 1
    | (n == 0 && s /= 0) = 0
    | (s == vendas n) = 1 + quantIguais s (n-1)
    | otherwise = 0 + quantIguais s (n-1)

-----------------------------------------------------------

{-  As duas funções abaixo trabalham em conjunto. A função calcula
    retorna a quantidade de divisores do parâmetro 'x'. A função ehprimo
    retorna se o parâmetro 'x' é primo ou não. -}

calcula :: Int -> Int -> Int
calcula x y
    | (y == 0) = 0
    | (mod x y == 0) = 1 + calcula x (y-1)
    | otherwise = 0 + calcula x (y-1)   

ehprimo :: Int -> Bool
ehprimo x
    | (calcula x x == 2) = True
    | otherwise = False

------------------------------------------------------------

{-  As duas funções abaixo trabalham em conjunto. A função contador
    retorna a quantidade de divisores comuns entre os parâmetros 'x'
    e 'y'. A função primosEntreSi retorna de os parâmetros 'x' e 'y'
    são primos entre si.-}

contador :: Int -> Int -> Int -> Int
contador x y z -- z vai de y a 0
    | z == 0 = 0
    | (mod x z == 0) && (mod y z == 0) = 1 + contador x y (z-1)
    | otherwise = 0 + contador x y (z-1) 

primosEntreSi :: Int -> Int -> Bool
primosEntreSi x y
    | (contador x y y == 1) = True
    | otherwise = False

-------------------------------------------------------------

{-  A função fat retorna o fatorial do parâmtro 'x'. -}

fat :: Int -> Int
fat x
    | x == 0 = 1
    | x == 1 = 1 * fat (x-1)
    | otherwise = x * fat (x-1)

--------------------------------------------------------------

{- Essa função compara se quatro números são iguais. -}

all4Equal :: Int -> Int -> Int -> Int -> Bool
all4Equal x y z w
    | x == y && x == z && x == w = True
    | otherwise = False

--------------------------------------------------------------

{- Essa função retorna quantos parâmetros (dos três) são iguais. -}

equalCount :: Int -> Int -> Int -> Int
equalCount x y z
    | x == y && x == z = 3
    | x == y || x == z || y == z = 2
    | otherwise = 0

--------------------------------------------------------------

{- Essa função retorna uma string que possui x espaços. -}

addEspacos :: Int -> String
addEspacos x
    | x == 0 = ""
    | otherwise = " "++(addEspacos (x-1))

--------------------------------------------------------------

{- Essa função adicionada x espaços à esquerda de uma string dada. -}

paraDireita :: Int -> String -> String
paraDireita x palavra = (addEspacos x)++palavra

--------------------------------------------------------------

{-  Essas funções abaixo trabalham em conjunto. Elas printam a quantidade
    de vendas das semanas de 0 a n, tudo em forma de tabela. -}

totalVendas :: Int -> Int
totalVendas x
    | x == -1 = 0
    | otherwise = vendas x + vendas (x-1)

calculaMedia :: Int -> Float
calculaMedia x = a / b
    where
        a = fromIntegral (totalVendas x) :: Float
        b = fromIntegral x :: Float

printaMedia :: Int -> String
printaMedia x = "  Media"++addEspacos 4++paraDireita 4 (show (calculaMedia x))++"\n"

printaTotal :: Int -> String
printaTotal x = "  Total"++addEspacos 4++paraDireita 5 (show (totalVendas x))++"\n"++printaMedia x

printa :: Int -> Int -> String
printa a b
    | a < b = printaTotal a
    | b > 9 = paraDireita 3 (show b)++addEspacos 5++paraDireita 6 (show(vendas b))++"\n"++printa a (b+1)
    | otherwise = paraDireita 3 (show b)++addEspacos 5++paraDireita 7 (show(vendas b))++"\n"++printa a (b+1)

vendasTabela :: Int -> String
vendasTabela n = "Semana        Venda"++"\n"++printa n 0