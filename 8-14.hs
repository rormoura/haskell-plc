module Main where
--import Set

{-main :: IO ()
main = do   putStrLn "Entre com a primeira string: "
            s1 <- getLine
            putStrLn (show s1)
            putStrLn "Entre com a segunda string: "
            s2 <- getLine
            putStrLn (show s2)
            putStrLn (if (s1 == s2) then "sao iguais" else "sao diferentes")
            putStrLn "Quer comparar outras strs? (y/n) "
            ans <- getLine
            if (ans == "y") then main else return ()-}

main :: IO () -- Esse código soma todos os inteiros que estão
                -- num arquivo de entrada. Dispostos da seguinte maneira,
                -- um número\noutro número\n...
                -- o último número não possui \n após ele.
main = do   putStr "Lendo o arquivo: "
            ans <- getLine
            putStrLn ans
            readFile ans >>=
                \a -> putStrLn (show(soma a))
            putStrLn "Deseja ler outro arquivo? (y/n)"
            ans1 <- getLine
            if (ans1 == "y") then main else return ()

soma :: String -> Int
soma x = foldl (+) 0 (remnl x)

remnl :: String -> [Int]
-- função que transforma a string advinda do arquivo lido em uma lista de inteiros
remnl [] = []
remnl [a] = [toInt(a)]
remnl (a:b:c)
    | a == '\n' = remnl (b:c)
    | a == '-' && retndig (b:c) == 1 = (-1)*toInt(b) : remnl c -- inteiro negativo de um dígito
    | a == '-' && retndig (b:c) > 1 = (-1)*(toInt1 (b:c) ((retndig (b:c)) - 1)) : remnl (remndig (b:c)) -- inteiro negativo de mais dígitos
    | b == '\n' = toInt(a) : remnl c --inteiro positivo de um dígito
    | otherwise = toInt1 (a:b:c) ((retndig (a:b:c)) - 1) : remnl (remndig (a:b:c))--inteiro positivo de mais dígitos

toInt :: Char -> Int
-- função que transforma um digito em um inteiro
toInt '0' = 0
toInt '1' = 1
toInt '2' = 2
toInt '3' = 3
toInt '4' = 4
toInt '5' = 5
toInt '6' = 6
toInt '7' = 7
toInt '8' = 8
toInt '9' = 9

toInt1 :: String -> Int -> Int
-- função que transforma uma string de 2 ou mais dígitos em um inteiro
toInt1 [] _ = 0 -- caso em que o número é o último do arquivo
toInt1 (a:b) len
    | len >= 0 =  toInt(a)*(powerten len) + toInt1 b (len-1)
    | otherwise = 0

retndig :: String -> Int
-- função que retorna a quantidade de dígitos de um inteiro
retndig [a] = 1 -- caso em que o número é o último do arquivo
retndig (a:b)
    | isDigit a = 1 + retndig b
    | otherwise = 0

remndig :: String -> String
-- função que remove de uma string os n primeiros dígitos antes do \n
remndig [] = [] -- caso em que o número é o último do arquivo
remndig (a:b)
    | a /= '\n' = remndig b
    | otherwise = b

powerten :: Int -> Int
-- função que retorna dez elevado ao número recebido
powerten 0 = 1
powerten x = 10*(powerten (x-1))

isDigit :: Char -> Bool
-- função que retorna True se um char é um dígito e False se um char não é um dígito
isDigit '0' = True
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit _ = False