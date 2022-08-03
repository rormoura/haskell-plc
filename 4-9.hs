sqr :: Int -> Int
sqr n = n * n

isCrescent :: (Int -> Int) -> Int -> Bool
-- Dada uma função, verificar se ela é crescente em um intervalo de 0 a n
isCrescent f x
    | x == 1 && (f 1 > f 0) = True
    | x == 1 && (f 1 <= f 0) = False
    | (f x > f (x-1)) = isCrescent f (x-1)
    | otherwise = False

elevaAoQuadrado :: [Int] -> [Int]
-- eleva os itens de uma lista ao quadrado
elevaAoQuadrado x = [a*a | a <- x]

somaDosQuadrados :: [Int] -> Int
-- retorna a soma dos elementos de uma lista, mas com os elmentos elevados ao quadrado
somaDosQuadrados x = foldl (+) 0 (elevaAoQuadrado x)

maioresQueZero :: [Int] -> [Int]
-- retorna os elementos que são maiores que zero utilizando a função filter
maioresQueZero x = filter maiorQueZero x
    where
        maiorQueZero a = (a > 0)

takeWhile' :: (t -> Bool) -> [t] -> [t]
takeWhile' f [] = []
takeWhile' f (a:b)
    | f a == True = a : takeWhile' f b
    | otherwise = []

dropWhile' :: (t -> Bool) -> [t] -> [t]
dropWhile' f [] = []
dropWhile' f (a:b)
    | f a == True = dropWhile' f b
    | otherwise = (a:b)