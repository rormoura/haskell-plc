{-inv :: (t -> u -> v) -> u -> t -> v
--função que aplica uma outra função, mas que recebe os argumentos em ordem inversa
inv f a b = f b a-}
inv f = (\a b -> f b a) -- usando notação lambda

insere :: Ord t => t -> [t] -> [t]
-- função que insere um determinado elemento de maneira ordenada numa lista já ordenada
insere a [] = [a]
insere a (b:c)
    | a <= b = a : (b:c)
    | otherwise = b : insere a c

insertionSort :: [Int] -> [Int]
-- insertion sort
insertionSort [] = []
insertionSort (a:b) = insere a (insertionSort b)

twice :: (t -> t) -> (t -> t)
-- função que compõe uma função com ela mesma
twice f = f . f

iter :: Int -> (t -> t) -> (t -> t)
-- função que faz uma composição de uma função n vezes com ela mesma
iter 0 f = id
iter n f = f . (iter (n-1) f)

double :: Int -> Int
double a = 2*a