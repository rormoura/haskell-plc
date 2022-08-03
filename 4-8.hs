agrupar :: Eq t => [[t]] -> [(t, Int)]
agrupar a = contaAi (concatenaAi a)

concatenaAi :: [[t]] -> [t]
concatenaAi [] = []
concatenaAi (a:b) = a ++ concatenaAi b

contaAi :: Eq t => [t] -> [(t, Int)]
contaAi [] = []
contaAi (a:b) = (a, contaAiDnv a (a:b)) : contaAi (removeAi a b)

contaAiDnv :: Eq t => t -> [t] -> Int
--função que conta quantas vezes um determinado 't' aparece numa lista
contaAiDnv _ [] = 0
contaAiDnv a (b:c)
    | a == b = 1 + contaAiDnv a c
    | otherwise = contaAiDnv a c

removeAi :: Eq t => t -> [t] -> [t]
--função que remove todas as aparições de um determinado 't' em uma lista
removeAi _ [] = []
removeAi a (b:c)
    | a == b = removeAi a c
    | otherwise = b : removeAi a c