multiply :: Int -> Int -> Int
multiply a b = a*b
doubleList :: [Int] -> [Int]
-- uma função que retorna os elementos de uma lista de inteiros multiplicados por 2
doubleList = map (multiply 2)
-- essa função acima é implementada por aplicação parcial de funções
-- as funções que são aplicadas parcialmente são a map e a multiply
---------------------------------------------------------------------------
whiteSpace = " "
retornaTudoMenosEsp :: [Char] -> [Char]
-- função que retorna todos os caracteres numa string que são diferentes do espaço
retornaTudoMenosEsp = filter (\ch -> not(elem ch whiteSpace))
-- essa função acima é implementada por aplicação parcial de funções
-- as funções que são aplicadas parcialmente são filter e elem
------------------------------------------------------------------------
-- implementando a função filter (>0).map(+1)
maisUm :: [Int] -> [Int]
-- função que soma todos os elementos de uma lista de inteiros com 1
maisUm = map (+1)
-- acima vemos a aplicação parcial da função map

maiorQueZero :: [Int] -> [Int]
-- função que retorna todos os elementos maior que zero numa lista de inteiros
maiorQueZero = filter (>0)
-- aplicação parcial da função filter

m1MQZ :: [Int] -> [Int]
-- função que é a composição de filter (>0) com map (+1)
m1MQZ = filter (>0) . map (+1)
-----------------------------------------------------------------------