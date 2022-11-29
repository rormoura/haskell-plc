-- PLC IF686 - 2022.1
-- Professor: Andre Luis de Medeiros Santos
-- Aluno: Rodrigo Rocha Moura (rrm2)
------------------------------------------------------------------------------------------------------------------
-- Primeira questão:
type Urna = (Int, [(String, Int)]) -- tipo que representa uma urna

teste :: [Urna]
teste = [(99,[("Cand1",89),("Cand2",93),("Cand3",99),("Cand4",91)]),
        (98,[("Cand1",85),("Cand2",98),("Cand3",89),("Cand4",90)]),
        (97,[("Cand1",97),("Cand2",93),("Cand3",99),("Cand4",92)])]

totalize :: [Urna] -> [(String, Int)]
-- função que totaliza os votos de cada candidato
totalize [] = []
totalize a = totalCand (concatResult a)

concatResult :: [Urna] -> [(String, Int)]
-- função que concatena em uma lista de [(String, Int)] todos os resultados das diferentes urnas
concatResult [] = []
concatResult ((a,b):c) = b ++ concatResult c

totalCand :: [(String, Int)] -> [(String, Int)]
-- função que retorna a totalização de votos por candidato, numa lista de (String, Int)
totalCand [] = []
totalCand ((a,b):c) = (a, somaVotos a ((a,b):c)) : totalCand [(x,y) | (x,y) <- c, x /= a]
-- Note que o argumento que passamos para totalCand, "[(x,y) | (x,y) <- ((a,b):c), x /= a]", retorna
-- uma lista que contém os votos por candidatos nas diferentes urnas, mas sem aqueles dados referentes
-- a candidatos já contados.

somaVotos :: String -> [(String, Int)] -> Int
-- função que soma todos os votos de um determinado candidato
somaVotos _ [] = 0
somaVotos a ((b,c):d)
    | a == b = c + somaVotos a d -- quando os nomes dos candidatos são iguais
    | otherwise = somaVotos a d -- caso contrário
-------------------------------------------------------------------------------------------------------------------------
-- Segunda Questão:
ordena ::[(String, Int)] -> [(String, Int)]
-- função que ordena uma lista de [(String, Int)] conforme a quantidade de votos de modo decrescente
-- essa função usa do algoritmo quicksort
ordena [] = []
ordena ((a,b):c) = ordena [(x,y) | (x,y) <- c, y >= b] ++ [(a,b)] ++ ordena [(x,y) | (x,y) <- c, y < b]
-- "[(x,y) | (x,y) <- c, y >= b]" retorna as tuplas (String, Int) daqueles candidatos que possuem mais (ou o mesmo número de) votos do que o selecionado
-- "[(x,y) | (x,y) <- c, y < b]" retorna as tuplas (String, Int) daqueles candidatos que possuem menos votos do que o selecionado
-------------------------------------------------------------------------------------------------------------------------
-- Terceira Questão:
frequencia :: String -> [(String, Int)]
-- função que recebe uma String e retona uma lista de pares com Strings de 2 caracteres que ocorrem mais de uma vez no texto e quantas vezes ela ocorre no texto
frequencia [] = []
frequencia [a] = []
frequencia (a:b) = retRep (remDup (quebraEmPares (a:b))) (a:b)

quebraEmPares :: String -> [String]
-- função que retorna os dois primeiros caracteres de uma String
-- por causa da função frequência tenho a garantia de que chega como argumento para quebraEmPares somente String com no mínimo 2 caracteres
quebraEmPares [] = []
quebraEmPares [a] = []
quebraEmPares (a:b:c) = (a:[b]) : quebraEmPares (b:c)

remDup :: [String] -> [String]
-- função que remove duplicatas de Strings de dois caracteres
remDup [] = []
remDup (a:b) = a : remDup [x | x <- b, x /= a]
-- Note que "[x | x <- b, x /= a]" retorna uma lista sem as String que são iguais à selecionada.

contaRep :: String -> String -> Int
-- função que conta quantas vezes uma sequência de dois caracteres aparece numa String
contaRep _ [] = 0
contaRep _ [a] = 0
contaRep a (b:c:d)
    | a == b:[c] = 1 + contaRep a (c:d)
    | otherwise = contaRep a (c:d)

retRep :: [String] -> String -> [(String, Int)]
-- função que retorna tuplas (str), num) em que str tem dois caracteres
-- e num é a quantidade de repetições que str aparece na String de entrada
-- Só são retornadas strings que aparecem mais de uma vez
retRep [] a = []
retRep (a:b) c
    | contaRep a c < 2 = retRep b c
    | otherwise = (a, contaRep a c) : retRep b c
-------------------------------------------------------------------------------------------------------------------------
-- Quarta Questão:
data Chaves = No Int String Chaves Chaves | Folha -- tipo algébrico que representa uma árvore binária que contém chaves para descompactar um arquivo

chaveTeste :: Chaves
chaveTeste = No 6 "te" (No 4 " t" Folha (No 5 "m " Folha Folha)) (No 8 "st" (No 7 "es" Folha Folha) Folha)

descompacta :: Chaves -> String -> String
-- função que descompacta um arquivo utilizando de uma árvore binária que contém chaves, a partir das quais troca-se um número por uma String
descompacta Folha a = a
descompacta _ [] = []
descompacta (No a b c d) (e:f)
    | ehDigito e = troca (No a b c d) e ++ descompacta (No a b c d) f -- quando o caractere é um dígito, chamo a função troca
    | otherwise = e : descompacta (No a b c d) f -- caso contrário

ehDigito :: Char -> Bool
-- função que retorna se um caractere é um dígito
ehDigito a
    | a == '0' = True
    | a == '1' = True
    | a == '2' = True
    | a == '3' = True
    | a == '4' = True
    | a == '5' = True
    | a == '6' = True
    | a == '7' = True
    | a == '8' = True
    | a == '9' = True
    | otherwise = False

troca :: Chaves -> Char -> String
-- função que realiza a conversão entre dígito e string
-- criei essa função auxiliar, porque ao longo da string podem haver dígitos diferentes e em qualquer ordem
-- logo, para cada dígito que encontrar preciso de toda a árvore binária
troca Folha _ = []
troca (No a b c d) e
    | x == a = b -- retorna a String correspondente à chave
    | x < a = troca c e -- se for menor, busca no ramo esquerdo
    | x > a = troca d e -- se for maior, busca no ramo direito
    where
        x = toInt e -- passando o char para um inteiro

toInt :: Char -> Int
-- função que passa um char para um inteiro
toInt a -- por causa da função descompacta tenho a garantia de que só chega dígitos como argumento para toInt
    | a == '0' = 0
    | a == '1' = 1
    | a == '2' = 2
    | a == '3' = 3
    | a == '4' = 4
    | a == '5' = 5
    | a == '6' = 6
    | a == '7' = 7
    | a == '8' = 8
    | a == '9' = 9
------------------------------------------------------------------------------------------------------------------------------------