import Data.Char
---------------------------------------------------------
{- A seguinte função analisa uma tupla (a,b,c), que corresponde
    aos coeficientes de uma equação do 2º grau e retorna se
    tem duas raízes reais (e tbm as raízes), se só tem uma raíz real
    (e a raíz tbm) e se não tem raízes reais. -}
eq2grau :: (Float, Float, Float) -> (String, [Float])
eq2grau (a,b,c)
    | b*b > 4.0*a*c = ("Tem duas raizes reais", [(-b + sqrt(b*b-4.0*a*c))/(2.0*a), (-b - sqrt(b*b-4.0*a*c))/(2.0*a)])
    | b*b == 4.0*a*c = ("Tem somente uma raiz real", [(-b)/(2.0*a)])
    | otherwise = ("Nao tem raiz real", [])
------------------------------------------------------------
{- A seguinte função recebe três inteiros e retorna uma 2-upla
    com o menor (esquerda) e o maior (direita) deles.-}
menorMaior :: Int -> Int -> Int -> (Int, Int)
menorMaior a b c
    | a <= b && b <= c = (a, c)
    | a <= c && c <= b = (a, b)
    | b <= c && c <= a = (b, a)
    | b <= a && a <= c = (b, c)
    | c <= b && b <= a = (c, a)
    | c <= a && a <= b = (c, b)
-------------------------------------------------------------
{- A seguinte função recebe uma 3-upla de inteiros e a ordena
    em ordem não decrescente. -}
ordenaTripla :: (Int, Int, Int) -> (Int, Int, Int)
ordenaTripla (a,b,c)
    | a <= b && b <= c = (a, b, c)
    | a <= c && c <= b = (a, c, b)
    | b <= c && c <= a = (b, c, a)
    | b <= a && a <= c = (b, a, c)
    | c <= b && b <= a = (c, b, a)
    | c <= a && a <= b = (c, a, b)
------------------------------------------------------------
{- Os seguintes tipos e funções trabalham em conjunto a fim
    de retornar a primeira coord de um ponto, a segunda coord
    de um ponto e se uma reta é vertical ou não. -}
type Ponto = (Float, Float)
type Reta = (Ponto , Ponto)

primcoord :: Ponto -> Float
primcoord a = fst a
--primcoord (a, b) = "x: "++show(a)

segcoord :: Ponto -> Float
segcoord a = snd a
--segcoord (a, b) = "y: "++show(b)

ehvertical :: Reta -> Bool
ehvertical ((a, b), (c, d)) = (a == c)
  --  | a == c = True
 --   | otherwise = False
-------------------------------------------------------------
{- A seguinte função recebe uma coordenada x e uma reta r, 
    retorna a coordenada y, de modo que (x,y) perteça a r.-}
pontoY :: Float -> Reta -> String
pontoY x ((a, b), (c, d))
    | (c-a) == 0 && x == a = "qualquer valor real"
    | (c-a) == 0 && x /= a = "a reta eh x = "++show(a)++", logo nao ha respostas"
    | otherwise = show(b + (d-b)*(x-a)/(c-a))