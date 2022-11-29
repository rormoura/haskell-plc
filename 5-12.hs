data Shape = Circle Float | Rectangle Float Float

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle a b) = a * b
----------------------------------------------------------
data Expr = Lit Int | Add Expr Expr | Sub Expr Expr deriving (Show)

showExpr :: Expr -> String
-- função que transforma uma expressão em uma string
showExpr (Lit n) = show n
showExpr (Add exp1 exp2) = "(" ++ showExpr exp1 ++ "+" ++ showExpr exp2 ++ ")"
showExpr (Sub exp1 exp2) = "(" ++ showExpr exp1 ++ "-" ++ showExpr exp2 ++ ")"
--------------------------------------------------------------
data ListaEnc t = Null | ListNode t (ListaEnc t) deriving(Show)

toList :: ListaEnc t -> [t]
-- função que transforma uma lista encadeada em uma lista simples
toList (Null) = []
toList (ListNode a b) = a : toList b

fromList :: [t] -> ListaEnc t
-- função que transforma uma lista simples em uma lista encadeada
fromList [] = Null
fromList (a:b) = ListNode a (fromList b)
-----------------------------------------------------------------
data Tree t =   NilT |
                Node t (Tree t) (Tree t)
                deriving (Eq, Show)

depth :: Tree t -> Int
-- função que calcula a profundidade de uma árvore 
depth NilT = 1
depth (Node a b c) = 1 + max (depth b) (depth c)

collapse :: Tree t -> [t]
-- função que transforma uma árvore em uma lista
-- utilizando de busca por largura
collapse (NilT) = []
collapse (Node a b c) = a : collapse b ++ collapse c

mapTree :: (t -> u) -> Tree t -> Tree u
-- função map só que para árvore, ou seja,
-- aplica uma determinada função recebida como argumento
-- em cada nó da árvore
mapTree f NilT = NilT
mapTree f (Node a b c) = (Node (f a) (mapTree f b) (mapTree f c))