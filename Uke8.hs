{--
data LL = L1 | L1 LL | L1, LL
data L1 = T = T
data T = Var | Kon | Fun

Var ::= String (Ts)
data Ts = T | T, Ts
Kon ::= String --Stor bokstav
Fun ::= String --Små bokstav
--}
data Ast = V String | N String [Ast] deriving (Show, Read)

tokenize [] t s = []
tokenize (xr:xs) t s | elem xr t = [xr] : tokenize xs t s
                    | elem xr s = tokenize xs t s
                    | otherwise = (takeWhile (notin (t++s)) (xr:xs)) : tokenize
                    (dropWhile (notin (t++s)) (xr:xs)) t s

notin xs = \x -> not (elem x xs)

tokens str = tokenize str "(=" ", "

stuff = tokens "X=C A=f(X,E) A=f(E,D) B=ga(C,D), Y =f(A,B)"

parse :: Strin -> [(Ast, Ast)]

parse [] = []
parse xs = let (t1, "=": r1) = parseT xs ; (t2, r2) = parseT r1 in (t1, t2)

parseT