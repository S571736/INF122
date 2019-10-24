{--
data LL = L1 | L1 LL | L1, LL
data L1 = T = T
data T = Var | Kon | Fun

Var ::= String (Ts)
data Ts = T | T, Ts
Kon ::= String --Stor bokstav
Fun ::= String --Små bokstav
--}

import Data.Char
data Ast = V String | N String [Ast] deriving (Show, Read)

tokenize [] t s = []
tokenize (xr:xs) t s | elem xr t = [xr] : tokenize xs t s
                    | elem xr s = tokenize xs t s
                    | otherwise = (takeWhile (notin (t++s)) (xr:xs)) : tokenize
                    (dropWhile (notin (t++s)) (xr:xs)) t s

notin xs = \x -> not (elem x xs)

tokens str = tokenize str "()=" ", "

liste = "X=C A=f(X,E) A=f(E,D) B=ga(C,D), Y=f(A,B)"
stuff = tokens "X=C A=f(X,E) A=f(E,D) B=ga(C,D), Y=f(A,B)"

parse :: [String] -> [(Ast, Ast)]

parse [] = []
parse xs = let (t1, "=":r1) = parseT xs ; (t2, r2) = parseT r1 in (t1, t2):(parse r2)

parseT :: [String] -> (Ast, [String])
parseT (x:"(":xs) = let (args, r1) = parseArg xs [] in (N x args, r1)
parseT (x:xs) = if isUpper(head x) then (V x, xs)
                else (N x [], xs) 

parseArg (")":xs) ar = (ar,xs)
parseArg (xs) ar = let (t1, r1) = parseT xs in parseArg r1 (ar++[t1])