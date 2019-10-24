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


app :: (Ast, Ast) -> Ast -> Ast
app (V x, t)(V y) = if x==y then t else (V y)
app (V x, t)(N n args) = N n (map (app (V x,t)) args)

appE :: (Ast, Ast) -> (Ast, Ast) -> (Ast, Ast)
appE s (v, h) = (app s v, app s h)
appL s eqs = map (appE s) eqs 

uni :: [(Ast, Ast)] -> [(Ast, Ast)] -> [(Ast, Ast)]
uni ls [] = if ferdig ls then ls else uni [] ls
uni ls ((N y ar, V x):rs) = uni ls ((V x, N y ar):re)
uni ls ((V x, N y ar):rs) = if occFeil (V x) (N y ar) then error "Occurs check feil ved"++(visAst (V x))++ "og "++ (visAst (N y ar))
                                                    else uni (appL ((V x, N y ar) ls) ++[(V x, N y ar)]) (appL (V x, N y ar) rs)

uni ls (N y ax, N y ay):r = if x == y then 
                            else error "ulike funksjonsnavn"


visAst (V x) = x
visAst (N f []) = f
visAst (N f args) = f++"("++visL args ++ ")"
visL []=[]
visL [x] = visAst x
visL (x:xs) = visAst x++","++visL xs
visE (a,b) = visAst a ++ " = " ++ visAst b
visLE xs = map visE xs