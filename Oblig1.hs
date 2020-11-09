-- a comment with your forename + surname and group
-- Sondre Lindaas Gjesdal || Group 3
module Oblig1 where
import Data.Char

data Ast
    = Word String
    | Num Int
    | Mult Ast Ast
    | Plus Ast Ast
    | Minus Ast Ast
    deriving (Eq, Show)

parse :: String -> Ast
parse str = fst (parsePres (tokenize str))

-- Method for parsing normal presedence
parsePres :: [String] -> (Ast, [String])
parsePres (s) = let (a,z) = parseMult (s) in
    if null z then (a,z)
    else if head(z) == "+" then
        let (c, rest) = parsePres(tail(z)) in (Plus a c, rest)
    else if head(z) == "-" then
        let (c, rest) = parsePres(tail(z)) in (Minus a c, rest)
    else (a,z)

-- Method called whenever multiplying to get right presedence
parseMult :: [String] -> (Ast, [String])
parseMult (s) = let (a,z) = parseNum (s) in 
    if null z then (a,z)
    else if isWord a then if head z == "*" then error "cannot multiply Term with something else"
    else (a,z) 
    else if head(z) == "*" then
        let (c, rest) = parseMult(tail(z)) in (Mult a c, rest)
    else (a,z)

parseNum :: [String] -> (Ast, [String])
parseNum (x:s)
    | all isDigit x
    = (Num (read x :: Int) , s)
    | all isAlpha x
    = (Word (read (show x) :: String), s)
parseNum _ = error "Invalid expression, could not parse"

isWord :: Ast -> Bool
isWord (Word w) = True
isWord _ = False 

isNum :: Ast -> Bool
isNum (Num w) = True
isNum _ = False 

tokenize :: String -> [String]
tokenize "" = []
tokenize ('+':xs) = "+" : tokenize xs
tokenize ('*':xs) = "*" : tokenize xs
tokenize ('-':xs) = "-" : tokenize xs
tokenize (x:xs)
    | isDigit x
    = takeWhile isDigit (x:xs) : tokenize (dropWhile isDigit (x:xs))
    | isAlpha x
    = takeWhile isAlpha (x:xs) : tokenize (dropWhile isAlpha (x:xs))
    | isSpace x
    = tokenize (dropWhile isSpace xs)
    | otherwise
    = error ("Could not tokenize unexpected token " ++ show x)



indent s 0 = []
indent s i = s ++ indent s (i - 1)

draw :: Ast -> Int -> String
draw (Plus x y) i = ("Plus\n") ++ indent " " i ++ draw x (i + 3) ++ "\n" ++ indent " " i ++ draw y (i + 3)
draw (Minus x y) i = ("Minus\n") ++ indent " " i ++ draw x (i + 3) ++ "\n" ++ indent " " i ++ draw y (i + 3) 
draw (Mult x y) i = ("Mult\n") ++ indent " " i ++ draw x (i + 3) ++ "\n" ++ indent " " i ++ draw y (i + 3)
draw (Word x) i = ("Word ") ++ (read (show x))
draw (Num x) i = ("Num ") ++ show x

viss :: Ast -> String
viss ast = draw ast 3

vis :: Ast -> IO ()
vis ast = putStr (viss ast ++ "\n")

eval :: Ast -> String
eval str = evalExpr str

evalExpr :: Ast -> String 
evalExpr (Word x) = x
evalExpr (Num x) = show x
evalExpr (Mult x y) = if isNum(x) 
    then indent (evalExpr y) (toNum x) 
    else error "first operator need to be Num to evaluate"
evalExpr (Plus x y) = evalExpr x ++ evalExpr y
evalExpr (Minus x y) = diff (evalExpr x) (evalExpr y)
evalExpr _ = error "Could not evaluate your expression!"


toString :: Ast -> String
toString (Word x) = x


toNum :: Ast -> Int
toNum (Num x) = x


rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) y = if x == y then xs else x : rem1 xs y


diff :: Eq a => [a] -> [a] -> [a]
diff [] _ = []
diff x [] = x
diff (x:xs) (y:ys) = diff (rem1 (x:xs) y) ys
