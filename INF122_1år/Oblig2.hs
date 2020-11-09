-- Sondre Lindaas Gjesdal

module Oblig2 where

import Data.Char

data Ast = Tall Int | Min Ast | Sum Ast Ast | Mult Ast Ast | Var String   deriving (Eq, Show)
-- denne definisjonen av Ast utvides med manglende bitene for Mult og Min 

parse :: String -> Ast

parse str = getFirst (parseExpr (tokenize str "+*-" " "))

onlyDigits x = takeWhile isDigit x == x
stringCheck x = takeWhile isLetter x == x

parseExpr :: [String] -> (Ast, [String])

parseExpr ("+":rs) = let(e1, r1) = parseExpr rs ;
                        (e2, r2) = parseExpr r1 in (Sum e1 e2, r2)

parseExpr ("*":rs) = let(e1, r1) = parseExpr rs ;
                        (e2, r2) = parseExpr r1 in (Mult e1 e2, r2)
            
parseExpr ("-":rs) = let(e1, r1) = parseExpr rs ; 
                        (e2, r2) = parseExpr r1 in case 


parseExpr (x:rs) = if (onlyDigits x)
                     then (Tall (read x :: Int), rs)
                    else if(stringCheck x)
                        then(Var (read (show x) :: String), rs)
                else error ("Syntaksfeil ved " ++ x)

getFirst :: (a, b) -> a
getFirst (a,b) = fst (a, b)

tokenize :: [Char] -> [Char] -> String ->  [String]
tokenize [] t s = []
tokenize (xr:xs) t s | elem xr t = [xr] : tokenize xs t s
                    | elem xr s = tokenize xs t s
                    | otherwise = (takeWhile (notin (t++s)) (xr:xs)) : tokenize
                    (dropWhile (notin (t++s)) (xr:xs)) t s
                    
notin ls = \z -> not (elem z ls)

fkdInd :: String
fkdInd = "   "

draw :: Ast -> String -> String
draw (Sum x y) indent = indent ++ ("Sum\n") ++ draw x fkdInd ++ "\n" ++ draw y fkdInd
draw (Mult x y) indent = indent ++ ("Mult\n") ++ indent ++ draw x fkdInd ++ "\n" ++ indent ++ draw y fkdInd
draw (Min x) indent = indent ++ ("Min\n") ++ indent ++ draw x fkdInd ++ "\n"
draw (Var x) indent = indent ++ ("Var ") ++ (read (show x))
draw (Tall x) indent = indent ++ ("Tall ") ++ show x

viss :: Ast -> String

viss ast = draw ast ""


vis :: Ast -> IO ()

vis ast = putStr (viss ast)


folde' :: (Int -> t) -> (t -> t -> t) -> (t -> t -> t) -> (t -> t) -> Ast -> Int -> t
folde' t s mul min (Tall x) i = t x
folde' t s mul min (Sum x y) i = s (folde' t s mul min x i) (folde' t s mul min y i)
folde' t s mul min (Mult x y) i = mul (folde' t s mul min x i) (folde' t s mul min y i)
folde' t s mul min (Min x) i = min (folde' t s mul min x i) 
folde' t s mul min (Var x) i = t i


evi :: Ast -> Int

evi str = folde' (id) (+) (*) (negate) str 0

evb :: Ast -> Bool

evb str = folde' (odd) (||) (&&) (not) str 0

evix :: Ast -> Int -> Int

evix str i = folde' (id) (+) (*) (negate) str i

evbx :: Ast -> Int -> Bool

evbx str i = folde' (odd) (||) (&&) (not) str i


