-- Sondre Lindaas Gjesdal

module Oblig2 where

import Data.Char

data Ast = Tall Int | Min Ast | Sum Ast Ast | Mult Ast Ast | Var String   deriving (Eq, Show)
-- denne definisjonen av Ast utvides med manglende bitene for Mult og Min 

parse :: String -> Ast

parse str = getFirst (parseExpr (tokenize str "+*-" " "))

onlyDigits x = takeWhile isDigit x == x

parseExpr :: [String] -> (Ast, [String])

parseExpr ("+":rs) = let(e1, r1) = parseExpr rs ;
                        (e2, r2) = parseExpr r1 in (Sum e1 e2, r2)

parseExpr ("*":rs) = let(e1, r1) = parseExpr rs ;
                        (e2, r2) = parseExpr r1 in (Mult e1 e2, r2)
            
parseExpr ("-":rs) = let(e1, r1) = parseExpr rs ; 
                        (e2, r2) = parseExpr r1 in (Min e1, r2)

parseExpr (x:rs) = if (onlyDigits x) then (Tall (read x :: Int), rs)
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



viss :: Ast -> String

viss ast = undefined
--tar inn Ast
--gjør om til String
--må ha \n for å få til forskjellige linjer, kanskje ANSI for å flytte ut
-- sammensmekking av evt fleire mindre strenger til the big boi
--Sender til vis som bruker putStr


vis :: Ast -> IO ()

vis ast = putStr (viss ast)

evi :: Ast -> Int

evi str = undefined

evb :: Ast -> Bool

evb str = undefined

evix :: Ast -> Int -> Int

evix str i = undefined

evbx :: Ast -> Int -> Bool

evbx str i = undefined


