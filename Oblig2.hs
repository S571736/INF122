-- Sondre Lindaas Gjesdal

module Oblig2 where

import Data.Char

data Ast = Tall Int | Sum Ast Ast | Mult Ast Ast | Min Ast |  Var String   deriving (Eq, Show)
-- denne definisjonen av Ast utvides med manglende bitene for Mult og Min 

--parse :: String -> Ast

parse str = parseE [str]

onlyDigits x = takeWhile isDigit x == x

parseE :: [String] -> (Ast, [String])

parseE ("+":rs) = let(e1, r1) = parseE rs ;
                     (e2, r2) = parseE r1 in (Sum e1 e2, r2)

parseE ("*":rs) = let(e1, r1) = parseE rs ;
                     (e2, r2) = parseE r1 in (Mult e1 e2, r2)
            
parseE ("-":rs) = let(e1, r1) = parseE r1 in (Min e1, r1)

parseE (x:rs) = if (onlyDigits x) then (Tall (read x :: Int), rs)
                        else error ("Syntaksfeil ved " ++ x)
parseE ([]:rs) = []
                        


viss :: Ast -> String

viss ast = undefined

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


