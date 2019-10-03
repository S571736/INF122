-- ditt Navn og Etternavn 

module Oblig2 where

import Data.Char

data Ast = Tall Int | Sum Ast Ast | Var String  deriving (Eq, Show)
-- denne definisjonen av Ast utvides med manglende bitene for Mult og Min 

parse :: String -> Ast

parse str = undefined

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
