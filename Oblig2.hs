-- Lage "main" metode å referere til fra den ekte main så man kan få med nødvendig data

import Data.List
main = do 
    putStrLn ("Vennligst skriv en kommando")
    ord <- getLine
    let input = words ord
    if (null input) then return () else case (head input) of
        "c" -> do c (input !! 1) 
        "n" -> do n (tail input) -- Hjelpefunksjon for å lage liste av tupler?
        "e" -> do e (tail input)
        "b" -> do b (input !! 1) (input !! 2)
        "s" -> do s (input !! 1) (input !! 2)
        "?" -> help
        "l" -> do l (input !! 1)
        "w" -> do w
        "q" -> do quit
        otherwise -> do {putStrLn "Ukjent kommando"}


c n = undefined

n (x:xs) = undefined

e (x:xs) = undefined

b m n = undefined

s m n = undefined

help = undefined

w = undefined

r name = undefined

l x = undefined

quit = undefined


-- HJELPEFUNKSJONER --

goto (x,y) = putStr ("\ESC[" ++ show y ++";"++show x ++ "H")

-- clear the screen
clr :: IO ()
clr = putStr "\ESC[2J"