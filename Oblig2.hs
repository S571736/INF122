-- Lage "main" metode å referere til fra den ekte main så man kan få med nødvendig data
-- Oppdatere matrise og brett skikkelig så de følger med
-- Oppdatere brettet uten å cleare heile brettet

import Data.List

menu :: Matrise -> Brett -> IO ()
menu matrise brett = do
  
  putStrLn $ show brett
  putStrLn ("Vennligst skriv en kommando")
  ord <- getLine
  let input = words ord
  if (null input)
    then return ()
    else case (head input) of
      "c" -> do c (read $ input !! 1)
      "n" -> do n (map (read :: String -> Int) $ tail input) matrise brett -- Hjelpefunksjon for å lage liste av tupler?
      "e" -> do e (map (read :: String -> Int) $ tail input) matrise brett
      "b" -> do b (input !! 1) (input !! 2)
      "s" -> do s (input !! 1) (input !! 2)
      "?" -> help
      "l" -> do l (input !! 1)
      "w" -> do w
      "q" -> do quit
      otherwise -> do putStrLn "Ukjent kommando"
  goto (0, (length matrise) + 2)
  putStrLn "\ESC[0J"
  return ()

c n = do
  let m = nyMatrise n
  visBrett m
  menu m []

n :: [Int] -> [[Char]] -> Brett -> IO ()
n (x : xx : xs) m brett = do
  if ((length (x : xx : xs)) `mod` 2 == 0)
    then do
      let matrise = updateMat (x, xx) m 'O'
      goto ((x*3+1), xx+1)
      putStr "O"
      goto (0, (length matrise + 2))
      putStrLn "\ESC[0J"
      if (xs /= [])
        then do
          n xs matrise brett
          return ()
        else do
          let nyttBrett = matrisetilBrett matrise
          menu matrise nyttBrett
    else error "List is not even, list needs to be even to work"

e :: [Int] -> [[Char]] -> Brett -> IO ()
e (x : xx : xs) m brett = do
  if ((length (x : xx : xs)) `mod` 2 == 0)
    then do
      let matrise = updateMat (x, xx) m '.'
      if (xs /= [])
        then do
          n xs matrise brett
          return ()
        else do
          let nyttBrett = matrisetilBrett matrise
          menu matrise nyttBrett
    else error "List is not even, list needs to be even to work"

b m n = undefined

s m n = undefined

help = undefined

w = undefined

r name = undefined

l x = undefined

quit = do
  clr
  return ()

-- HJELPEFUNKSJONER --

type Matrise = [[Char]] -- Alle tegnene i matrisen

type Pos = (Int, Int) -- Posisjon

type Brett = [Pos] -- Liste med alle levende posisjoner

visBrett :: Matrise -> IO ()
visBrett [[]] = putStrLn ""
visBrett [] = putStrLn ""
visBrett m = do
  let n = length m
  clr
  putStr "   "
  goto (lft + 1, 0)
  printK (map show [1 .. n])
  lagBrett [1 .. n] m

lagBrett :: (Show a, Ord a, Num a) => [a] -> [[Char]] -> IO ()
lagBrett [] _ = return ()
lagBrett (x : xs) (y : ys) = do
  lagLinje x (y)
  lagBrett xs (ys)


lagLinje :: (Show a, Ord a, Num a) => a -> [Char] -> IO ()
lagLinje rad x = do
  goto ((if rad > 9 then (lft - 2) else lft - 1, 1 + rad))
  putStr (show rad)
  putStr " "
  putStrLn $ makeSpace x

makeSpace :: [Char] -> [Char]
makeSpace [] = []
makeSpace [x] = [x]
makeSpace (x : xs) = x : ' ' : ' ' : makeSpace xs


matrisetilBrett :: Matrise -> Brett
matrisetilBrett matrise  = [(x, y) | x <- [0 .. length matrise], y <- [0 .. length matrise], matrise !! x !! y /= '.']

brettTilMatrise :: Matrise -> Brett -> Matrise
brettTilMatrise matrise [] = []
brettTilMatrise matrise (b:bs) = brettTilMatrise (updateMat b matrise 'O') bs

nyMatrise n = [['.' | a <- [0 .. n-1]] | b <- [0 .. n-1]]

goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

printK [] = return ()
printK (x : xs) = do
  if (read x :: Integer) < 9
    then putStr (x ++ "  ")
    else putStr (x ++ " ")
  printK xs

-- clear the screen
clr :: IO ()
clr = putStr "\ESC[2J"

lft :: Int
lft = 3

updateMat (x, y) matrise tegn = do
  take x matrise
    ++ [ take y (matrise !! x)
           ++ [tegn]
           ++ drop (y + 1) (matrise !! x)
       ]
    ++ drop (x + 1) matrise