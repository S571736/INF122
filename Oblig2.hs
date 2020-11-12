-- Lage "main" metode å referere til fra den ekte main så man kan få med nødvendig data
-- Vise regler
-- gjøre automatisk
-- handle input error

-- TODO 1  read from file
-- TODO 2 Live mode
-- Fiks informasjonsmetodene
-- TODO 3 handle input error



import Data.List

menu :: [[Char]] -> [Pos] -> [Int] -> IO ()
menu matrise brett regler = do
  --print brett
  --print regler
  putStrLn ("Vennligst skriv en kommando")
  ord <- getLine
  let input = words ord
  if (null input)
    then do nextGen matrise regler
    else case (head input) of
      "c" -> do c (read $ input !! 1) regler
      "n" -> do n (map (read :: String -> Int) $ tail input) matrise brett regler -- Hjelpefunksjon for å lage liste av tupler?
      "e" -> do e (map (read :: String -> Int) $ tail input) matrise brett regler
      "b" -> do b (read $ input !! 1) (read $ input !! 2) matrise brett regler
      "s" -> do s (read $ input !! 1) (read $ input !! 2) matrise brett regler
      "?" -> help
      "l" -> do l (input !! 1)
      "w" -> do w
      "r" -> do r (unwords $ tail input) matrise brett regler
      "q" -> do quit
      otherwise -> do putStrLn "Ukjent kommando"
  goto (0, (length matrise) + 2)
  putStrLn "\ESC[0J"
  return ()

nextGen :: Matrise -> [Int] -> IO ()
nextGen matrise regler = do
  let brett = matrisetilBrett matrise
  let nyttBrett = newGeneration brett regler (length matrise)
  if (brett /= nyttBrett)
    then do
      --visBrett (brettTilMatrise matrise nyttBrett)
      visBrett (brettTilMatrise (nyMatrise (length matrise)) nyttBrett)
      menu (brettTilMatrise (nyMatrise (length matrise)) nyttBrett) nyttBrett regler
    else do
      goto (0, length matrise + 2)
      putStr "\ESC[2K"
      putStrLn "Stabil konfigurasjon oppnådd"
      menu (brettTilMatrise (nyMatrise (length matrise)) nyttBrett) nyttBrett regler

c :: (Num t, Enum t) => t -> [Int] -> IO ()
c n regler = do
  let m = nyMatrise n
  visBrett m
  menu m [] regler

n :: [Int] -> [[Char]] -> [Pos] -> [Int] -> IO ()
n (x : xx : xs) m brett regler = do
  if ((length (x : xx : xs)) `mod` 2 == 0)
    then do
      let matrise = updateMat (x, xx) m 'O'
      goto ((x * 3 + 1), xx + 1)
      putStr "O"
      goto (0, (length matrise + 2))
      putStr "\ESC[0J"
      if (xs /= [])
        then do
          n xs matrise brett regler
          return ()
        else do
          let nyttBrett = matrisetilBrett matrise
          menu matrise nyttBrett regler
    else error "List is not even, list needs to be even to work"

e :: [Int] -> [[Char]] -> [Pos] -> [Int] -> IO ()
e (x : xx : xs) m brett regler = do
  if ((length (x : xx : xs)) `mod` 2 == 0)
    then do
      let matrise = updateMat (x, xx) m '.'
      goto ((x * 3 + 1), xx + 1)
      putStr "."
      goto (0, (length matrise + 2))
      putStr "\ESC[0J"
      if (xs /= [])
        then do
          e xs matrise brett regler
          return ()
        else do
          let nyttBrett = matrisetilBrett matrise
          menu matrise nyttBrett regler
    else error "List is not even, list needs to be even to work"

b :: Int -> Int -> Matrise -> Brett -> [Int] -> IO ()
b m n matrise brett regler = do
  let s1 = regler !! 0
  let s2 = regler !! 1
  menu matrise brett [s1, s2, m, n]

s :: Int -> Int -> [[Char]] -> [Pos] -> [Int] -> IO ()
s m n matrise brett regler = do
  let b1 = regler !! 2
  let b2 = regler !! 3
  menu matrise brett [m, n, b1, b2]

help = undefined

w = undefined

-- example file contents
-- 5 (s 2 3, b 3 3) 3 3 1 3 2 1 3 2 2 3
-- 5 (b 3 3, s 2 3) 2 1 2 3 3 3 3 2 1 3


r name m b regler= do
  content <- readFile name
  goto (0, length m+2)
  putStrLn content
  menu m b regler


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
matrisetilBrett matrise = [(x + 1, y + 1) | x <- [0 .. length matrise -1], y <- [0 .. length matrise -1], matrise !! x !! y /= '.']

brettTilMatrise :: Matrise -> Brett -> Matrise
brettTilMatrise matrise [] = matrise
brettTilMatrise matrise (b : bs) = brettTilMatrise (updateMat b matrise 'O') bs

nyMatrise n = [['.' | a <- [0 .. n -1]] | b <- [0 .. n -1]]

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
  take (x -1) matrise
    ++ [ take (y -1) (matrise !! (x -1))
           ++ [tegn]
           ++ drop y (matrise !! (x -1))
       ]
    ++ drop x matrise

naboer :: Int -> Pos -> [Pos]
naboer size (x, y)
  | x == 0 && y == 0 = [(x, y + 1), (x + 1, y + 1), (x + 1, y)]
  | x == size && y == 0 = [(x -1, y), (x -1, y + 1), (x, y + 1)]
  | x == 0 && y == size = [(x, y -1), (x + 1, y -1), (x + 1, y)]
  | x == size && y == size = [(x -1, y -1), (x -1, y), (x, y -1)]
  | x == 0 = [(x, y -1), (x, y + 1), (x + 1, y + 1), (x + 1, y -1), (x -1, y)]
  | y == 0 = [(x -1, y), (x -1, y + 1), (x + 1, y + 1), (x, y + 1), (x + 1, y)]
  | x == size = [(x -1, y -1), (x -1, y), (x -1, y + 1), (x, y + 1), (x, y -1)]
  | y == size = [(x -1, y -1), (x -1, y), (x, y -1), (x + 1, y -1), (x + 1, y)]
  | x > 0 && x < size && y > 0 && y < size = [(x -1, y -1), (x -1, y), (x -1, y + 1), (x, y + 1), (x, y -1), (x + 1, y + 1), (x + 1, y -1), (x + 1, y)]
  | otherwise = []

newGeneration :: Brett -> [Int] -> Int -> Brett
newGeneration brett regler lengde = overlever brett (head regler) (regler !! 1) ++ nyeCeller brett (regler !! 2) (regler !! 3) lengde

nyeCeller b b1 b2 dim = [p | p <- removeDuplicates (concat (map (naboer (length b -1)) b)), fst p < dim, snd p < dim, tom b p, elem (levendeNaboer b p) [b1 .. b2]]

removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

overlever :: Brett -> Int -> Int -> [Pos]
overlever brett s1 s2 = [s | s <- brett, levendeNaboer brett s `elem` [s1 .. s2]]

levendeNaboer :: Brett -> Pos -> Int
levendeNaboer brett = length . filter (iLive brett) . naboer (length brett -1)

iLive :: Brett -> Pos -> Bool
iLive brett pos = pos `elem` brett

tom brett pos = not (iLive brett pos)

sleep n = sequence_ [return () | z <- [0..n]]