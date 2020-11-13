-- Sondre Lindaas Gjesdal
{-
pattern of problem 2.4 is (1,1) (2,2) (2,3) (1,4)
or as it would look in the file: 10 (s 0 0, b 2 2) 1 1 2 2 2 3 1 4
-}
-- TODO Lage skikkelig main
-- TODO 3 handle input error

import Data.Char (digitToInt) -- Used in line 193 and 199 in function loadFile


main = do
  putStrLn ("Do you want to start a new board (c n) or load from file (r)")
  ord <- getLine
  let input = words ord
  if (null input)
    then do main
    else case (head input) of
      "c" -> do c (read $ input !! 1) [2,3,3,3]
      "r" -> do r (unwords $ tail input)
      otherwise -> do putStrLn "wrong input"
                      main


menu :: [[Char]] -> [Pos] -> [Int] -> IO ()
menu matrise brett regler = do
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
      "?" -> help matrise brett regler
      "l" -> do l (read $ input !! 1 :: Int) matrise brett regler 0
      "w" -> do w matrise brett regler
      "r" -> do r (unwords $ tail input)
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

l :: Int -> Matrise -> [Pos] -> [Int] -> Int -> IO ()
l x m brett regler i = do
  if x > 0
    then do
      if i == x
        then do
          goto (0, length m + 2)
          putStr "\ESC[2K"
          putStrLn ("Gjennomgått " ++ show x ++ " generasjoner")
          menu (brettTilMatrise (nyMatrise (length m)) brett) brett regler
        else do
          let brett = matrisetilBrett m
          let nyttBrett = newGeneration brett regler (length m)
          sleep 1000000
          if (brett /= nyttBrett)
            then do
              visBrett (brettTilMatrise (nyMatrise (length m)) nyttBrett)
              l x (brettTilMatrise (nyMatrise (length m)) nyttBrett) nyttBrett regler (i + 1)
            else do
              goto (0, length m + 2)
              putStr "\ESC[2K"
              putStrLn "Stabil konfigurasjon oppnådd"
              menu (brettTilMatrise (nyMatrise (length m)) nyttBrett) nyttBrett regler
    else do
      putStrLn "x must be > 0"
      menu m brett regler

c :: (Num t, Enum t) => t -> [Int] -> IO ()
c n regler = do
  let m = nyMatrise n
  visBrett m
  menu m [] regler

n :: [Int] -> [[Char]] -> [Pos] -> [Int] -> IO ()
n (x : xx : xs) m brett regler = do
  if x < length m && x > 0 && xx < length m && xx > 0
    then do
      if ((length (x : xx : xs)) `mod` 2 == 0)
        then do
          let matrise = updateMat (x -1, xx -1) m 'O'
          goto ((xx * 3 + 1), x + 1)
          putStr "O"
          goto (0, (length matrise + 2))
          clrln
          if (xs /= [])
            then do
              n xs matrise brett regler
              return ()
            else do
              let nyttBrett = matrisetilBrett matrise
              menu matrise nyttBrett regler
        else do
          putStrLn "List is not even, list needs to be even to work"
          menu m brett regler
    else do
      putStrLn "Input not valid"
      menu m brett regler

e :: [Int] -> [[Char]] -> [Pos] -> [Int] -> IO ()
e (x : xx : xs) m brett regler = do
  if x < length m && x > 0 && xx < length m && xx > 0
    then do
      if ((length (x : xx : xs)) `mod` 2 == 0)
        then do
          let matrise = updateMat (x -1, xx -1) m '.'
          goto ((xx * 3 + 1), x + 1)
          putStr "."
          goto (0, (length matrise + 2))
          clrln
          if (xs /= [])
            then do
              e xs matrise brett regler
              return ()
            else do
              let nyttBrett = matrisetilBrett matrise
              menu matrise nyttBrett regler
        else do
          putStrLn "List is not even, list needs to be even to work"
          menu m brett regler
    else do
      putStrLn "Input not valid"
      menu m brett regler

b :: Int -> Int -> Matrise -> Brett -> [Int] -> IO ()
b m n matrise brett regler = do
  if m <= 0 && n <= m
    then do
      let s1 = head regler
      let s2 = regler !! 1
      menu matrise brett [s1, s2, m, n]
    else do
      putStrLn "input not valid"
      menu matrise brett regler

s :: Int -> Int -> [[Char]] -> [Pos] -> [Int] -> IO ()
s m n matrise brett regler = do
  if m <= 0 && n <= m
    then do
      let b1 = regler !! 2
      let b2 = regler !! 3
      menu matrise brett [m, n, b1, b2]
    else do
      putStrLn "input not valid"
      menu matrise brett regler

help :: Matrise -> Brett -> [Int] -> IO ()
help m b regler = do
  goto (0, length m + 2)
  clrln
  putStrLn ("s " ++ show (head regler) ++ " " ++ show (regler !! 1) ++ " b " ++ show (regler !! 2) ++ " " ++ show (regler !! 3))
  menu m b regler

w :: [[Char]] -> [(Int, Int)] -> [Int] -> IO ()
w m b regler = do
  let streng [] = []
      streng (x : xs) = " " ++ show (fst x + 1) ++ " " ++ show (snd x + 1) ++ streng xs
  goto (0, length m + 2)
  clrln
  putStrLn (streng b)
  menu m b regler

-- example file contents
-- 5 (s 2 3, b 3 3) 3 3 1 3 2 1 3 2 2 3
-- 5 (b 3 3, s 2 3) 2 1 2 3 3 3 3 2 1 3

r :: FilePath -> IO ()
r name = do
  s <- readFile name
  loadFile s

loadFile :: String -> IO ()
loadFile content = do
  let (c : r : r1 : r2 : rr : r3 : r4 : brett) = words content

  let tilBrett [] = []
      tilBrett (x : y : rest) = ((read x :: Int) -1, (read y :: Int) -1) : tilBrett rest
  let nyttBrett = tilBrett brett
  let regel2 = head r2
  let regel4 = head r4
  --assigning rules depending on if s or b comes first:
  if (last r == 's')
    then do
      let regler = [read r1 :: Int, digitToInt regel2, read r3 :: Int, digitToInt regel4]
      visBrett (brettTilMatrise (nyMatrise (read c :: Int)) nyttBrett)
      menu (brettTilMatrise (nyMatrise (read c :: Int)) nyttBrett) nyttBrett regler
    else
      if (last r == 'b')
        then do
          let regler = [read r3 :: Int, digitToInt regel4 :: Int, read r1 :: Int, digitToInt regel2 :: Int]
          visBrett (brettTilMatrise (nyMatrise (read c :: Int)) nyttBrett)
          menu (brettTilMatrise (nyMatrise (read c :: Int)) nyttBrett) nyttBrett regler
        else error "Filen mangler rett regeltype"

quit :: IO ()
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

printK :: [String] -> IO ()
printK [] = return ()
printK (x : xs) = do
  if (read x :: Integer) < 9
    then putStr (x ++ "  ")
    else putStr (x ++ " ")
  printK xs

matrisetilBrett :: Matrise -> Brett
matrisetilBrett matrise = [(x, y) | x <- [0 .. length matrise -1], y <- [0 .. length matrise -1], matrise !! x !! y /= '.']

brettTilMatrise :: Matrise -> Brett -> Matrise
brettTilMatrise matrise [] = matrise
brettTilMatrise matrise (b : bs) = brettTilMatrise (updateMat b matrise 'O') bs

nyMatrise :: (Num t, Enum t) => t -> [[Char]]
nyMatrise n = [['.' | a <- [0 .. n -1]] | b <- [0 .. n-1]]

goto :: (Show a1, Show a2) => (a2, a1) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")



-- clear the screen
clr :: IO ()
clr = putStr "\ESC[2J"

clrln :: IO ()
clrln = putStr "\ESC[0J"

lft :: Int
lft = 3

updateMat :: (Int, Int) -> [[a]] -> a -> [[a]]
updateMat (x, y) matrise tegn = do
  take x matrise ++ [take y (matrise !! x) ++ [tegn] ++ drop (y + 1) (matrise !! x)] ++ drop (x + 1) matrise

-- HANDLING NEIGHBOURS AND SUCH --
naboer :: Int -> Pos -> [Pos]
naboer size (x, y)
  | x == 0 && y == 0 = [(x, y + 1), (x + 1, y + 1), (x + 1, y)]
  | x == size && y == 0 = [(x -1, y), (x -1, y + 1), (x, y + 1)]
  | x == 0 && y == size = [(x, y -1), (x + 1, y -1), (x + 1, y)]
  | x == size && y == size = [(x -1, y -1), (x -1, y), (x, y -1)]
  | x == 0 = [(x, y + 1), (x, y -1), (x + 1, y + 1), (x + 1, y -1), (x + 1, y)]
  | y == 0 = [(x -1, y), (x -1, y + 1), (x, y + 1), (x + 1, y + 1), (x + 1, y)]
  | x == size = [(x -1, y -1), (x -1, y), (x -1, y + 1), (x, y + 1), (x, y -1)]
  | y == size = [(x -1, y -1), (x -1, y), (x, y -1), (x + 1, y -1), (x + 1, y)]
  | x > 0 && x < size && y > 0 && y < size = [(x -1, y -1), (x -1, y), (x -1, y + 1), (x, y + 1), (x, y -1), (x + 1, y + 1), (x + 1, y -1), (x + 1, y)]
  | otherwise = []

newGeneration :: [Pos] -> [Int] -> Int -> [Pos]
newGeneration brett regler lengde = overlever brett (head regler) (regler !! 1) lengde ++ nyeCeller brett (regler !! 2) (regler !! 3) lengde

nyeCeller :: [Pos] -> Int -> Int -> Int -> [Pos]
nyeCeller b b1 b2 dim = [p | p <- removeDuplicates (concat (map (naboer (dim)) b)), fst p < dim, snd p < dim, tom b p,  (levendeNaboer b dim p) `elem` [b1 .. b2]]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : removeDuplicates (filter (/= x) xs)

overlever :: [Pos] -> Int -> Int -> Int -> [Pos]
overlever brett s1 s2 lengde = [s | s <- brett,  (levendeNaboer brett lengde s) `elem` [s1 .. s2]]

levendeNaboer :: Brett -> Int -> Pos -> Int
levendeNaboer brett lengde = length . filter (iLive brett) . (naboer lengde)

iLive :: Brett -> Pos -> Bool
iLive brett pos = elem pos brett

tom :: Brett -> Pos -> Bool
tom brett pos = not (iLive brett pos)

sleep :: (Monad m, Num t, Enum t) => t -> m ()
sleep n = sequence_ [return () | z <- [0 .. n]]