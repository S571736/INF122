import Data.Char
import Data.List
-- Forklarer hva det gjør
spill :: IO()
spill = do
    putStrLn ("n(im) x / c(homp) / q(uit)")
    ord <- getLine
    let input = words ord
    if (null input) then return () else case (head input) of
        "q" -> do return ()
        "n" -> do nim (last input)
        "c" -> do chomp (last input)
        otherwise -> do {putStrLn "Ukjent kommando"}

next :: Int -> Int
next 1 = 2
next 2 = 1

-- Sier at brett er liste av inter
type Board = [Int]

-- Lager et brett x antall rader
initial :: Int -> Board
initial x = [1..x]

initialc x = replicate x x



-- Sjekker at brettet er tomt for stjerner
finished :: Board -> Bool
finished = all (== 0)

-- Sjekker at det er nok stjerner på brettet
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

validc board row col

-- Gjør eit sick move
move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
        where update r n = if r == row then n-num else n


newline :: IO ()
newline = putChar '\n'

putRow :: Int -> Int -> IO ()
putRow 0 0 = putStr ""
putRow row num = do putStr (show row) 
                    putStr  " "
                    putStrLn (concat (replicate num "* "))



putBoard (x:xs) n = do 
        putBoardS' (x:xs) 0
        putLastRow (length (x:xs))

putLastRow l = sequence_ (putStr "  ": [putStr ((show i)++" ") | i <- [1..l]]++[putStrLn""])

putBoardS' [] n = putRow 0 0
putBoardS' (x:xs) n = do 
                        putRow (n+1) x
                        putBoardS' xs (n+1)                      

{-???
putBoardc' [] n b = putRow 0 0
putBoardc' (x:xs) n b = do 
                        putRow b n
                        putBoardc' xs n (b+1)
-}

antall x y = x - y


ai :: Board -> [Int] -> [(Int, Int)]
ai board [] = [(1,1)]
ai board (x:xs) = 
       do
          let target = foldr (^) 0 board
          let i = (length (x:xs))
          if target < x then do
          --let num = x-target
             if valid board i (x-target) && (x-target) > 0 && i > 0 then
                return (i, (x-target))
             else [(i+1, (x-target+1))]   
          else
             ai board xs

{-
getDigit :: String -> Char -> Int
getDigit inn = do
        let x = inn
        if isDigit x then
           return (digitToInt x)
        else
           do putStrLn ""
              return x
-}

spillN :: Board -> Int -> IO ()
spillN brett spiller = 
        do newline
           putBoard brett 0
           if finished brett then 
              do newline
                 if spiller == 1 then
                    do putStr "Datamaskinen vant!\n"
                       spill
                 else 
                    do putStr "Du vant!\n"
                       spill
           else
              do newline
                 putStr "Spiller "
                 putStrLn (show spiller)
                 if spiller == 1 then --Spiller sin tur
                    do putStrLn "r a / ? / q"
                       inn <- getLine
                       if (null inn) then spillN brett spiller else case (head inn) of
                          'q' -> do spill
                          '?' -> do hjelpN
                          otherwise -> do let row = digitToInt (head inn)
                                          let num = read (tail inn)::Int
                                          if valid brett row num then
                                             spillN (move brett row num) (next spiller)
                                          else
                                             do newline
                                                putStrLn "Ugyldig trekk"
                                                spillN brett spiller
                 else --Maskinen sin tur
                    do 
                       let tuppel = ai brett brett
                       let row = fst (head tuppel)
                       let num = snd (head tuppel)
                       putStr (show row)
                       putStr (show num)
                       if valid brett row num && num > 0 then
                          spillN (move brett row num) (next spiller)
                       else
                         do newline
                            putStrLn "Ugyldig trekk"
                            spillN brett spiller


nim x = spillN (initial (read x :: Int)) 1
chomp x = undefined

hjelpN = putStrLn "r a = fjern a brikker fra rad r. Vinner den som tar siste brikke."