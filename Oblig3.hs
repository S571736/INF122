import Data.Char
import Data.List
-- Forklarer hva det gjør
spill :: IO()
spill = do
    putStrLn ("Hva ønsker du å spille? \n" ++ "n x for nim \n" ++ "c x for chomp \n" ++ "q hvis du ønsker å avslutte")
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



-- Sjekker at brettet er tomt for stjerner
finished :: Board -> Bool
finished = all (== 0)

-- Sjekker at det er nok stjerner på brettet
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

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


{- old possibly useful
putBoard :: Board -> Char -> Int -> IO ()
putBoard (x:xs) game n = if game == 'n' then putBoard' (x:xs) n 1 else putBoardc' (x:xs) n 1
-}

putBoard (x:xs) n = do 
        putBoardS' (x:xs) 0
        putLastRow (length (x:xs))

putLastRow l = sequence_ (putStr "  ": [putStr ((show i)++" ") | i <- [1..l]]++[putStrLn""])

putBoardS' [] n = putRow 0 0
putBoardS' (x:xs) n = do 
                        putRow (n+1) x
                        putBoardS' xs (n+1)                      

{-
putBoard' [] n b = putRow 0 0
putBoard' (x:xs) n b = do 
                        putRow b x
                        putBoard' xs n (b+1)
-}

{-???
putBoardc' [] n b = putRow 0 0
putBoardc' (x:xs) n b = do 
                        putRow b n
                        putBoardc' xs n (b+1)
-}


getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERRRRRRRRRRRROOOOOORRRR"
                           getDigit prompt

spillN :: Board -> Int -> IO ()
spillN brett spiller = 
        do newline
           putBoard brett 0
           if finished brett then 
              do newline
                 if spiller == 1 then
                    do putStr "Datamaskinen vant!\n"
                 else 
                    do putStr "Du vant!\n"
           else
              do newline
                 putStr "Spiller "
                 putStrLn (show spiller)
                 if spiller == 1 then
                    do 
                       row <- getDigit "Legg inn radnummer: "
                       num <- getDigit "Stjerner som skal fjernes: "
                 else 
                        {-
                        her kommer ai
                        -}
                 if valid brett row num then
                    spillN (move brett row num) (next spiller)
                 else
                 do newline
                    putStrLn "Ugyldig trekk"
                    spillN brett spiller



nim x = spillN (initial (read x :: Int)) 1
chomp x = undefined