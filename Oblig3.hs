import Data.Char
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

newline = putChar '\n'

putRow :: Int -> Int -> IO ()
putRow 0 _ = putStrLn (show (initial 5))
putRow row num = do putStr (show row) 
                    putStr  ": "
                    putStrLn (concat (replicate num "* "))


applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)

-- tanken: henter ut hver linje rekursivt
--putBoard :: Board -> IO ()
--putBoard board = do putStrln $ show (length board) ++ " : " ++ putRow (last board) (length )


putBoard :: Board -> Char -> Int -> IO ()
putBoard (x:xs) game n = if game == 'n' then putBoard' (x:xs) n 1 else putBoardc' (x:xs) n 1


putBoard' [] n b = putRow 0 0
putBoard' (x:xs) n b = do 
                        putRow b x
                        putBoard' xs n (b+1)

putBoardc' [] n b = putRow 0 0
putBoardc' (x:xs) n b = do 
                        putRow b n
                        putBoardc' xs n (b+1)

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else
                        do putStrLn "ERRRRRRRRRRRROOOOOORRRR"
                           getDigit prompt

nim x = undefined

chomp x = undefined