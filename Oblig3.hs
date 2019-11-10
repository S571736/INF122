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

-- Lager et brett med størrelse x*x
initial :: Int -> Board
initial x = replicate x x

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









nim x = undefined

chomp x = undefined