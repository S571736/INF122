import Data.Char
import Data.List
-- Hovedmenyen
generiskSpill :: IO()
GeneriskSpill = do
    putStrLn ("n(im) x / c(homp) x / q(uit)")
    ord <- getLine
    let input = words ord
    if (null input) then return () else case (head input) of
        "q" -> do return ()
        "n" -> do nim (last input)
        "c" -> do chomp (last input)
        otherwise -> do {putStrLn "Ukjent kommando"}

-- Veksler mellom spillere
next :: Int -> Int
next 1 = 2
next 2 = 1

-- Sier at brett er liste av inter
type Board = [Int]

-- Lager et brett x antall rader
initial :: Int -> Board
initial x = [1..x]

-- Lager et Chomp brett
initialc :: Int -> Board
initialc x = replicate x x



-- Sjekker at brettet er tomt for stjerner
finished :: Board -> Bool
finished = all (== 0)

-- Sjekker at det er nok stjerner på brettet, c markerer for Chomp
valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

validc :: Board -> Int -> Int -> Bool
validc board row col = (length board) >= row && board!!(row-1) >= col



-- Gjør eit sick move, c markerer for Chomp
move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
        where update r n = if r == row then n-num else n

movec :: Board -> Int -> Int -> Board
movec board row col = [update r c | (r,c) <- zip [1..] board]
        where update r c = if r <= row then (col-1) else c

-- Legger inn ei ny linje
newline :: IO ()
newline = putChar '\n'

-- Printer ein rad
putRow :: Int -> Int -> IO ()
putRow 0 0 = putStr ""
putRow row num = do putStr (show row) 
                    putStr  " "
                    putStrLn (concat (replicate num "* "))


-- Printer heile brettet
putBoard (x:xs) n = do 
        putBoardS' (x:xs) 0
        putLastRow (length (x:xs))

-- Siste linje på brettet for å få med kolonnene
putLastRow l = sequence_ (putStr "  ": [putStr ((show i)++" ") | i <- [1..l]]++[putStrLn""])

-- Hjelpemetode for å printe ut brettet
putBoardS' [] n = putRow 0 0
putBoardS' (x:xs) n = do 
                        putRow (n+1) x
                        putBoardS' xs (n+1)                      


-- Maskinens algoritme for valg i Nim
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


--aic n (x:xs) forsøk på å begynne på algoritme for maskinen til chomp
-- Hovedmetoden for å spille Nim
spillN :: Board -> Int -> IO ()
spillN brett spiller = 
        do newline
           putBoard brett 0
           if finished brett then 
              do newline
                 if spiller == 1 then
                    do putStr "Du vant!\n"
                       generiskSpill
                 else -- Mulig kem som vinner burde byttes om på her eller på nim, var ikkje lett å forstå fra oppgaveteksten
                    do putStr "Datamaskinen vant!\n"
                       generiskSpill
           else
              do newline
                 putStr "Spiller "
                 putStrLn (show spiller)
                 if spiller == 1 then --Spiller sin tur
                    do putStrLn "r a / ? / q"
                       inn <- getLine
                       if (null inn) then spillN brett spiller else case (head inn) of
                          'q' -> do generiskSpill
                          '?' -> do hjelpN
                                    input <- getLine
                                    spillN brett spiller
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

-- Chomp sin hovedmetode
spillC brett spiller =
        do newline
           putBoard brett 0
           if finished brett then
              do newline
                 if spiller == 1 then
                    do putStr "Datamaskinen vant!"    
                       generiskSpill
                 else -- Mulig kem som vinner burde byttes om på her eller på nim, var ikkje lett å forstå fra oppgaveteksten
                    do putStr "Du vant! Gratulerer"
                       generiskSpill
           else 
              do newline
                 putStr "Spiller "
                 putStrLn (show spiller)
                 if spiller == 1 then -- Spiller sin tur
                    do putStrLn "r k / ? / q"
                       inn <- getLine 
                       if (null inn) then spillC brett spiller else case (head inn) of
                          'q' -> do generiskSpill
                          '?' -> do hjelpC
                                    input <- getLine
                                    spillC brett spiller
                          otherwise -> do let row = digitToInt (head inn)
                                          let col = read (tail inn)::Int
                                          if valid brett row col then
                                             spillC (movec brett row col) (next spiller)
                                          else 
                                             do newline
                                                putStrLn "Ugyldig trekk"
                                                spillC brett spiller
                 else -- Maskinen sin tur, bruker bare nim sin algoritme, så funker ikkje heilt
                    do
                       let tuppel = ai brett brett
                       let row = fst (head tuppel)
                       let col = snd (head tuppel)
                       putStr (show row)
                       putStr (show col)
                       if validc brett row col && col > 0 then
                          spillC (movec brett row col) (next spiller)
                       else
                          do newline
                             putStrLn "Ugyldig trekk av datamaskin"
                             spillC brett spiller




-- Kjører Nim for første gang
nim x = spillN (initial (read x :: Int)) 1

-- Kjører Chomp for første gang
chomp x = spillC (initialc (read x :: Int)) 1

-- Hjelpetekst
hjelpN = putStrLn "r a = fjern a brikker fra rad r. Vinner den som tar siste brikke.\nTrykk på hvilken som helst knapp for å fortsette..."
hjelpC = putStrLn ("r k = fjern ri =< r og ki >= k. Dette er i mangel på bedre ord ettersom " 
                   ++ "at eg har vert våken i snart 24 timer og jobbet med dette i et kjør som har så langt vart 8 timer og 44 min, klokka er nå 0744.\nTrykk på Hvilken som helst knapp for å fortsette...")