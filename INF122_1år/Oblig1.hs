module Oblig1 where 
    -- Sondre Lindaas Gjesdal

{- A (a) Programmer funksjon fjern som fjerner fra strengen
    i første argumentet tegnet i posisjonen gitt ved andre argumentet-}
    
    fjern :: String -> Int -> String

    fjern str n = 
        let s = take (n) str
            e = drop (n+1) str
            in s ++ e

{- A. (b) Programmer funksjon fjernc som fjerner fra strengen i første 
argumentet alle forekomster av tegnet i andre argumentet-}
    fjernc :: String -> Char -> String

    fjernc str c = filter(/=c) str


{-A.(c) Programmer funksjon tegnpos som returnerer liste med alle 
posisjonene i inputstrenger der tegnet gitt i andre argumentet forekommer-}
    tegnpos :: String -> Char -> [Int]

    tegnpos str c = [y | (x, y) <- zip str [0..], x == c]


{-B.(a) Programmer funksjonen ord som deler inputstrengen opp i liste av strenger ved hver sekvens av mellomrom-}
    ord :: String -> [String]
    ord "" = []
    ord str = 
        let s  = takeWhile (/=' ') $ dropWhile (==' ') str
            (_, rest) = splitAt (length s) str
        in s : ord (dropWhile (==' ') rest)

{-B.(b) Programmer funksjon tokenize
>tokenize str imp rem skal dele opp strengen str i en liste ac strenger der
    - hvert tegn som forekommer i strengen imp skal stå som egen streng,
    - hvert tegn som forekommer i strengen rem skal fjernes, mens
    - tegn fra imp ++ rem brukes som skilletegn, dvs andre delstrenger av str 
    i resultatlisten er de som forekommer mellom tegn fra imp ++ rem
    Vi antar at ingen tegn forekommer i begge argumentlister imp og rem
    > tokenize “aa b -c”   “”   “ ”  (den siste er strengen med enkelt blank, og nest siste en tom streng) skal gi
        [“aa”, ”b”, ”-c”]
    > tokenize “aa b -c”   “-”   “ ”   (den siste er strengen med enkelt blank) skal gi 
        [“aa”, ”b”, ”-“, “c”]
    > tokenize “a + b* 12–def”   “+*–”   “ “ (den siste er strengen med enkelt blank) skal gi 
        [“a”, ”+”, ”b”, ”*”, ”12”, “–“, “def”]
    > tokenize “a + b* 12–def”   “+*”   “ “ (den siste er strengen med enkelt blank) skal gi 
        [“a”, ”+”, ”b”, ”*”, ”12–def”]-}


    tokenize :: [Char] -> [Char] -> String ->  [String]
   
    tokenize _ _ "" = []
    tokenize imp rem (x:str)
        | x `elem` rem                  = rest
        | x `elem` imp                  = [x]:rest
        | ([i:_]) <- rest, i `elem` imp = [x]:rest
        | (voksende:rest') <- rest      = (x:voksende):rest'
        | otherwise                     = [x]:rest
        where rest = tokenize imp rem str




{-C. I denne oppgaven betrakter vi lister som mengder, dvs. vi ser bort fra rekkefølgen og repetisjoner av elementer
    - [1,3,1,2,1,3] og [3,2,1] betraktes som like-}

    rmdup :: Eq a => [a] -> [a]
    rmdup = rdHelp []
        where rdHelp sett [] = sett
              rdHelp sett (x:xs)
                | x `elem` sett = rdHelp sett xs
                |otherwise = rdHelp (sett ++ [x]) xs

{-    
    qs [] = []
    qs (x:xs) = (qs mindre) ++ [x] ++ (qs storre)
        where 
            storre = [ i | i <- xs, i >= x ]
            mindre = [ i | i <- xs, i < x ]
            

    --eqli :: Eq t => [t] -> [t] -> Bool
    eqli l1 l2= do
        let li1 = rmdup l1
        let li2 = rmdup l2
        let lis1  = qs li1
        let lis2  = qs li2
        lis1 == lis2
-}

    eqli :: Eq t => [t] -> [t] -> Bool
    eqli [] [] = True   
    eqli a b = eqli2 (rmdup a) (rmdup b)
    
    eqli2 a b = if length a /= length b then False else checker a b

    checker a b = check [(x,y) | (x,y) <- zip a b, x <- a, y <- b, x == y]

    check a = check2 [x == y|(x,y) <- a]

    check2 a = if all (== True) a then True else False
{-
    remDoubles [] = []
    remDoubles (x:xs) | x elem xs = remDoubles xs
                      | otherwise = x : remDoubles xs
    -}
{-D. programmer funksjon sjekk som tar som input en streng med mulige parantesuttrykk, og sjekker om paranteser er riktig.
    Er de det, returneres strengen "Korrekt", mens er det feil, returneres strengen "Feil".
    Du kan først lage en løsning hvor kun paranteser ( og ) kan forekomme og, deretter, utvide den med parantespar [] og {}.
    Paranteser må matches kun mot paranteser med samme type. f.eks skal strengen "({)}" avvises fordi innerste "{" har tilsvarende 
    ")" istedenfor "}". Utenom paranteser, kan man ha vilkårlige tegn i strenger, f.eks skal "abc( de {a} jjjj)[x]" aksepteres.-}

    sjekk :: String -> String
    sjekk xs = if testing xs then "Korrekt" else "False"
    
    
    testing xs = head cumulated == 0 && all (>= 0) cumulated where
        cumulated = scanr (+) 0 $ map depth xs
        depth '(' = -1
        depth ')' = 1
        depth _ = 0