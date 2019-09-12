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

    fjernc str c = [x | x <- str, not (c `elem` str)]


{-A.(c) Programmer funksjon tegnpos som returnerer liste med alle 
posisjonene i inputstrenger der tegnet gitt i andre argumentet forekommer-}
    --tegnpos :: String -> Char -> [Int]


{-B.(a) Programmer funksjon tokenize
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

    --tokenize :: String -> String -> String -> [String]

{-C. I denne oppgaven betrakter vi lister som mengder, dvs. vi ser bort fra rekkefølgen og repetisjoner av elementer
    - [1,3,1,2,1,3] og [3,2,1] betraktes som like-}

    --eqli :: Eq t => [t] -> [t] -> Bool

{-D. programmer funksjon sjekk som tar som input en streng med mulige parantesuttrykk, og sjekker om paranteser er riktig.
    Er de det, returneres strengen "Korrekt", mens er det feil, returneres strengen "Feil".
    Du kan først lage en løsning hvor kun paranteser ( og ) kan forekomme og, deretter, utvide den med parantespar [] og {}.
    Paranteser må matches kun mot paranteser med samme type. f.eks skal strengen "({)}" avvises fordi innerste "{" har tilsvarende 
    ")" istedenfor "}". Utenom paranteser, kan man ha vilkårlige tegn i strenger, f.eks skal "abc( de {a} jjjj)[x]" aksepteres.-}

    --sjekk :: String -> String