
--A
fjern :: String -> Char -> String
fjern [] _ = []
fjern (x:xs) y = if x == y then fjern xs y else x : fjern xs y


fjernComp :: String -> Char -> String
fjernComp [] _ = []
fjernComp xs y = [x | x <- xs, x /= y]

--B

pos :: (Eq t, Num a) => a -> t  -> [t] -> [a]
pos _ _ [] = []
pos n y (x:xs) = if (x == y) then n : pos (n+1) y xs
                             else pos (n+1) y xs

tegnPos :: Char -> String -> [Int]
tegnPos _ [] = []
tegnPos y xs = pos 0 y xs


tegnposComp :: Char -> String -> [Int]
tegnposComp _ [] = []
tegnposComp c str = [y | (x, y) <- zip str [0..], x == c]


--C

intToList :: Int -> [Int]
intToList 0 = []
intToList i = intToList(i `div` 10) ++ [i `mod` 10]


--D(a)

settSammen :: [String] -> String
settSammen [] = ""
settSammen (x:xs) = x ++ space xs
    where 
        space [] = ""
        space (l:ls) = ' ' : (l ++ space ls)

--D(b)

delStrengen :: String -> [String]
delStrengen "" = []
delStrengen str = 
    let s = takeWhile (/=' ') $ dropWhile (==' ') str
        (_, rest) = splitAt (length s) str
        in s : delStrengen (dropWhile (==' ')rest)

delStrengen2 :: String -> [String]
delStrengen2 xs = (delStrengen2' (dropWhile (== ' ') xs))
    where 
        delStrengen2' :: String -> [String]
        delStrengen2' [] = []
        delStrengen2' xs = 
            let 
                (prefix, rest) = break (== ' ') xs
                rest'          = dropWhile (== ' ') rest
            in prefix : delStrengen2' rest'


gdelStrengen :: String -> String -> [String]
gdelStrengen [] spl = []
gdelStrengen (s:str) spl
    | elem s spl = rest -- hvis i st1, skip
    | str == [] = [[s]] -- hvis neste element er tom liste, returner s som liste
    | elem (head str) spl = [s] : rest -- hvis neste element er i spl, terminer string
    | otherwise = (s : head rest) : tail rest -- otherwise legg s til i strengen
    where
        rest = gdelStrengen str spl -- rekursivt kall på resten av lista

gdelStrengen2 :: String -> String -> [String]
gdelStrengen2 xs ys = (gdelStrengen2' (dropWhile (`elem` ys) xs) ys)
    where 
        gdelStrengen2' :: String -> String -> [String]
        gdelStrengen2' [] _ = []
        gdelStrengen2' xs ys = 
            let 
                (prefix, rest) = break (`elem` ys) xs
                rest'          = dropWhile (`elem` ys) rest
            in prefix : gdelStrengen2' rest' ys