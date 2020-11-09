
alaa :: [Bool] -> Bool
alaa [] = True
alaa (x:xs) = if x == False then False else alaa xs

alb :: [Bool] -> Bool
alb [] = True
alb xs = all (==True) xs

alc :: [Bool] -> Bool
alc [] = True
alc x = foldl (==) True x

ald :: [Bool] -> Bool
ald [] = True
ald x = foldr (==) True x

ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala _ o [] = o
ala f o (x:xs) = ala f (f o x) xs
 

trekant :: Int -> IO ()
trekant k = mapM_ putStrLn (map (lagLinje k) [1..k])

lagLinje :: Int -> Int -> [Char]
lagLinje k n = mellomRom k n ++ stjerne n

mellomRom :: Int -> Int -> [Char]
mellomRom k n = take (k-n) (repeat ' ')

stjerne :: Int -> String
stjerne n = unwords (take n (repeat "*"))
