-- 7.1 
lc f p xs= [f x | x <- xs, p x]
highOrder f p xs = map f (filter p xs)

-- 7.2
--a 
all' :: (a -> Bool) -> [a] -> Bool
all' p = and . map p

--b
any' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p

--c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
    | p x = x : takeWhile' p xs
    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs) 
    | p x = dropWhile' p xs
    | otherwise = (x:xs)

--7.4
dec2int :: [Int] -> Int

dec2int = foldl leggTil 0
    where leggTil num d = 10*num + d

--7.5
