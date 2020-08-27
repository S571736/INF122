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
curry f a b = f(a,b)

uncurry f(a,b)= f a b 

--7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x: xs) = f x : altMap g f xs

--7.6
unfold p h t x 
    | p x = []
    | otherwise = h x : unfold p h t (t x)


chop8' = unfold null (take 8) (drop 8)

map' f = unfold null (f . head) tail

iterate' f = unfold (const False) id f  