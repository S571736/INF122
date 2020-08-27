-- Uke 3
-- A
--print(4.3)
--a
safetail :: [a] -> [a]
safetail xs = if null xs then xs else tail xs

--b
safetail' :: [a] -> [a]
safetail' xs | null xs = []
            | otherwise = tail xs

--c

safetail'' :: [a] -> [a]
safetail'' [] = []
safetail'' xs = tail xs

--4.4
(||) :: Bool -> Bool -> Bool
True || True = True
True || False = True
False || True = True
False || False = False

(|||) :: Bool -> Bool -> Bool
False ||| False = False
_ ||| _ = True

(||||) :: Bool -> Bool -> Bool
False |||| b = b
True |||| _ = True

(|||||) :: Bool -> Bool -> Bool
b ||||| c | b == c = b
            | otherwise = True

--4.5
(&&) :: Bool -> Bool -> Bool
a && b = if a == True then
            if b == True then True else False
        else False

--4.6
(&&&) :: Bool -> Bool -> Bool
a &&& b = if a == True then b else False

--4.7
mult :: Int -> Int -> Int -> Int
mult  = \x -> (\y -> (\z -> x * y * z))
-- B
f :: Int -> Int
f 0 = 0
f x = x^2 + f (x-1)


f1 x = sum[x^2 | x <- [1..x]]

f2 x = (\x -> x^2 + f (x-1)) x

-- C
toList :: Int -> [Int]
toList 0 = []
toList x = toList(x `div` 10) ++ [x `mod` 10]

--D
ab :: String -> Bool

ab st = ok st []
ok [] [] = True
ok [] as = False
ok ('a':xs) as = ok xs ('a':as)
ok ('b':xs) [] = False
ok ('b':xs) (a:as) = ok xs as
ok (x:xs) as = ok xs as

--E
--5.2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x, y) | x <- [0..m], y <- [0..n]]
--5.3
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]
--5.4
replicate' :: Int -> a -> [a]
replicate' x y = [ y | _ <- [1..x]]
