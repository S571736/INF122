-- Uke 3
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

ab :: String -> Bool

ab [] = []
ab x:xs = 