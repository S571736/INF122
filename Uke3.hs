-- Uke 3
-- A
-- 4.3
safetail :: Eq a => [a] -> [a]
safetail xs = 
if xs == [] 
    then [] 
    else tail xs

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
tell str = filter (flip elem "ab") str
tella str = length $ filter (=='a') str
tellb str = length $ filter (=='b') str


ab (x:xs) = do
    let a = 0 + tella [x]
    let b = 0 + tellb [x]
    if a >= b
        then ab xs
        else False