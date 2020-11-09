-- A
--4.5 Without using any other library function or operators, show how the meaning of the following pattern matching definition for logical conjunction && can be formalised using conditional expressions
{-
True && True = True
_ && _ = False
-}

(&&) :: Bool -> Bool -> Bool
a && b = if a == True then 
            if b == True then True else False
         else False

--4.7
mult :: Int -> Int -> Int -> Int

mult = \x -> (\y -> (\z -> x * y * z))


-- B
--5.6 
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]


perfect x = sum(factors x) == 2 * x

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], perfect x]


--5.7

compre = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]


--5.9

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

--C Programmer funksjonen rem1 som fjerner første forekomsten av et element, gitt i andre argumentet, fra listen gitt som det første argumentet
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x:xs) y = if x == y then xs else x : rem1 xs y



rem2 :: Eq a => [a] -> a -> [a]
rem2 [] _ = []
rem2 (x:xs) a = if x == a then xs else x : (rem1 xs a)


--D Programmer funksjonen diff slik at diff as bs fjerner første forekomsten av første elementet i bs fra as, deretter første forekomsten av andre elementet i bs fra as osv
diff :: Eq a => [a] -> [a] -> [a]
diff [] _ = []
diff x [] = x
diff (x:xs) (y:ys) = diff (rem1 (x:xs) y) ys
