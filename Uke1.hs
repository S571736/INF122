-- Sondre Lindaas Gjesdal

-- Chapter 1.7

-- 1.1 Give another calculation for the result of double (double 2)
{-
The normal solution to double (double 2) is the following

double (double 2)
 - invoke the double
 
 double (2+2)

 - double 4

 4+4 = 8

 another way to do this is:

 double (double 2)

 double 2 + double 2

 (2+2) + (2+2)

 4+4 = 8
-}
double :: Integer -> Integer
double n = n + n

quadruple :: Integer ->  Integer
quadruple n = double (double n)

-- 1.2 Show that sum [x] = x for any x


sum2 [] = 0
sum2 (n:ns) = n + sum2 ns

{-
 sum [x]

 x + sum []

 x + 0 = x
-}

-- 1.3 Define a function "product" that produces the product of a list of numbers, and show using your definition that product [2,3,4] = 24

product2 :: Num p => [p] -> p

product2 [] = 1
product2 (n:ns) = n * product2 ns

{-
product [2,3,4]

2 * product [3, 4]

2 * 3 * product [4]

2 * 3 * 4 * product []

2 * 3 * 4 * 1 = 24
-}

-- 2.1 Work through the examples

factorial n = product [1..n]

average ns = sum ns `div` length ns

-- 2.2 Paranthesis the following

{-
2^3*4 -> (2^3)*4

2*3 + 4*5 -> (2*3)+(4*5)

2 + 3 * 4^5 -> 2+(3*(4^5))


-}

-- 2.3 The script below contains three syntactical erros, correct them
{-
N = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]
-}

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- TIL INNLEVERING
-- B 1.4

revqsort [] = []

revqsort (x:xs) = revqsort larger ++ [x] ++ revqsort smaller
                where
                    larger = [a | a <- xs, a > x]
                    smaller = [b | b <- xs, b <= x]

qsort [] = []

qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where
                    larger = [a | a <- xs, a > x]
                    smaller = [b | b <- xs, b <= x]

-- 1.5

{-
The difference would be that the algorithm would dismiss the other values equaling eachother 
e.g. the second 1 and 2 would not be added to the smaller or larger list, and thus be removed entirely
the result would be [1,2,3] instead of [1,1,2,2,3]
-}

-- 2.4

lastv2 x = head (reverse x)

-- 2.5

init2 x = reverse (drop 1 rx)
        where
            rx = reverse x


-- C Programmer følgende funksjoner

{-1.
plu: [Int] -> Int -> [Int]
 Input: en liste k med heltall og et heltall n
Output: listen der hvert element e fra listen k er erstattet med e+n (elementene står i
samme rekkefølge som i k).
F.eks.:
plu [1,2,3] 5 = [5,7,8]
plu [1,2,3] 0 = [1,2,3]
-}
plu :: [Int] -> Int -> [Int]
plu k n = [e + n | e <- k]


{-2.
 pali: [a] -> Bool
som gir True hvis inputlisten er en palindrome og False ellers.
(== er boolsk likhet, dvs. s == t gir True hvis s og t er like, og False ellers.)
F.eks.:
pali “abba” = True
pali “abbac” = False
pali [1,2,3,3,2,1] = True
-}

--pali :: [a] -> Bool
pali x = x == reverse x