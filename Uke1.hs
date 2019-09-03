-- B
plu k n = [ e+n | e <- k ]


-- B2

pali x = x == reverse x

-- C
llrev xs = [reverse x | x <- xs]

-- Others

-- 1.1

double x = 2*x

dd x = double (double x)

ddalt x = double x + double x

-- 1.2 show that sum [x] = x for any number x

-- what how

-- 1.3

product' []  = 1
product' (x:xs) = x*(product' xs)

-- 1.4

-- større = [ i | i <- xs, i >= x ]
-- mindre = [ i | i <- xs, i < x ]

qsrev [] = []
qsrev (x:xs) = (qsrev større) ++ [x] ++ (qsrev mindre)
    where 
        større = [ i | i <- xs, i >= x ]
        mindre = [ i | i <- xs, i < x ]

-- 2.2 Parenthesise following numeric expression

-- (2^4)*4
-- (2*3) + (4*5)
-- 2 + (3*(4^5))

-- 2.3
n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]