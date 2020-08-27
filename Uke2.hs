--B 3.3 What are the types of the following functions
second xs = head (tail xs) --second :: [a] -> a

swap (x,y) = (y,x) --swap :: (b,a) -> (a,b)

pair x y = (x,y) -- pair :: a -> b -> (a, b)

double x = x*2 -- double :: Num a => a -> a

palindrome xs = reverse xs == xs -- palindrome :: Eq a => [a] -> Bool

twice f x = f (f x) --twice :: (t -> t) -> t -> t

-- C Angi typer til følgende uttrykk evt, si at uttrykket ikke har en type. Gi osgå nødvendige klasserestriksjoner der de trenges

{-
False :: Bool

5+8 :: Num a => a

(+) 2 :: Num a => a -> a

(+2) :: Num a => a -> a

(2+) :: Num a => a -> a

(["foo", "bar"], 'a') :: ([[Char]], Char)

[(True,[]), (False, [['a']])] :: [(Bool, [[Char]])]

\x y -> y !! x :: Int -> [a] -> a

-- [take, drop, \x y -> (y !! x)] :: This is not possible as take and drop does not have the same type as the anonymous function

[take, drop, \x y -> [y !! x]] :: [Int -> [a] -> [a]]
-}

-- D Angi typen til hver foo og si hvilke av disse funksjonene er ekvivalente

foo1:: a -> b -> (a,b)
foo1 x y = (x,y)

foo2 :: a -> b -> (a,b)
foo2 x = \y -> (x,y)

foo3 :: a -> b -> (a,b)
foo3 = \x y -> (x,y)

foo4 :: a -> b -> (a,b)
foo4 = \x -> \y -> (x,y)

foo5 :: b -> a -> (a,b)
foo5 = \x -> \y -> (y,x)

foo6 :: a -> b -> (a,b)
foo6 = \y -> \x -> (y,x)

-- E Definer vilkårlige funksjoner med angitte typer

f1 :: a -> (a, a)
f1 x = (x, x)

f2 :: (a, b) -> a
f2 (x,y) = fst (x,y)

f3 :: (a,b) -> b
f3 (x,y) = snd (x,y)

f4 :: a -> b -> a
f4 x y = x

f5 :: a -> b -> b
f5 x y = y

-- F programmer to funksjoener

--f :: Int -> Int -> Int

--g :: (Int, Int) -> Int