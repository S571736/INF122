-- 7.1
listcomp f p xs = [f x | x <- xs, p x]

highOrder f p xs = map f (filter p xs)



-- 7.4
dec2int :: [Int] -> Int
dec2int = foldl leggTil 0
    where leggTil num d = 10*num + d

-- 7.5
curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x y -> f(x,y)

uncurry :: (a -> b -> c) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y

-- 7.9

altmap :: (a -> b) -> (a -> b) -> [a] -> [b]
altmap _ _ [] = []
altmap f g (x:xs) = f x : altmap g f xs

--8.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a-> a) -> Expr -> a
folde f g (Val x) = f x
folde f g  (Add x y) = g (folde f g x) (folde f g y)

--8.6

eval :: Expr -> Int
eval e = folde id (+) e

size :: Expr -> Int
size e = folde (\_ -> 1) (+) e

--B 

infiks :: Expr -> String
infiks (Val x) = show x
infiks (Add x y) = infiks' x ++ " + " ++ infiks' y

infiks' :: Expr -> String
infiks' (Val x) = show x
infiks' (Add x y) = "(" ++ infiks' x ++ " + " ++ infiks' y ++ ")"


prefiks :: Expr -> String
prefiks (Val x) = " " ++ show x ++ " "
prefiks (Add x y) = " + " ++ prefiks x ++ prefiks y


postfiks :: Expr -> String
postfiks (Val x) = show x ++ " "
postfiks (Add x y) = postfiks x ++ postfiks y ++ " + "
{-
B. I tillegg til eval og size fra 8.6, skriv 3 følgende funksjoner for datatypen
Expr fra 8.6:
    infiks :: Expr -> String
    prefiks :: Expr -> String
    postfiks :: Expr -> String
som skriver Expr i infiks notasjon med riktige paranteser, og i prefiks of
postfiks notasjon (uten noen paranteser). F.eks.,

infiks (Add (Add (Val 1) (Add (Val 2) (Val 3))) (Val 4) =
"(1 + (2 + 3)) + 4" eventuelt "((1 + (2 + 3)) + 4)"

prefiks (Add (Add (Val 1) (Add (Val 2) (Val 3))) (Val 4) =
"+ + 1 + 2 3 4"

postfiks (Add (Add (Val 1) (Add (Val 2) (Val 3))) (Val 4) =
"1 2 3 + + 4 +"
-}