--A
--8.3
data Tree a = Leaf a | Node(Tree a) (Tree a)

leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node l r) = (leaves l + leaves r)

balanced :: Tree a -> Bool
balanced (Leaf y) = True
balanced (Node l r) = abs(leaves l - leaves r) <= 1 && balanced l && balanced r

t :: Tree Int
t = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4))

--8.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a

folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)
--8.6
eval :: Expr -> Int
eval e = folde id (+) e

size :: Expr -> Int
size e = folde (\_ -> 1) (+) e

--A
data Exp = S | U | Og Exp Exp | El Exp Exp | Ik Exp

--a
folde' :: a-> a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> Exp -> a
folde' s u o e i S = s
folde' s u o e i U = u
folde' s u o e i (Og x y) = o (folde' s u o e i x) (folde' s u o e i y)
folde' s u o e i (El x y) = e (folde' s u o e i x) (folde' s u o e i y)
folde' s u o e i (Ik x) = i (folde' s u o e i x)

--b
evb :: Exp -> Bool
evb exp = folde' True False (&&) (||) (not) exp

--c 
succMax x y = succ(max x y)
evh :: Exp -> Int
evh exp = folde' 1 1 succMax succMax succ exp

--d
evi :: Exp -> Int
evi exp = folde' 1 5 (+) (*) negate exp