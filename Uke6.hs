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

folde f g e = case e of
    Val x -> f x
    Add x y -> g (folde f g x) (folde f g y)


--8.6
eval :: Expr -> Int
eval e = folde (\x -> x) (\x y -> x+y) e

size :: Expr -> Int
size e = 