data Tree = Leaf Int | Node Tree Tree

blad (Leaf _) = 1
blad (Node t1 t2) = (blad t1) + (blad t2)

inode (Leaf _) = 0
inode (Node t1 t2) = inode t1 + inode t2 + 1

-- (blad t) = 1 + (inode t) for enhver t :: Tree

--noe t = Leaf _ . blad (Leaf _) = 1 = 1+0 = 1+inode (Leaf_)

--C1
bsum 1 = 1/2
bsum x = (bsum(x-1)) + 1/(x*(x+1))

{--C2
basis - bsum 1 = 1/2 = 1/(1*(1+1))
induksjunshypotese:
bsum x = 1/(1+2) + 1/(2+3) + ... + 1/x*(x+1)
bsum (x+1) = bsum (x-1) + 1/(x+1*(x+2))
= IH = (1/(1*2) + 1/(2*3) + ... + 1/x*(x+1)) + 1/((x+1) + (x+2))


C3
Bruk nå induksjon for å bevise at for alle naturlige tall n: >=1!!

x/(x+1) + 1/((x+1) (x+2)) = (x + (x+2) + 1)/ | ikkje ferdig

-}

C4
bsum x = x/(x+1)

--D
data Tree a = Leaf a | Node (Tree a) a (Tree a)

fmap :: (a -> b) -> Tree a -> Tree b
fmap f (Leaf x) = Leaf (f x)
fmap (Node ve x ho) = Node (fmap f ve) (f x) (fmap f ho)

--bevis ved induksjon
fmap id = id

{-
fmap id v = id v
fmap id h = id h

fmap id (Node v c h) = Node (fmap id v) (id x) (fmap id h)
                     = Node (fmap id v) x (fmap id h)
                     = Node (id v) x (id h)
                     = Node v x h
                     = id (Node v x h)
-}

for alle t: Tree a: fmap (f . g) = (fmap f) . (fmap g)
(.) (f . g) x = f(g x)
Basis: t = Leaf x

fmap (f . g) (Leaf x) =f1= Leaf ((f . g)x)
=(.)= Leaf (f (g x))
=(f1)= fmap f (Leaf (g x))
=(f1)= fmap (fmap g(Node x))
=(.)= ((fmap f) . (fmap g)) (Leaf x)

IH:
    fmap (f.g) v = ((fmap f).(fmap g)) v
    fmap (f.g) h = ((fmap f).(fmap g)) h


fmap (f . g) (Node v x h)
=f2= Node (fmap (f . g)v) (f.g x) (fmap (f.g) h)
=(.)= Node (fmap (f.g)v) (f(g x)) (fmap (f.g)h)
=IH= Node ((fmap f).(fmap g)v) (f(g x)) (f(g x)) ((fmap f).(fmap g) h)
=(.)= Node ((fmap f) ((fmap g)v)) (f(g x)) (f(g x)) ((fmap f) ((fmap g) h))
=(f2)= fmap f (Node (fmap g v)(g x) (fmap g h))
=(f2)= fmap f (fmap g(Node v h x))
=      (fmap f) ( (fmap g) (Node v h x))
=(.)= ((fmap f) . (fmap g)) (Node v x h)



--Spesielt interesserte E
m1) mengde [] = True
m2) mengde (x:xs) = if (elem x xs) then False else mengde xs

r1) rep [] = []
r2) rep (x:xs) = if (elem x xs) then rep xs else x:rep xs

mengde (rep ls) = True

Basis: ls = []
m (r []) =r1= m [] =m1= True

Ind:
IH: m (r xs) = True

m (r (x:xs)) = ... True
=(r2)= m (if (elem x xs) then rep xs else x:rep xs)

=if= if (elem x xs) then m(rep xs) else (x:rep xs)

1) når elem x xs = True --naar?
da: m (r xs) = True
IH!

2) når (elem x xs) = False --naar?
da: m(x: rep xs) = True
m(x: rep xs) =m2= if (elem x (rep xs)) then False else m (rep xs)


