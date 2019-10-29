data Tree = Leaf Int | Node Tree Tree

blad (Leaf _) = 1
blad (Node t1 t2) = blad t1 + t2

inode (Leaf _) = 0
inode (Node t1 t2) = inode t1 + inode t2 + 1

-- (blad t) = 1 + (inode t) for enhver t :: Tree

--noe t = Leaf _ . blad (Leaf _) = 1 = 1+0 = 1+inode (Leaf_)

