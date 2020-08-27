-- 03.09.2019

-- Nytt fra gammelt
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

-- betingede likninger
og x y = if x==False then False
    else if y==False then False
        else True

-- voktede likninger
og x y | x==False = False
        | y==False = False
        | otherwise = True

-- Anonyme funksjoner

-- \ x -> \ y -> if x==False then False else y

-- Mønstre
og False _ = False
og _ False = False
og _ _ = True

og True True = True
og _ _ = False

og False _ = False
og True x = x

ptrue = [True, False]

-- Funksjon lignende til Uke 3 D

aab st = okk st []
okk [] as = True
okk ('a':xs) as == okk xs ('a':as)
okk ('b':xs) [] = False
okk ('b':xs) (a:as) = okk xs as
okk (x:xs) as = okk xs as

-- Muligens heilt lik oppgava, om det er forskjell 
ab st = ok st []
ok [] [] = True
ok [] as = False
ok ('a':xs) as = ok xs ('a':as)
ok ('b':xs) [] = False
ok ('b':xs) (a:as) = ok xs as
ok (x:xs) as = ok xs as