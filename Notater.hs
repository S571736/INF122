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