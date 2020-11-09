import Data.Char
data Ast = V Int | P Ast Ast | M Ast Ast | Ord String deriving Show

--1
eval :: Ast -> Int
eval (Ord x) = error("Du må fjernet ordet: " ++ x ++ " i uttrykket")
eval (V x) = x
eval (P x y) = eval x + eval y
eval (M x y) = eval x * eval y


--2.
inn :: Ast -> String
inn (V x) = show x
inn (P x y) = "(" ++ inn x ++ " + " ++ inn y ++ ")"
inn (M x y) = "(" ++ inn x ++ " * " ++ inn y ++ ")"


--3.1
tokenize :: String -> [String]
tokenize str = tokenizer str "+*" " "

tokenizer :: [Char] -> [Char] -> String -> [String]
tokenizer [] t s = []
tokenizer (x:xs) t s | elem x t = [x] : tokenizer xs t s
                    | elem x s = tokenizer xs t s
                    | otherwise = (takeWhile (notin (t++s)) (x:xs)) : tokenizer (dropWhile (notin (t++s)) (x:xs)) t s

notin ls = \z -> not (elem z ls)



--3.2
parse :: String -> Ast
parse str = fst (parse' (tokenize str))

parse' :: [String] -> (Ast, [String])
parse' [] = error ("Tom for verdier å operer med")
parse' ("+":[_]) = error ("Tom for verdier å plusse")
parse' ("*":[_]) = error ("Tom for verdier å gange")
parse' ("+":xs) = let(e1, r1) = parse' xs ;
                        (e2, r2) = parse' r1 in (P e1 e2, r2)

parse' ("*":xs) = let(e1, r1) = parse' xs ;
                        (e2, r2) = parse' r1 in (M e1 e2, r2)

parse' (x:xs) = if (onlyDigits x) 
                    then (V (read x :: Int), xs)
                else if(stringCheck x)
                    then(Ord (read (show x) :: String), xs)
                else error ("Syntaksfeil ved " ++ x)


onlyDigits x = takeWhile isDigit x == x
stringCheck x = takeWhile isLetter x == x

--3.3
ev :: String -> Int 
ev str = eval (parse str)



--3.4 
innfiks :: String -> String
innfiks str = inn (parse str)
