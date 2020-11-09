
printK [] = return ()
printK (x:xs) = do if (read x::Integer) < 9 
                        then putStr (x ++ "  ") 
                        else putStr (x ++ " ")
                   printK xs

lagLinje rad nR = do goto ((if rad > 9 then (lft - 2) else lft - 1, 1+rad))
                     putStr (show rad)
                     putStr " "
                     putStrLn $ concat $ replicate nR ".  "
                     


lagBrett [] _ = return ()
lagBrett (x:xs) nR = do lagLinje x nR
                        lagBrett xs nR


main :: IO ()
main = do
    putStrLn ("Hvor stort skal brettet være? 0<N<100")
    inn <- readLn
    let tall = inn :: Int
    if (tall > 99) || (tall < 1) 
        then do
            putStrLn "Vennligst skriv et gyldig tall"
            main
        else do
        brett tall
        kommando tall
        return ()
    
kommando tall = do
    putStrLn ("Vennligst gjør en kommando n x y/ d x y/ q")
    ord <- getLine
    let input = words ord
    if (null input) then return () else case (head input) of
        "n" -> do n (read (input !! 1)) (read $ last input) tall
        "d" -> do d (read (input !! 1)) (read $ last input) tall
        otherwise -> do {putStrLn "Ukjent kommando"}
    goto (0,tall + 2)
    putStr "\ESC[0J"
    if (head input /= "q") 
        then (kommando tall) 
        else do clr
                return()

brett :: Int -> IO ()
brett nR = do
    clr
    putStr "   "
    goto (lft+1, 0)
    printK (map show [1..nR])
    lagBrett [1..nR] nR
    

n x y tall = if (x > tall) || (y > tall) || (x < 1) || (y < 1) 
    then do 
        return()
    else do 
        goto (((x*3)),(y+1))
        putStrLn " X"
           

d x y tall = if (x > tall) || (y > tall) || (x < 1) || (y < 1) 
    then do
        return()
    else do
        goto ((x*3),(y+1))
        putStrLn " ."

clr :: IO ()
clr = putStr "\ESC[2J"

lft :: Int
lft = 3

goto (x,y) = putStr ("\ESC[" ++ show y ++";"++show x ++ "H")
