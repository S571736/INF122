import Data.Char
import Data.List
{-
ai board [] target = []
ai board (x:xs) target = 
    do
       if target < x then do          
          return (makeList x (x - target))
       else
          ai board xs target
-}
formatter a = show a

rInt :: String -> Int
rInt = read


makeList x y = x ++ y