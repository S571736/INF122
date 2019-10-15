-- A Oppgave 10.1 fra boka
import Data.Char

-- B
data FileOrFolder = File | Folder [FileOrFolder]

prettyPrint :: FileOrFolder -> IO ()
prettyPrint structure = putStr (pp structure)

pp :: FileOrFolder -> String
pp structure = draw structure ""

ind = "  "
draw :: FileOrFolder -> String -> String

draw (Folder (x:xs)) indent = indent ++ "-Folder\n" ++ draw x ind ++ draw (head xs) ind
draw File indent = indent ++ "-File\n" ++ "\n"
draw _ _ = []
