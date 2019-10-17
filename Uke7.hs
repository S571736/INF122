import Data.Char
-- A Oppgave 10.1 fra boka


-- B
data FileOrFolder = File | Folder [FileOrFolder]

prettyPrint :: FileOrFolder -> IO ()
prettyPrint structure = putStr (pp structure)

pp :: FileOrFolder -> String
pp structure = draw structure ""

indent = "  "
draw :: FileOrFolder -> String -> String

draw (Folder (x:xs)) ind = ind ++ "-Folder\n" ++ draw x indent ++ "\n" ++ ind ++ draw (head xs) indent
draw File ind = ind ++ "-File" 
draw _ _ = []


eksempel = (Folder 
                [Folder
                     [File,
                     Folder 
                        [File, 
                        File, 
                        File]],
                    File, 
                Folder [
                    File],
                File ] )