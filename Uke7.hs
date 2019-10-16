import Data.Char
-- A Oppgave 10.1 fra boka


-- B
data FileOrFolder = File | Folder [FileOrFolder]

prettyPrint :: FileOrFolder -> IO ()
prettyPrint structure = putStr (pp structure)

pp :: FileOrFolder -> String
pp structure = draw structure ""

ind = "  "
draw :: FileOrFolder -> String -> String

draw (Folder (x:xs)) indent = indent ++ "-Folder\n" ++ indent ++ draw x ind ++ "\n" ++ indent ++ indent ++ draw (head xs) ind
draw File indent = "-File" 
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