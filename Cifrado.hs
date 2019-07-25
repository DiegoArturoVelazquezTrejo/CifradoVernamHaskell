module Cifrado where
  import Cadenas
  import System.IO

  fun = do
        putStr "Dame un mensaje\n"
        nombre <- getLine
        putStr "Dame una llave\n"
        llave <- getLine
        texto <- readFile nombre
        putStr "Salida\n"
        salida <- getLine
        writeFile salida (codifica texto llave)
        putStr "Archivo Bien\n"
        putStr "Salida"
