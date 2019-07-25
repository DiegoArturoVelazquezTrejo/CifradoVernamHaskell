module Cadenas where
  import Logica
  import Data.Char

  -- función que devuelve una lista de ceros y unos en representacion al numero binario que se le pasó como argumento.
  dec2Bin = reverse . q
    where
          q 0 = [0]
          q 1 = [1]
          q n = r :q d where(d,r) = divMod n 2

  --normaliza a un mismo tamaño a todas las listas de numeros binarios
  normalizar :: [Int] -> Int -> [Int]
  normalizar (x:xs) a = if((a - length(x:xs)) > 0) then normalizar ([0] ++ (x:xs)) (a)
                        else (x:xs)

  -- función que pasa una palabra a una lista con el número entero de cada caracter miembro de la cadena
  cadena :: String -> [[Int]]
  cadena [] = []
  cadena (x:xs) = [normalizar (dec2Bin(fromEnum x)) 10] ++ cadena(xs)

  -- Genera la llave con la longitud del mensaje
  generarLlaveAuxiliar :: String -> String -> String -> String -> String
  generarLlaveAuxiliar cadena mensaje "" llave = generarLlaveAuxiliar cadena mensaje llave llave
  generarLlaveAuxiliar cadena mensaje (x:xs) llave = if(length cadena == length mensaje) then cadena
                                              else generarLlaveAuxiliar (cadena ++ [x]) mensaje xs llave

  -- Genera la llave con la longitud igual a la del mensaje para ejecutar la función XOR.
  generarLlave :: String -> String -> String
  generarLlave a b = generarLlaveAuxiliar a b a a

  -- Función de XOR aplicada en una sola matriz
  matXORmat :: [Int] -> [Int] -> [Int]
  matXORmat [] [] = []
  matXORmat (x:xs) (y:ys) = [xor x y]++(matXORmat xs ys)

  mensajeXORllave :: [[Int]] -> [[Int]] -> [[Int]]
  mensajeXORllave [] [] = []
  mensajeXORllave (x:xs) (y:ys) = [matXORmat x y]++(mensajeXORllave xs ys)

  bin2nat :: [Int] -> Int
  bin2nat [] = 0
  bin2nat (x:xs) = x * (exponente 2 (length xs)) + bin2nat xs

  exponente :: Int -> Int -> Int
  exponente a 0 = 1
  exponente a 1 = a
  exponente a n = a * exponente a (n-1)

  binarioAChar :: [[Int]] -> [Char]
  binarioAChar [] = []
  binarioAChar (x:xs) = [chr (bin2nat x)]++(binarioAChar xs)

  codifica :: String -> String -> String
  codifica mensaje llave = binarioAChar (mensajeXORllave(cadena mensaje) (cadena(generarLlave llave mensaje)))
