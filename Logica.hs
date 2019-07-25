module Logica where

  --  Función que simula una comperta XOR, será utilizada en el cifrado para obtener el nuevo mensaje.
  xor :: Int -> Int -> Int
  xor a b = disyuncion (conjuncion a (negacion b)) (conjuncion (negacion a) b )

  -- Función que simula una compueta AND, usada para construir la función XOR.
  conjuncion :: Int -> Int -> Int
  conjuncion a 1 = a
  conjuncion b 0 = 0

  -- Función negacion que simula una negación, para construir la función XOR.
  negacion :: Int -> Int
  negacion 0 = 1
  negacion 1 = 0

  -- Función disyunción que simula una compuerta OR, para construir la función XOR. 
  disyuncion :: Int -> Int -> Int
  disyuncion a 1 = 1
  disyuncion b 0 = b
