module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha longitudDeseada s = [ ' ' | _ <- [1 .. cantEspacios] ] ++ s -- [1 .. cantEspacios] == [] cuando cantEspacios < 1
                                  where cantEspacios = longitudDeseada - length s

-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f = zipWith aplicaEnIndice [0 ..]
  where
    aplicaEnIndice p1 p2 = if n == p1 then f p2 else p2

-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)

-- | @porcentaje x y@ calcula el porcentaje que representa @x@ sobre @y@.
-- Si @y@ es 0, devuelve 0.
porcentaje :: Int -> Int -> Float
porcentaje _ 0 = 0
porcentaje x y = (fromIntegral x / fromIntegral y) * 100

-- | @calcularPorcentajes xs@ calcula los porcentajes que representa cada elemento de la lista @xs@ sobre la suma de todos los elementos.
-- Si la suma es 0, todos los porcentajes son 0.
calcularPorcentajes :: [Int] -> [Float]
calcularPorcentajes xs = map (\x -> porcentaje x (sum xs)) xs