module Util where

-- | @alinearDerecha longitudDeseada s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @longitudDeseada@.
-- Si @s@ ya tiene longitud @>= longitudDeseada@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha longitudDeseada s = replicate cantEspacios ' '  ++ s 
                                  where 
                                    cantEspacios = longitudDeseada - length s

-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem indice f = zipWith aplicarFuncionEnIndice [0 ..]
  where
    aplicarFuncionEnIndice indiceElemento elemento = if indiceElemento == indice then f elemento else elemento

-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)