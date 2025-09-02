module Util where

-- | @alinearDerecha n s@ agrega espacios a la izquierda de @s@ hasta que su longitud sea @n@.
-- Si @s@ ya tiene longitud @>= n@, devuelve @s@.
alinearDerecha :: Int -> String -> String
alinearDerecha indice s = 
    let cantEspacios = indice - length s in 
        if cantEspacios <= 0
            then s 
            else [' ' | c <- [1..cantEspacios]] ++ s

git commit -m "feat: Agregamos la implementacion de alinearDerecha y actualizarElem
>
> Co-authored-by: yarthax23 <jdfigari@gmail.com>
> Co-authored-by: Franco3333 <franco.straface1@gmail.com>"

-- | Dado un índice y una función, actualiza el elemento en la posición del índice
-- aplicando la función al valor actual. Si el índice está fuera de los límites
-- de la lista, devuelve la lista sin cambios.
-- El primer elemento de la lista es el índice 0.
actualizarElem :: Int -> (a -> a) -> [a] -> [a]
actualizarElem n f = zipWith aplicaEnIndice [0..]
    where
        aplicaEnIndice p1 p2 = if n == p1 then f p2 else p2

-- | infinito positivo (Haskell no tiene literal para +infinito)
infinitoPositivo :: Float
infinitoPositivo = 1 / 0

-- | infinito negativo (Haskell no tiene literal para -infinito)
infinitoNegativo :: Float
infinitoNegativo = -(1 / 0)
