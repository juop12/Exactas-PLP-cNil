-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util
import Data.List (zipWith4)

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = Histograma l ((u - l) / fromIntegral n ) [0 | i <- [1..(n+2)]]

-- | Agrega un valor al histograma.
-- Idea: modificar cant_por_casillero con el elemento actualizado (+1 en el bucket que corresponda)
-- Dificultad: encontrar el índice del bucket donde cae el float.
-- Log: casilleros (agregar (vacio ...)) pisaba el agregado por cant_por_casillero <- [0,0,0,...], arreglado.
-- Log: idem anterior, pero no actualizaba el porcentaje, arreglado.
-- Falta hacer TESTs
agregar2 :: Float -> Histograma -> Histograma
agregar2 n (Histograma inicio tamIntervalo cant_por_casillero) = Histograma inicio tamIntervalo nuevo_cant_por_casillero
  where
    nuevo_cant_por_casillero = actualizarElem (calcularIndiceHist n listaDeLimitesSuperiores) (+1) cant_por_casillero
    listaDeLimitesSuperiores = [inicio, inicio+tamIntervalo .. (inicio+tamIntervalo * len)]
    --calcularIndiceReal k = if k>len-2 then len-1 else k 
    calcularIndiceHist n = foldl (\indice limSuperior -> if n >= limSuperior then indice + 1 else indice) 0
    len = fromIntegral (length cant_por_casillero)


-- Transformamos el valor en su índice para pasarlo a actualizarElem
agregar :: Float -> Histograma -> Histograma
agregar valor (Histograma inicio tamIntervalo cant_por_casillero) =
  Histograma inicio tamIntervalo nueva_cant_por_casillero
  where
    nueva_cant_por_casillero = actualizarElem (indice_real) (+1) cant_por_casillero
    indice_real = if valor < inicio 
      then 0
      else (if valor >= inicio + tamIntervalo * fromIntegral (length(cant_por_casillero)-2)
        then  length(cant_por_casillero)-1
        else  indice_candidato)
    indice_candidato = floor ((valor - inicio) / tamIntervalo) + 1
    tope = length cant_por_casillero

--agregar = foldl (\k x -> if (n < i + t * fromIntegral k) || (k > limiteSuperior) then k 
-- else calcularIndice n (k+1) (Histograma i t ls)) 0 []

--agregar n (Histograma inicio tamIntervalo cant_por_casillero) = Histograma inicio tamIntervalo cant_por_casillero_modificada
-- where cant_por_casillero_modificada = actualizarElem () cant_por_casillero


-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma cantidadBins (inicio, fin) datos = foldr agregar (vacio cantidadBins (inicio, fin)) datos

-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
-- casilleros (Histograma inicio tamañoDelIntervalo [0, 0, 0, 0, 0]) = error "COMPLETAR EJERCICIO 6"

casilleros (Histograma inicial tam cant_por_casillero) = zipWith4 Casillero f g h i
                                                   where
                                                        f = infinitoNegativo : [inicial + tam * fromIntegral i | i <- [0..(length cant_por_casillero-2)]]
                                                        g = [inicial + tam * fromIntegral i | i <- [0..(length cant_por_casillero-2)]] ++ [infinitoPositivo]
                                                        h = cant_por_casillero
                                                        i = calcularPorcentajes cant_por_casillero

{- casilleros (Histograma valorInicial tamIntervalo [0, 0, 0]) =
  [ Casillero infinitoNegativo valorInicial 0 0.0,
    Casillero valorInicial (valorInicial + tamIntervalo) 0 0.0,
    Casillero (valorInicial + tamIntervalo) infinitoPositivo 0 0.0
  ]
casilleros (Histograma valorInicial tamIntervalo [0, 0, 0, 0]) =
  [ Casillero infinitoNegativo valorInicial 0 0.0,
    Casillero valorInicial (valorInicial + tamIntervalo) 0 0.0,
    Casillero (valorInicial + tamIntervalo) (valorInicial + tamIntervalo + tamIntervalo) 0 0.0,
    Casillero (valorInicial + tamIntervalo + tamIntervalo) infinitoPositivo 0 0.0
  ]
--}
