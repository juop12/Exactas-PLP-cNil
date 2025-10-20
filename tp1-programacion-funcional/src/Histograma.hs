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

import Data.List (zipWith4)
import Util

data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @limiteInferior < limiteSuperior@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio cantDeCasilleros (inicioDeHist, finDeHist) = Histograma inicioDeHist tamIntervalo cantElemPorCasillero
  where
    tamIntervalo = (finDeHist - inicioDeHist) / fromIntegral cantDeCasilleros
    cantElemPorCasillero = replicate (cantDeCasilleros + 2) 0 

-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar valor (Histograma inicio tamIntervalo cantElemPorCasillero) = Histograma inicio tamIntervalo nuevaCantElemPorCasillero
  where
    nuevaCantElemPorCasillero = actualizarElem indice (+ 1) cantElemPorCasillero
    indice = max 0 (min (length cantElemPorCasillero - 1) indiceCandidato)        -- me aseguro que el indice esté entre 0 y la longitud de la lista - 1
    indiceCandidato = floor ((valor - inicio) / tamIntervalo) + 1                 -- +1 porque el primer casillero es el de -inf
      
-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
-- | Requiere: inicio < fin y cantidadBins >= 1
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma cantDeCasilleros (inicioDeHist, finDeHist) datos = foldr agregar histVacio datos
  where
    histVacio = vacio cantDeCasilleros (inicioDeHist, finDeHist)

-- =========================================================================
-- Casillero
-- =========================================================================

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
casilleros (Histograma inicio tamIntervalo cantElemPorCasillero) = zipWith4 Casillero minimosPorCasillero maximosPorCasillero cantElemPorCasillero porcentajesPorCasillero
  where
    limitesFinitos = [inicio + tamIntervalo * fromIntegral i | i <- [0..(length cantElemPorCasillero - 2)]]
    minimosPorCasillero = infinitoNegativo : limitesFinitos
    maximosPorCasillero = limitesFinitos ++ [infinitoPositivo]
    total = sum cantElemPorCasillero
    porcentajesPorCasillero = map (\x -> if total==0 then 0 else fromIntegral x / fromIntegral total * 100) cantElemPorCasillero