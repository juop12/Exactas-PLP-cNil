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
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio cantDeCasilleros (inicioDeHist, finDeHist) = Histograma inicio tamIntervalo cantElemPorCasillero
  where
    inicio = inicioDeHist
    tamIntervalo = (finDeHist - inicioDeHist) / fromIntegral cantDeCasilleros
    cantElemPorCasillero = [0 | i <- [1 .. (cantDeCasilleros + 2)]] -- length [0,0,...,0] ~> cantDeCasilleros + 2

-- | Agrega un valor al histograma.
agregar :: Float -> Histograma -> Histograma
agregar valor (Histograma inicio tamIntervalo cantElemPorCasillero) =
  Histograma inicio tamIntervalo nuevaCantElemPorCasillero
  where
    nuevaCantElemPorCasillero = actualizarElem indice (+ 1) cantElemPorCasillero
    indice -- Se calcula el infice apartir de aritmetica
      | valor < inicio = 0
      | valor >= tope = length cantElemPorCasillero - 1
      | otherwise = indiceCandidato
    indiceCandidato = floor ((valor - inicio) / tamIntervalo) + 1
    tope = inicio + tamIntervalo * fromIntegral (length cantElemPorCasillero - 2)

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
casilleros (Histograma inicial tamIntervalo cantElemPorCasillero) = zipWith4 Casillero minimosPorCasillero maximosPorCasillero cantPorCasillero porcentajesPorCasillero
  where
    indices = [0 .. (length cantElemPorCasillero - 2)] -- Se abstrae la lista de indices para crear los minimos y maximos
    minimosPorCasillero = infinitoNegativo : [inicial + tamIntervalo * fromIntegral i | i <- indices]
    maximosPorCasillero = [inicial + tamIntervalo * fromIntegral i | i <- indices] ++ [infinitoPositivo]
    cantPorCasillero = cantElemPorCasillero
    porcentajesPorCasillero = calcularPorcentajes cantElemPorCasillero
