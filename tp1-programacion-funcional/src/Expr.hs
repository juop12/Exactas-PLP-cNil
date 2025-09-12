module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- | Esquema de recursión estructural sobre Expr
foldExpr ::
  (Float -> b) ->           -- fConst
  (Float -> Float -> b) ->  -- fRango
  (b -> b -> b) ->          -- fSuma
  (b -> b -> b) ->          -- fResta
  (b -> b -> b) ->          -- fMult
  (b -> b -> b) ->          -- fDiv
   Expr ->                  -- exp
   b                        -- resultado
foldExpr fConst fRango fSuma fResta fMult fDiv expr =
  case expr of
    Const x     -> fConst  x
    Rango x y   -> fRango  x y
    Suma  e1 e2 -> fSuma  (r e1) (r e2)
    Resta e1 e2 -> fResta (r e1) (r e2)
    Mult  e1 e2 -> fMult  (r e1) (r e2)
    Div   e1 e2 -> fDiv   (r e1) (r e2)
    where
      r = foldExpr fConst fRango fSuma fResta fMult fDiv

-- | Esquema de recursión primitiva sobre Expr
recrExpr ::
  (Float -> b) ->                  -- fConst
  (Float -> Float -> b) ->         -- fRango
  (Expr -> b -> Expr -> b -> b) -> -- fSuma
  (Expr -> b -> Expr -> b -> b) -> -- fResta
  (Expr -> b -> Expr -> b -> b) -> -- fMult
  (Expr -> b -> Expr -> b -> b) -> -- fDiv
   Expr ->                         -- expr
   b                               -- resultado
recrExpr fConst fRango fSuma fResta fMult fDiv expr =
  case expr of
    Const x     -> fConst x
    Rango x y   -> fRango x y
    Suma  e1 e2 -> fSuma  e1 (r e1) e2 (r e2)
    Resta e1 e2 -> fResta e1 (r e1) e2 (r e2)
    Mult  e1 e2 -> fMult  e1 (r e1) e2 (r e2)
    Div   e1 e2 -> fDiv   e1 (r e1) e2 (r e2)
    where
      r = recrExpr fConst fRango fSuma fResta fMult fDiv

-- | @operadorBinarioGen operador g1 g2@ devuelve una función que al tomar un generador @gen@ aplica el operador binario @operador@
-- a los resultados de: la aplicación de @gen@ a @g1@ y la aplicación del generador @gen'@ (resultante de la aplicación anterior) a @g2@
operadorBinarioGen :: (a -> a -> a) -> G a -> G a -> G a
operadorBinarioGen operador g1 g2 gen = let (x, gen') = g1 gen
                                            (y, gen'') = g2 gen' -- uso gen' para que no repetir generador
                                        in (operador x y, gen'')

-- ? Con o sin gen en la definición de operadorBinarioGen? Osea, dejo una lambda explicita o no. atte: Fede
-- ? Debemos testear que funciona? Caso positivo, debemos testear que funcionan las otras auxiliares que definimos en los otros ej? 
-- TODO: PREGUNTAR

-- | Evaluar expresiones dado un generador de números aleatorios
eval :: Expr -> G Float
eval = foldExpr (,) (curry dameUno) (operadorBinarioGen (+)) (operadorBinarioGen (-))  (operadorBinarioGen (*)) (operadorBinarioGen (/))

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = error "COMPLETAR EJERCICIO 9"
-- Idea Fede: 
-- armarHistograma m n f g = let (datos, genFinal) = muestra f n g
--                           in (histograma m (rango95 datos) datos, genFinal)

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = error "COMPLETAR EJERCICIO 10"
-- (idea Fede) 
-- evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"


data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
