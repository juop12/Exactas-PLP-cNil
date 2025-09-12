module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Funcion auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

-- "nombre" ~: test ~?= valor,
allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar
      -- "Expr.Parser.parse" ~: testsParse,
      -- "App.mostrarFloat" ~: testsMostrarFloat,
      -- "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ "Dado una palabra mas corta que n, Cuando se alinea a la derecha, Entonces se agregan espacios adelante"
        ~: alinearDerecha 6 "hola"
        ~?= "  hola",
      "Dado una palabra mas larga que n, Cuando se alinea a la derecha, Entonces se devuelve la palabra original"
        ~: alinearDerecha 10 "incierticalc"
        ~?= "incierticalc",
      "Dado una cadena vacia, Cuando se alinea a la derecha con n=2, Entonces se obtienen dos espacios"
        ~: alinearDerecha 2 ""
        ~?= "  ",
      "Dado n negativo, Cuando se alinea a la derecha, Entonces se devuelve la palabra original"
        ~: alinearDerecha (-2) "hola"
        ~?= "hola"
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ "Dado una lista y un indice valido, Cuando se actualiza el primer elemento, Entonces el elemento se modifica"
        ~: actualizarElem 0 (+ 10) [1, 2, 3]
        ~?= [11, 2, 3],
      "Dado una lista y un indice valido, Cuando se actualiza el segundo elemento, Entonces el elemento se modifica"
        ~: actualizarElem 1 (+ 10) [1, 2, 3]
        ~?= [1, 12, 3],
      "Dado una lista y un indice mayor a la longitud, Cuando se intenta actualizar, Entonces la lista permanece igual"
        ~: actualizarElem 3 (+ 10) [1, 2, 3]
        ~?= [1, 2, 3],
      "Dado una lista y un indice negativo, Cuando se intenta actualizar, Entonces la lista permanece igual"
        ~: actualizarElem (-1) (+ 10) [1, 2, 3]
        ~?= [1, 2, 3],
      "Dado una lista de strings y un indice valido, Cuando se actualiza el cuarto elemento, Entonces el elemento se modifica"
        ~: actualizarElem 3 ('t' :) ["s", "t", "r", "i", "n", "g"]
        ~?= ["s", "t", "r", "ti", "n", "g"]
    ]

testsVacio :: Test
testsVacio =
  test
    [ {-"Dado un histograma vacio con ningun casillero, Cuando se consulta los casilleros, Entonces se obtienen unicamente los extremos"
        ~: casilleros (vacio 0 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 10 infinitoPositivo 0 0
            ], -}
      "Dado un histograma vacio con un casillero, Cuando se consulta los casilleros, Entonces se obtienen los extremos y el unico casillero"
        ~: casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      "Dado un histograma vacio con dos casilleros, Cuando se consulta los casilleros, Entonces se obtienen los extremos y los dos casilleros con el tamanio correspondiente"
        ~: casilleros (vacio 2 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 5 0 0,
              Casillero 5 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      "Dado un histograma vacio con tres casilleros, Cuando se consulta los casilleros, Entonces se obtienen los extremos y los tres casilleros con el tamanio correspondiente"
        ~: casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      "Dado un histograma vacio con tres casilleros y rango negativo, Cuando se consulta los casilleros, Entonces se obtienen los extremos y los tres casilleros con el tamanio correspondiente"
        ~: casilleros (vacio 3 (-6, 0))
        ~?= [ Casillero infinitoNegativo (-6) 0 0,
              Casillero (-6) (-4) 0 0,
              Casillero (-4) (-2) 0 0,
              Casillero (-2) 0 0 0,
              Casillero 0 infinitoPositivo 0 0
            ],
      "Dado un histograma vacio con tres casilleros y rango mixto, Cuando se consulta los casilleros, Entonces se obtienen los extremos y los tres casilleros con el tamanio correspondiente"
        ~: casilleros (vacio 3 (-3, 3))
        ~?= [ Casillero infinitoNegativo (-3) 0 0,
              Casillero (-3) (-1) 0 0,
              Casillero (-1) 1 0 0,
              Casillero 1 3 0 0,
              Casillero 3 infinitoPositivo 0 0
            ]
    ]

testsAgregar :: Test
testsAgregar =
  let histogramaDeTam1 = vacio 1 (0, 2)
      histogramaDeTam3 = vacio 3 (0, 6)
   in test
        [ "Dado un histograma de tamanio 1 y un valor en el limite inferior, Cuando se agrega el valor, Entonces el casillero correspondiente incrementa su cantidad"
            ~: casilleros (agregar 0 histogramaDeTam1)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores estan aca
                  Casillero 2 infinitoPositivo 0 0
                ],
          "Dado un histograma de tamanio 3 y un valor en el limite inferior, Cuando se agrega el valor, Entonces el primer casillero incrementa su cantidad"
            ~: casilleros (agregar 0 histogramaDeTam3)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores estan aca
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          "Dado un histograma de tamanio 3 y un valor en el segundo casillero, Cuando se agrega el valor, Entonces el segundo casillero incrementa su cantidad"
            ~: casilleros (agregar 2 histogramaDeTam3)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores estan aca
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          "Dado un histograma de tamanio 3 y un valor fuera del rango inferior, Cuando se agrega el valor, Entonces el casillero de infinito negativo incrementa su cantidad"
            ~: casilleros (agregar (-1) histogramaDeTam3)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores estan aca
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          "Dado un histograma de tamanio 3 y un valor fuera del rango superior, Cuando se agrega el valor, Entonces el casillero de infinito positivo incrementa su cantidad"
            ~: casilleros (agregar 7 histogramaDeTam3)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 1 100 -- El 100% de los valores estan aca
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ "Dado ningun dato, Cuando se genera el histograma, Entonces se obtiene el histograma vacio"
        ~: histograma 4 (1, 5) []
        ~?= vacio 4 (1, 5),
      "Dado datos dentro de un rango positivo, Cuando se genera el histograma, Entonces los datos se agregan a los casilleros correspondientes"
        ~: histograma 4 (1, 5) [1, 2, 3]
        ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      "Dado datos dentro de un rango negativo, Cuando se genera el histograma, Entonces los datos se agregan a los casilleros correspondientes"
        ~: histograma 4 (-10, -3) [-10, -7, -2]
        ~?= agregar (-10) (agregar (-7) (agregar (-2) (vacio 4 (-10, -3)))),
      "Dado datos dentro de un rango mixto, Cuando se genera el histograma, Entonces los datos se agregan a los casilleros correspondientes"
        ~: histograma 4 (-10, 5) [-10, -7, 2]
        ~?= agregar (-10) (agregar (-7) (agregar 2 (vacio 4 (-10, 5)))),
      "Dado datos con extremos decimales, Cuando se genera el histograma, Entonces los datos se agregan a los casilleros correspondientes"
        ~: histograma 4 (1.5, 5.5) [1.5, 2.5, 3.5]
        ~?= agregar 3.5 (agregar 2.5 (agregar 1.5 (vacio 4 (1.5, 5.5)))),
      -- "Dado rango nulo, Cuando se genera el histograma, Entonces deberia dar error" -- test comentado
      "Dado datos fuera del rango, Cuando se genera el histograma, Entonces los datos se agregan a los extremos"
        ~: histograma 4 (1, 5) [-10, 0, 6, 10]
        ~?= agregar (-10) (agregar 0 (agregar 6 (agregar 10 (vacio 4 (1, 5))))),
      "Dado varios datos en el mismo casillero, Cuando se genera el histograma, Entonces todos los datos se agregan al mismo casillero"
        ~: histograma 4 (1, 5) [1, 1.5, 1.8]
        ~?= agregar 1.8 (agregar 1.5 (agregar 1 (vacio 4 (1, 5)))),
      "Dado todos los datos iguales, Cuando se genera el histograma, Entonces todos los datos se agregan al mismo casillero"
        ~: histograma 4 (1, 5) [3, 3, 3, 3]
        ~?= agregar 3 (agregar 3 (agregar 3 (agregar 3 (vacio 4 (1, 5)))))
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ "Dado un histograma vacio de tamanio 1 y rango (0,1), Cuando se consulta los casilleros, Entonces se obtienen los extremos y el unico casillero"
        ~: casilleros (vacio 1 (0, 1))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 1.0 0 0.0,
              Casillero 1.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 1 y rango (0,2), Cuando se consulta los casilleros, Entonces se obtienen los extremos y el unico casillero"
        ~: casilleros (vacio 1 (0, 2))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 1 y rango (1,2), Cuando se consulta los casilleros, Entonces se obtienen los extremos y el unico casillero"
        ~: casilleros (vacio 1 (1, 2))
        ~?= [ Casillero infinitoNegativo 1.0 0 0.0,
              Casillero 1.0 2.0 0 0.0,
              Casillero 2.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 2 y rango (-10,10), Cuando se consulta los casilleros, Entonces se obtienen los extremos y los dos casilleros"
        ~: casilleros (vacio 2 (-10, 10))
        ~?= [ Casillero infinitoNegativo (-10.0) 0 0.0,
              Casillero (-10.0) 0 0 0.0,
              Casillero 0.0 10.0 0 0.0,
              Casillero 10.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 2 y rango (0,1), Cuando se consulta los casilleros, Entonces se obtienen los extremos y los dos casilleros"
        ~: casilleros (vacio 2 (0, 1))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 0.5 0 0.0,
              Casillero 0.5 1.0 0 0.0,
              Casillero 1.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), Cuando se consulta los casilleros, Entonces se obtienen los extremos y los tres casilleros"
        ~: casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), Cuando se agrega el valor 2, Entonces el casillero correspondiente incrementa su cantidad"
        ~: casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), Cuando se agrega dos veces el valor 2, Entonces el casillero correspondiente incrementa su cantidad a 2"
        ~: casilleros (agregar 2 (agregar 2 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 2 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), Cuando se agrega los valores 1 y 2, Entonces los casilleros correspondientes incrementan su cantidad"
        ~: casilleros (agregar 1 (agregar 2 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 1 50.0,
              Casillero 2.0 4.0 1 50.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), Cuando se agrega un valor fuera del rango inferior, Entonces el casillero de infinito negativo incrementa su cantidad"
        ~: casilleros (agregar (-1) (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 1 100.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), Cuando se agrega un valor fuera del rango superior, Entonces el casillero de infinito positivo incrementa su cantidad"
        ~: casilleros (agregar 7 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 1 100.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), Cuando se agrega un valor muy fuera del rango superior, Entonces el casillero de infinito positivo incrementa su cantidad"
        ~: casilleros (agregar 100 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 1 100.0
            ]
    ]

testsRecr :: Test
testsRecr =
  let suma ex x ey y = x + y
      resta ex x ey y = x - y
      mult ex x ey y = x * y
      div ex x ey y = x / y
   in test
        [ "Dado un expr Const 1, Cuando se aplica recrExpr, Entonces se obtiene Const 1"
            ~: recrExpr id (\x y -> y - x) suma resta mult div (Const 1)
            ~?= 1,
          "Dado un expr Rango 1 5, Cuando se aplica recrExpr, Entonces se obtiene 4"
            ~: recrExpr id (\x y -> y - x) suma resta mult div (Rango 1 5)
            ~?= 4,
          "Dado un expr Suma (Const 1) (Const 2), Cuando se aplica recrExpr, Entonces se obtiene 3"
            ~: recrExpr id (\x y -> y - x) suma resta mult div (Suma (Const 1) (Const 2))
            ~?= 3,
          "Dado un expr Resta (Const 5) (Const 2), Cuando se aplica recrExpr, Entonces se obtiene 3"
            ~: recrExpr id (\x y -> y - x) suma resta mult div (Resta (Const 5) (Const 2))
            ~?= 3,
          "Dado un expr Mult (Const 2) (Const 3), Cuando se aplica recrExpr, Entonces se obtiene 6"
            ~: recrExpr id (\x y -> y - x) suma resta mult div (Mult (Const 2) (Const 3))
            ~?= 6,
          "Dado un expr Div (Const 6) (Const 2), Cuando se aplica recrExpr, Entonces se obtiene 3"
            ~: recrExpr id (\x y -> y - x) suma resta mult div (Div (Const 6) (Const 2))
            ~?= 3,
          "Dado un expr Suma (Const 1) (Rango 1 5), Cuando se aplica recrExpr, Entonces se obtiene 5"
            ~: recrExpr id (\x y -> y - x) suma resta mult div (Suma (Const 1) (Rango 1 5))
            ~?= 5,
          "Dado un expr Suma (Rango 1 5) (Rango 1 5), Cuando se aplica recrExpr, Entonces se obtiene 8"
            ~: recrExpr id (\x y -> y - x) suma resta mult div (Suma (Rango 1 5) (Rango 1 5))
            ~?= 8
        ]

testsFold :: Test
testsFold =
  test -- Estoy utilizando (\x y -> y - x) como fRango.
    [ "Dado un expr Const 1, Cuando se aplica foldExpr, Entonces se obtiene Const 1"
        ~: foldExpr id (\x y -> y - x) (+) (-) (*) (/) (Const 1)
        ~?= 1,
      "Dado un expr Rango 1 5, Cuando se aplica foldExpr, Entonces se obtiene 4"
        ~: foldExpr id (\x y -> y - x) (+) (-) (*) (/) (Rango 1 5)
        ~?= 4,
      "Dado un expr Suma (Const 1) (Const 2), Cuando se aplica foldExpr, Entonces se obtiene 3"
        ~: foldExpr id (\x y -> y - x) (+) (-) (*) (/) (Suma (Const 1) (Const 2))
        ~?= 3,
      "Dado un expr Resta (Const 5) (Const 2), Cuando se aplica foldExpr, Entonces se obtiene 3"
        ~: foldExpr id (\x y -> y - x) (+) (-) (*) (/) (Resta (Const 5) (Const 2))
        ~?= 3,
      "Dado un expr Mult (Const 2) (Const 3), Cuando se aplica foldExpr, Entonces se obtiene 6"
        ~: foldExpr id (\x y -> y - x) (+) (-) (*) (/) (Mult (Const 2) (Const 3))
        ~?= 6,
      "Dado un expr Div (Const 6) (Const 2), Cuando se aplica foldExpr, Entonces se obtiene 3"
        ~: foldExpr id (\x y -> y - x) (+) (-) (*) (/) (Div (Const 6) (Const 2))
        ~?= 3,
      "Dado un expr Suma (Const 1) (Rango 1 5), Cuando se aplica foldExpr, Entonces se obtiene 5"
        ~: foldExpr id (\x y -> y - x) (+) (-) (*) (/) (Suma (Const 1) (Rango 1 5))
        ~?= 5,
      "Dado un expr Suma (Rango 1 5) (Rango 1 5), Cuando se aplica foldExpr, Entonces se obtiene 8"
        ~: foldExpr id (\x y -> y - x) (+) (-) (*) (/) (Suma (Rango 1 5) (Rango 1 5))
        ~?= 8
    ]

testsEval :: Test
testsEval =
  test
    [ "Dado un expr Suma (Rango 1 5) (Const 1) con genFijo, Cuando se aplica fst . eval, Entonces se obtiene 4.0"
        ~: fst (eval (Suma (Rango 1 5) (Const 1)) genFijo)
        ~?= 4.0,
      "Dado un expr Suma (Rango 1 5) (Const 1) con genNormalConSemilla 0, Cuando se aplica fst . eval, Entonces se obtiene 3.7980492"
        ~: fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0))
        ~?= 3.7980492, -- el primer rango evalua a 2.7980492
      "Dado un expr Suma (Rango 1 5) (Rango 1 5) con genNormalConSemilla 0, Cuando se aplica fst . eval, Entonces se obtiene 5.92308"
        ~: fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0))
        ~?= 5.92308, -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      "Dado un expr Resta (Rango 1 5) (Rango 1 5) con genNormalConSemilla 0, Cuando se aplica fst . eval, Entonces se obtiene -0.32698154"
        ~: fst (eval (Resta (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0))
        ~?= -0.32698154,
      "Dado un expr Mult (Resta (Rango 1 5) (Rango 1 5)) (Rango 1 5) con genNormalConSemilla 0, Cuando se aplica fst . eval, Entonces se obtiene -1.7866315"
        ~: fst (eval (Mult (Resta (Rango 1 5) (Rango 1 5)) (Rango 1 5)) (genNormalConSemilla 0))
        ~?= -1.7866315, -- el primer rango evalua a 2.7980492, el segundo a 3.1250308 y el tercero a 5.464013
      "Dado un expr Mult (Resta (Rango (-10) 10) (Rango (-10) 10)) (Rango 1 5) con genNormalConSemilla 0, Cuando se aplica fst . eval, Entonces se obtiene -1.7866315"
        ~: fst (eval (Mult (Resta (Rango (-10) 10) (Rango 100 1000)) (Rango 1 5)) (genNormalConSemilla 0))
        ~?= -3164.4377, -- el primer rango evalua a -1.0097542, el segundo a 578.1319 y el tercero a 5.464013
        -- rango 1 5 recibe misma semilla 2
      let (x, gen) = eval (Rango (-10) 10) (genNormalConSemilla 0)
          (y, gen') = eval (Rango 100 1000) gen
          (z, gen'') = eval (Rango 1 5) gen'
       in (x, y, z) ~?= (-1.0097542, 578.1319, 5.464013),
      let (x, gen) = eval (Rango 1 5) (genNormalConSemilla 0)
          (y, gen') = eval (Rango 1 5) gen
          (z, gen'') = eval (Rango 1 5) gen'
       in (x, y, z) ~?= (2.7980492, 3.1250308, 5.464013)
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [ "Dado 1 casillero, 3 experimentos, funcion que use dameUno con generador Fijo, Cuando se aplica armarHistograma, Entonces se obtiene casillero único completo"
        ~: casilleros (fst (armarHistograma 1 3 (curry dameUno 1 5) genFijo))
        ~?= [ Casillero infinitoNegativo 2 0 0.0,
              Casillero 2 4 3 100.0, -- El 100% de los valores estan aca
              Casillero 4 infinitoPositivo 0 0.0
            ], -- el primer rango evalua a 3, el segundo a 3 y el tercero a 3
      "Dado 3 casilleros, 10 experimentos, funcion que use dameUno con generador Fijo, Cuando se aplica armarHistograma, Entonces se obtiene un casillero completo"
        ~: casilleros (fst (armarHistograma 3 10 (curry dameUno 1 5) genFijo))
        ~?= [ Casillero infinitoNegativo 2 0 0.0,
              Casillero 2 2.6666667 0 0.0,
              Casillero 2.6666667 3.3333335 10 100.0, -- El 100% de los valores estan aca
              Casillero 3.3333335 4 0 0.0,
              Casillero 4 infinitoPositivo 0 0.0
            ], -- el primer rango evalua a 3, el segundo a 3 y análogamente todos los experimentos
      "Dado 1 casillero, 3 experimentos, funcion que use dameUno con generador Normal con Semilla, Cuando se aplica armarHistograma, Entonces se obtiene "
        ~: casilleros (fst (armarHistograma 1 3 (curry dameUno 1 5) (genNormalConSemilla 0)))
        ~?= [ Casillero infinitoNegativo 1.4687741 0 0.0,
              Casillero 1.4687741 6.1226206 3 100.0, -- El 100% de los valores estan aca (pocas muestras)
              Casillero 6.1226206 infinitoPositivo 0 0.0
            ], -- el primer rango evalua a 2.7980492, el segundo a 3.1250308 y el tercero a 5.464013
      "Dado 1 casillero, 1000 experimentos, funcion que use dameUno, Cuando se aplica armarHistograma, Entonces se obtiene muestras bien distribuidas"
        ~: casilleros (fst (armarHistograma 1 1000 (curry dameUno 1 5) (genNormalConSemilla 0)))
        ~?= [ Casillero infinitoNegativo 1.0136857 25 2.5,
              Casillero 1.0136857 4.966351 955 95.5, -- El ~95% de los valores estan aca (bastantes muestras)
              Casillero 4.966351 infinitoPositivo 20 2.0
            ],
      "Dado 4 casilleros, 1000 experimentos, funcion que use dameUno, Cuando se aplica armarHistograma, Entonces vemos la distribución"
        ~: casilleros (fst (armarHistograma 4 1000 (curry dameUno 1 5) (genNormalConSemilla 0)))
        ~?= [ Casillero infinitoNegativo 1.0136857 25 2.5, -- El ~2.5% de los valores estan aca
              Casillero 1.0136857 2.001852 129 12.9,
              Casillero 2.001852 2.9900184 338 33.8,
              Casillero 2.9900184 3.9781847 347 34.7,
              Casillero 3.9781847 4.966351 141 14.1,
              Casillero 4.966351 infinitoPositivo 20 2.0 -- El ~2.5% de los valores estan aca
            ],
      "Dado 11 casilleros, 100000 experimentos, funcion que use dameUno, Cuando se aplica armarHistograma, Entonces vemos la distribución"
        ~: casilleros (fst (armarHistograma 10 100000 (curry dameUno 1 5) (genNormalConSemilla 0)))
        ~?= [ Casillero infinitoNegativo 1.0097816 2464 2.464, -- El ~2.5% de los valores estan aca
              Casillero 1.0097816 1.4086598 3299 3.299,
              Casillero 1.4086598 1.807538 6074 6.0740004,
              Casillero 1.807538 2.2064161 9798 9.798,
              Casillero 2.2064161 2.6052945 13276 13.276,
              Casillero 2.6052945 3.0041728 15229 15.229,
              Casillero 3.0041728 3.403051 15200 15.2,
              Casillero 3.403051 3.8019292 13068 13.068,
              Casillero 3.8019292 4.2008076 9608 9.608,
              Casillero 4.2008076 4.5996857 6074 6.0740004,
              Casillero 4.5996857 4.998564 3365 3.365,
              Casillero 4.998564 infinitoPositivo 2545 2.545 -- El ~2.5% de los valores estan aca
            ]
    {-  "Dado 11 casilleros, 100000 experimentos, funcion que use dameUno, Cuando se aplica armarHistograma, Entonces vemos la distribución"
        ~: casilleros (fst (armarHistograma 10 100000 (operadorBinarioGen (*) (curry dameUno 1 5) (curry dameUno 5 30)) (genNormalConSemilla 0)))
        ~?= [ Casillero infinitoNegativo 1.0097816 2464 2.464, -- El ~2.5% de los valores estan aca
              Casillero 0 0 0 0,
              Casillero 4.998564 infinitoPositivo 2545 2.545 -- El ~2.5% de los valores estan aca
            ]-}
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [completar]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 66.67%",
                  "0.00 - 2.00 |░░░░░░░░░░░░░░░",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
