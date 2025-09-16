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
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ "Dado una palabra mas corta que n, cuando se alinea a la derecha, entonces se agregan espacios adelante"
        ~: alinearDerecha 6 "hola"
        ~?= "  hola",
      "Dado una palabra igual de larga que n, cuando se alinea a la derecha, entonces no agrega nada"
        ~: alinearDerecha 4 "hola"
        ~?= "hola",
      "Dado una palabra mas larga que n, cuando se alinea a la derecha, entonces se devuelve la palabra original"
        ~: alinearDerecha 10 "incierticalc"
        ~?= "incierticalc",
      "Dado una cadena vacia, cuando se alinea a la derecha con n=2, entonces se obtienen dos espacios"
        ~: alinearDerecha 2 ""
        ~?= "  ",
      "Dado n negativo, cuando se alinea a la derecha, entonces se devuelve la palabra original"
        ~: alinearDerecha (-2) "hola"
        ~?= "hola"
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ "Dado una lista y un indice valido, cuando se actualiza el primer elemento, entonces el elemento se modifica"
        ~: actualizarElem 0 (+ 10) [1, 2, 3]
        ~?= [11, 2, 3],
      "Dado una lista y un indice valido, cuando se actualiza el segundo elemento, entonces el elemento se modifica"
        ~: actualizarElem 1 (+ 10) [1, 2, 3]
        ~?= [1, 12, 3],
      "Dado una lista y un indice invalido (mayor a la longitud), cuando se intenta actualizar, entonces la lista permanece igual"
        ~: actualizarElem 3 (+ 10) [1, 2, 3]
        ~?= [1, 2, 3],
      "Dado una lista y un indice invalido (negativo), cuando se intenta actualizar, entonces la lista permanece igual"
        ~: actualizarElem (-1) (+ 10) [1, 2, 3]
        ~?= [1, 2, 3],
      "Dado una lista de strings y un indice valido, cuando se actualiza el cuarto elemento, entonces el elemento se modifica"
        ~: actualizarElem 3 ('t' :) ["s", "t", "r", "i", "n", "g"]
        ~?= ["s", "t", "r", "ti", "n", "g"]
    ]

testsVacio :: Test
testsVacio =
  test
    [ "Dado un histograma vacio con un casillero, cuando se consulta los casilleros, entonces se obtienen los extremos y el unico casillero"
        ~: casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      "Dado un histograma vacio con dos casilleros, cuando se consulta los casilleros, entonces se obtienen los extremos y los dos casilleros con el tamanio correspondiente"
        ~: casilleros (vacio 2 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 5 0 0,
              Casillero 5 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      "Dado un histograma vacio con tres casilleros, cuando se consulta los casilleros, entonces se obtienen los extremos y los tres casilleros con el tamanio correspondiente"
        ~: casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      "Dado un histograma vacio con tres casilleros y rango negativo, cuando se consulta los casilleros, entonces se obtienen los extremos y los tres casilleros con el tamanio correspondiente"
        ~: casilleros (vacio 3 (-6, 0))
        ~?= [ Casillero infinitoNegativo (-6) 0 0,
              Casillero (-6) (-4) 0 0,
              Casillero (-4) (-2) 0 0,
              Casillero (-2) 0 0 0,
              Casillero 0 infinitoPositivo 0 0
            ],
      "Dado un histograma vacio con tres casilleros y rango mixto, cuando se consulta los casilleros, entonces se obtienen los extremos y los tres casilleros con el tamanio correspondiente"
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
        [ "Dado un histograma de tamanio 1 y un valor en el limite inferior, cuando se agrega el valor, entonces el segundo casillero incrementa su cantidad"
            ~: casilleros (agregar 0 histogramaDeTam1)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores estan aca
                  Casillero 2 infinitoPositivo 0 0
                ],
          "Dado un histograma de tamanio 1 y un valor en el limite superior, cuando se agrega el valor, entonces el último casillero incrementa su cantidad"
            ~: casilleros (agregar 2 histogramaDeTam1)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0, 
                  Casillero 2 infinitoPositivo 1 100 -- El 100% de los valores estan aca
                ],
          "Dado un histograma de tamanio 3 y un valor en el limite inferior, cuando se agrega el valor, entonces el segundo casillero incrementa su cantidad"
            ~: casilleros (agregar 0 histogramaDeTam3)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores estan aca
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          "Dado un histograma de tamanio 3 y un valor en el segundo casillero, cuando se agrega el valor, entonces el tercer casillero incrementa su cantidad"
            ~: casilleros (agregar 2 histogramaDeTam3)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores estan aca
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          "Dado un histograma de tamanio 3 y un valor fuera del rango inferior, cuando se agrega el valor, entonces el primer casillero (infinito negativo) incrementa su cantidad"
            ~: casilleros (agregar (-1) histogramaDeTam3)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores estan aca
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          "Dado un histograma de tamanio 3 y un valor fuera del rango superior, cuando se agrega el valor, entonces el último casillero (infinito positivo) incrementa su cantidad"
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
    [ "Dado ningun dato, cuando se genera el histograma, entonces se obtiene el histograma vacio"
        ~: casilleros (histograma 4 (1, 5) [])
        ~?= [ Casillero infinitoNegativo 1 0 0,
              Casillero 1 2 0 0,
              Casillero 2 3 0 0,
              Casillero 3 4 0 0,
              Casillero 4 5 0 0,
              Casillero 5 infinitoPositivo 0 0 
            ],
      "Dado datos dentro de un rango positivo, cuando se genera el histograma, entonces los datos se agregan a los casilleros correspondientes"
        ~: casilleros(histograma 4 (1, 5) [1, 2, 3])
        ~?= [ Casillero infinitoNegativo 1 0 0,
              Casillero 1 2 1 33.333336,
              Casillero 2 3 1 33.333336,
              Casillero 3 4 1 33.333336,
              Casillero 4 5 0 0,
              Casillero 5 infinitoPositivo 0 0 
            ],
      "Dado datos dentro de un rango negativo, cuando se genera el histograma, entonces los datos se agregan a los casilleros correspondientes"
        ~: casilleros (histograma 4 (-11, -3) [-10, -7, -2])
        ~?= [ Casillero infinitoNegativo (-11) 0 0,
              Casillero (-11) (-9) 1 33.333336,
              Casillero (-9)  (-7) 0 0,
              Casillero (-7)  (-5) 1 33.333334,
              Casillero (-5)  (-3) 0 0,
              Casillero (-3) infinitoPositivo 1 33.333336 
            ],
      "Dado datos dentro de un rango mixto, cuando se genera el histograma, entonces los datos se agregan a los casilleros correspondientes"
        ~: casilleros (histograma 5 (-10, 5) [-10, -7, 2])
        ~?= [ Casillero infinitoNegativo (-10) 0 0,
              Casillero (-10) (-7) 1 33.333336,
              Casillero (-7)  (-4) 1 33.333336,
              Casillero (-4)  (-1) 0 0,
              Casillero (-1)    2  0 0,
              Casillero   2     5  1 33.333336,
              Casillero   5   infinitoPositivo 0 0 
            ],
      "Dado datos con extremos decimales, cuando se genera el histograma, entonces los datos se agregan a los casilleros correspondientes"
        ~: casilleros (histograma 4 (1.5, 5.5) [1.5, 2.5, 3.5])
        ~?= [ Casillero infinitoNegativo 1.5 0 0,
              Casillero 1.5 2.5 1 33.333336,
              Casillero 2.5 3.5 1 33.333336,
              Casillero 3.5 4.5 1 33.333336,
              Casillero 4.5 5.5 0 0,
              Casillero 5.5 infinitoPositivo 0 0 
            ],
      "Dado datos fuera del rango, cuando se genera el histograma, entonces los datos se agregan a los extremos"
        ~: casilleros (histograma 4 (1, 5) [-10, 0, 6, 10])
        ~?= [ Casillero infinitoNegativo 1 2 50,
              Casillero 1 2 0 0,
              Casillero 2 3 0 0,
              Casillero 3 4 0 0,
              Casillero 4 5 0 0,
              Casillero 5 infinitoPositivo 2 50 
            ],
      "Dado varios datos en el mismo casillero, cuando se genera el histograma, entonces todos los datos se agregan al mismo casillero"
        ~: casilleros (histograma 4 (1, 5) [1, 1.5, 1.8])
        ~?= [ Casillero infinitoNegativo 1 0 0,
              Casillero 1 2 3 100,
              Casillero 2 3 0 0,
              Casillero 3 4 0 0,
              Casillero 4 5 0 0,
              Casillero 5 infinitoPositivo 0 0 
            ],
      "Dado todos los datos iguales, cuando se genera el histograma, entonces todos los datos se agregan al mismo casillero"
        ~: casilleros (histograma 4 (1, 5) [3, 3, 3, 3])
        ~?= [ Casillero infinitoNegativo 1 0 0,
              Casillero 1 2 0 0,
              Casillero 2 3 0 0,
              Casillero 3 4 4 100,
              Casillero 4 5 0 0,
              Casillero 5 infinitoPositivo 0 0 
            ]
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ "Dado un histograma vacio de tamanio 1 y rango (0,1), cuando se consulta los casilleros, entonces se obtienen los extremos y el unico casillero"
        ~: casilleros (vacio 1 (0, 1))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 1.0 0 0.0,
              Casillero 1.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 1 y rango (0,2), cuando se consulta los casilleros, entonces se obtienen los extremos y el unico casillero"
        ~: casilleros (vacio 1 (0, 2))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 1 y rango (1,2), cuando se consulta los casilleros, entonces se obtienen los extremos y el unico casillero"
        ~: casilleros (vacio 1 (1, 2))
        ~?= [ Casillero infinitoNegativo 1.0 0 0.0,
              Casillero 1.0 2.0 0 0.0,
              Casillero 2.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 2 y rango (-10,10), cuando se consulta los casilleros, entonces se obtienen los extremos y los dos casilleros"
        ~: casilleros (vacio 2 (-10, 10))
        ~?= [ Casillero infinitoNegativo (-10.0) 0 0.0,
              Casillero (-10.0) 0 0 0.0,
              Casillero 0.0 10.0 0 0.0,
              Casillero 10.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 2 y rango (0,1), cuando se consulta los casilleros, entonces se obtienen los extremos y los dos casilleros"
        ~: casilleros (vacio 2 (0, 1))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 0.5 0 0.0,
              Casillero 0.5 1.0 0 0.0,
              Casillero 1.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), cuando se consulta los casilleros, entonces se obtienen los extremos y los tres casilleros"
        ~: casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), cuando se agrega el valor 2, entonces el casillero correspondiente incrementa su cantidad"
        ~: casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), cuando se agrega dos veces el valor 2, entonces el casillero correspondiente incrementa su cantidad a 2"
        ~: casilleros (agregar 2 (agregar 2 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 2 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), cuando se agrega los valores 1 y 2, entonces los casilleros correspondientes incrementan su cantidad"
        ~: casilleros (agregar 1 (agregar 2 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 1 50.0,
              Casillero 2.0 4.0 1 50.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), cuando se agrega un valor fuera del rango inferior, entonces el casillero de infinito negativo incrementa su cantidad"
        ~: casilleros (agregar (-1) (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 1 100.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), cuando se agrega un valor fuera del rango superior, entonces el casillero de infinito positivo incrementa su cantidad"
        ~: casilleros (agregar 7 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 1 100.0
            ],
      "Dado un histograma vacio de tamanio 3 y rango (0,6), cuando se agrega un valor muy fuera del rango superior, entonces el casillero de infinito positivo incrementa su cantidad"
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
  let rango x y = y - x
      suma ex x ey y = x + y
      resta ex x ey y = x - y
      mult ex x ey y = x * y
      div ex x ey y = x / y
   in test
        [ "Dado un expr Const 1, cuando se aplica recrExpr, entonces se obtiene Const 1"
            ~: recrExpr id rango suma resta mult div (Const 1)
            ~?= 1,
          "Dado un expr Rango 1 5, cuando se aplica recrExpr, entonces se obtiene 4"
            ~: recrExpr id rango suma resta mult div (Rango 1 5)
            ~?= 4,
          "Dado un expr Suma (Const 1) (Const 2), cuando se aplica recrExpr, entonces se obtiene 3"
            ~: recrExpr id rango suma resta mult div (Suma (Const 1) (Const 2))
            ~?= 3,
          "Dado un expr Resta (Const 5) (Const 2), cuando se aplica recrExpr, entonces se obtiene 3"
            ~: recrExpr id rango suma resta mult div (Resta (Const 5) (Const 2))
            ~?= 3,
          "Dado un expr Mult (Const 2) (Const 3), cuando se aplica recrExpr, entonces se obtiene 6"
            ~: recrExpr id rango suma resta mult div (Mult (Const 2) (Const 3))
            ~?= 6,
          "Dado un expr Div (Const 6) (Const 2), cuando se aplica recrExpr, entonces se obtiene 3"
            ~: recrExpr id rango suma resta mult div (Div (Const 6) (Const 2))
            ~?= 3,
          "Dado un expr Suma (Const 1) (Rango 1 5), cuando se aplica recrExpr, entonces se obtiene 5"
            ~: recrExpr id rango suma resta mult div (Suma (Const 1) (Rango 1 5))
            ~?= 5,
          "Dado un expr Suma (Rango 1 5) (Rango 1 5), cuando se aplica recrExpr, entonces se obtiene 8"
            ~: recrExpr id rango suma resta mult div (Suma (Rango 1 5) (Rango 1 5))
            ~?= 8
        ]

testsFold :: Test
testsFold =
  let rango x y = y - x
   in test
    [ "Dado un expr Const 1, cuando se aplica foldExpr, entonces se obtiene Const 1"
        ~: foldExpr id rango (+) (-) (*) (/) (Const 1)
        ~?= 1,
      "Dado un expr Rango 1 5, cuando se aplica foldExpr, entonces se obtiene 4"
        ~: foldExpr id rango (+) (-) (*) (/) (Rango 1 5)
        ~?= 4,
      "Dado un expr Suma (Const 1) (Const 2), cuando se aplica foldExpr, entonces se obtiene 3"
        ~: foldExpr id rango (+) (-) (*) (/) (Suma (Const 1) (Const 2))
        ~?= 3,
      "Dado un expr Resta (Const 5) (Const 2), cuando se aplica foldExpr, entonces se obtiene 3"
        ~: foldExpr id rango (+) (-) (*) (/) (Resta (Const 5) (Const 2))
        ~?= 3,
      "Dado un expr Mult (Const 2) (Const 3), cuando se aplica foldExpr, entonces se obtiene 6"
        ~: foldExpr id rango (+) (-) (*) (/) (Mult (Const 2) (Const 3))
        ~?= 6,
      "Dado un expr Div (Const 6) (Const 2), cuando se aplica foldExpr, entonces se obtiene 3"
        ~: foldExpr id rango (+) (-) (*) (/) (Div (Const 6) (Const 2))
        ~?= 3,
      "Dado un expr Suma (Const 1) (Rango 1 5), cuando se aplica foldExpr, entonces se obtiene 5"
        ~: foldExpr id rango (+) (-) (*) (/) (Suma (Const 1) (Rango 1 5))
        ~?= 5,
      "Dado un expr Suma (Rango 1 5) (Rango 1 5), cuando se aplica foldExpr, entonces se obtiene 8"
        ~: foldExpr id rango (+) (-) (*) (/) (Suma (Rango 1 5) (Rango 1 5))
        ~?= 8
    ]

testsEval :: Test
testsEval =
  test
    [ "Dado un expr Suma (Rango 1 5) (Const 1) con genFijo, cuando se aplica fst . eval, entonces se obtiene 4.0"
        ~: fst (eval (Suma (Rango 1 5) (Const 1)) genFijo)
        ~?= 4.0,
      "Dado un expr Suma (Rango 1 5) (Const 1) con genNormalConSemilla 0, cuando se aplica fst . eval, entonces se obtiene 3.7980492"
        ~: fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0))
        ~?= 3.7980492, -- el primer rango evalua a 2.7980492
      "Dado un expr Suma (Rango 1 5) (Rango 1 5) con genNormalConSemilla 0, cuando se aplica fst . eval, entonces se obtiene 5.92308"
        ~: fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0))
        ~?= 5.92308, -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      "Dado un expr Resta (Rango 1 5) (Rango 1 5) con genNormalConSemilla 0, cuando se aplica fst . eval, entonces se obtiene -0.32698154"
        ~: fst (eval (Resta (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0))
        ~?= -0.32698154,
      "Dado un expr Div (Rango 1 5) (Rango 1 5) con genNormalConSemilla 0, cuando se aplica fst . eval, entonces se obtiene 0.8953669192636"
        ~: fst (eval (Div (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0))
        ~?= 0.8953669192636,
      "Dado un expr Mult (Resta (Rango 1 5) (Rango 1 5)) (Rango 1 5) con genNormalConSemilla 0, cuando se aplica fst . eval, entonces se obtiene -1.7866315"
        ~: fst (eval (Mult (Resta (Rango 1 5) (Rango 1 5)) (Rango 1 5)) (genNormalConSemilla 0))
        ~?= -1.7866315, -- el primer rango evalua a 2.7980492, el segundo a 3.1250308 y el tercero a 5.464013
      "Dado un expr Mult (Resta (Rango (-10) 10) (Rango 100 1000) (Rango 1 5) con genNormalConSemilla 0, cuando se aplica fst . eval, entonces se obtiene -1.7866315"
        ~: fst (eval (Mult (Resta (Rango (-10) 10) (Rango 100 1000)) (Rango 1 5)) (genNormalConSemilla 0))
        ~?= -3164.4377, -- el primer rango evalua a -1.0097542, el segundo a 578.1319 y el tercero a 5.464013
        -- rango 1 5 recibe misma semilla 2
        
      "Dado tres rangos distintos con genNormalConSemilla 0, cuando se aplica eval en cadena, entonces se obtienen los valores esperados"
        ~: let  (x, gen)  = eval (Rango (-10) 10) (genNormalConSemilla 0)
                (y, gen') = eval (Rango 100 1000) gen
                (z, gen'')= eval (Rango 1 5) gen'
           in (x, y, z) ~?= (-1.0097542, 578.1319, 5.464013),

      "Dado tres rangos iguales con genNormalConSemilla 0, cuando se aplica eval en cadena, entonces se obtienen los valores esperados"
        ~: let  (x, gen)  = eval (Rango 1 5) (genNormalConSemilla 0)
                (y, gen') = eval (Rango 1 5) gen
                (z, gen'')= eval (Rango 1 5) gen'
           in (x, y, z) ~?= (2.7980492, 3.1250308, 5.464013)
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [ "Dado 1 casillero, 3 experimentos, funcion que use dameUno con generador Fijo, cuando se aplica armarHistograma, entonces se obtiene casillero único completo"
        ~: casilleros (fst (armarHistograma 1 3 (curry dameUno 1 5) genFijo))
        ~?= [ Casillero infinitoNegativo 2 0 0.0,
              Casillero 2 4 3 100.0, -- El 100% de los valores estan aca
              Casillero 4 infinitoPositivo 0 0.0
            ], -- el primer rango evalua a 3, el segundo a 3 y el tercero a 3
      "Dado 3 casilleros, 10 experimentos, funcion que use dameUno con generador Fijo, cuando se aplica armarHistograma, entonces se obtiene un casillero completo"
        ~: casilleros (fst (armarHistograma 3 10 (curry dameUno 1 5) genFijo))
        ~?= [ Casillero infinitoNegativo 2 0 0.0,
              Casillero 2 2.6666667 0 0.0,
              Casillero 2.6666667 3.3333335 10 100.0, -- El 100% de los valores estan aca
              Casillero 3.3333335 4 0 0.0,
              Casillero 4 infinitoPositivo 0 0.0
            ], -- el primer rango evalua a 3, el segundo a 3 y análogamente todos los experimentos
      "Dado 1 casillero, 3 experimentos, funcion que use dameUno con generador Normal con Semilla, cuando se aplica armarHistograma, entonces se obtiene "
        ~: casilleros (fst (armarHistograma 1 3 (curry dameUno 1 5) (genNormalConSemilla 0)))
        ~?= [ Casillero infinitoNegativo 1.4687741 0 0.0,
              Casillero 1.4687741 6.1226206 3 100.0, -- El 100% de los valores estan aca (pocas muestras)
              Casillero 6.1226206 infinitoPositivo 0 0.0
            ], -- el primer rango evalua a 2.7980492, el segundo a 3.1250308 y el tercero a 5.464013
      "Dado 1 casillero, 20 experimentos, funcion que use dameUno con generador Normal con Semilla, cuando se aplica armarHistograma, entonces se obtiene "
        ~: casilleros (fst (armarHistograma 1 20 (curry dameUno 1 5) (genNormalConSemilla 0)))
        ~?= [ Casillero infinitoNegativo 0.9621854 0 0.0,
              Casillero 0.9621854 5.279082 19 95.0,
              Casillero 5.279082 infinitoPositivo 1 5.0
            ]
    
    
      {- "Dado 1 casillero, 1000 experimentos, funcion que use dameUno, cuando se aplica armarHistograma, entonces se obtiene muestras bien distribuidas"
        ~: casilleros (fst (armarHistograma 1 1000 (curry dameUno 1 5) (genNormalConSemilla 0)))
        ~?= [ Casillero infinitoNegativo 1.0136857 25 2.5,
              Casillero 1.0136857 4.966351 955 95.5, -- El ~95% de los valores estan aca (bastantes muestras)
              Casillero 4.966351 infinitoPositivo 20 2.0
            ],
      "Dado 4 casilleros, 1000 experimentos, funcion que use dameUno, cuando se aplica armarHistograma, entonces vemos la distribución"
        ~: casilleros (fst (armarHistograma 4 1000 (curry dameUno 1 5) (genNormalConSemilla 0)))
        ~?= [ Casillero infinitoNegativo 1.0136857 25 2.5, -- El ~2.5% de los valores estan aca
              Casillero 1.0136857 2.001852 129 12.9,
              Casillero 2.001852 2.9900184 338 33.8,
              Casillero 2.9900184 3.9781847 347 34.7,
              Casillero 3.9781847 4.966351 141 14.1,
              Casillero 4.966351 infinitoPositivo 20 2.0 -- El ~2.5% de los valores estan aca
            ],
      "Dado 11 casilleros, 100000 experimentos, funcion que use dameUno, cuando se aplica armarHistograma, entonces vemos la distribución"
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
            ] -}
    {-  "Dado 11 casilleros, 100000 experimentos, funcion que use dameUno, cuando se aplica armarHistograma, entonces vemos la distribución"
        ~: casilleros (fst (armarHistograma 10 100000 (operadorBinarioGen (*) (curry dameUno 1 5) (curry dameUno 5 30)) (genNormalConSemilla 0)))
        ~?= [ Casillero infinitoNegativo 1.0097816 2464 2.464, -- El ~2.5% de los valores estan aca
              Casillero 0 0 0 0,
              Casillero 4.998564 infinitoPositivo 2545 2.545 -- El ~2.5% de los valores estan aca
            ]-}
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [""
     ~: casilleros (fst (evalHistograma 1 1000 (Mult (Rango 1 5) (Const 2)) (genNormalConSemilla 0)))
     ~?= [Casillero infinitoNegativo 2.0273714 25 2.5,Casillero 2.0273714 9.932702 955 95.5,Casillero 9.932702 infinitoPositivo 20 2.0]
     ]
     


{- Casos de test posibles:

- Considerar Expresiones, cantCasilleros, cantidadDeMuestras y generador

-- Expresión sin rango    
      -- Probarlo con un genFijo y un genConSemilla
      -- Cantidad de casilleros es indistinta

      -- Solamente un const 
      evalHistograma 1 5 (Const 2) genFijo
      evalHistograma 1 5 (Const 2) (genNormalConSemilla 0) 

      -- Cuenta con solo const 
      evalHistograma 7 5 (Suma (Const 2) (Const 10)) genFijo

-- Solamente un rango -> Deberia devolver un histograma como los de ArmarHistograma (agarrar un ejemplo de ahi) (aca testear tambien con varios casilleros)
evalHistograma 1 10 (Rango 1 5) (genNormalConSemilla 0)
evalHistograma 1 5  (Rango -10 10) (genNormalConSemilla 0)

-- De aca en adelante testear con 7 casilleros, 1 5 10 15 muestras 
-- Una multiplicacion entre rango y una constante usando genConSemilla 
evalHstograma 1 1 (Mult (Rango 1 5) (Const 2)) (genNormalconSemilla 0)
evalHstograma 1 10 (Mult (Rango 1 5) (Const 2)) (genNormalconSemilla 0)

-- Una suma 

-- Una resta 

-- Un div

-- Calculo muy largo (final) este (1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0? 


 -}

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
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"

  -- En el Test original estaba mal el caracter : esperaba \9618 pero obtenía \9617
  -- \9617 ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
  -- \9618 ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒
                  
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
