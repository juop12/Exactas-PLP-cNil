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

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros
      -- "Ej 7 - Expr.recrExpr" ~: testsRecr,
      -- "Ej 7 - Expr.foldExpr" ~: testsFold,
      -- "Ej 8 - Expr.eval" ~: testsEval,
      -- "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      -- "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      -- "Ej 11 - Expr.mostrar" ~: testsMostrar,
      -- "Expr.Parser.parse" ~: testsParse,
      -- "App.mostrarFloat" ~: testsMostrarFloat,
      -- "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 2 "" ~?= "  ", -- probando con una string vacio
      alinearDerecha (-2) "hola" ~?= "hola" -- longitud de "hola" >= (-2) entonces devuelve "hola". Es decir, cada vez que se ponga un indice negativo devuelve el string original
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 3 (+ 10) [1, 2, 3] ~?= [1, 2, 3], -- probando con un indice mayor a la longitud de la lista
      actualizarElem (-1) (+ 10) [1, 2, 3] ~?= [1, 2, 3], -- probando con un indice negativo
      actualizarElem 3 ('t' :) ["s", "t", "r", "i", "n", "g"] ~?= ["s", "t", "r", "ti", "n", "g"] -- probando con una lista de strings para probar con otros tipos
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 2 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 5 0 0,
              Casillero 5 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ]
      --completar
    ]

testsAgregar :: Test
testsAgregar =
  let h3 = vacio 1 (0, 2)
      h0 = vacio 3 (0, 6)
   in test
        [ casilleros (agregar 0 h3)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 infinitoPositivo 0 0
                ],
          casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ]
        ]

    -- "nombre" ~: test ~?= valor,
testsHistograma :: Test
testsHistograma =
  test
    [ 
      "Sin datos" ~: histograma 4 (1, 5) [] ~?= vacio 4 (1, 5),
      "Intervalos con extremos positivos" ~: histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      "Intervalos con extremos negativos" ~: histograma 4 (-10, -3) [-10, -7, -2] ~?= agregar (-10) (agregar (-7) (agregar (-2) (vacio 4 (-10, -3)))),
      "Intervalos con extremos de distinto signo" ~: histograma 4 (-10, 5) [-10, -7, 2] ~?= agregar (-10) (agregar (-7) (agregar 2 (vacio 4 (-10, 5)))),
      "Intervalos con extremos decimales" ~: histograma 4 (1.5, 5.5) [1.5, 2.5, 3.5] ~?= agregar 3.5 (agregar 2.5 (agregar 1.5 (vacio 4 (1.5, 5.5)))),
      -- "Intervalos con extremos iguales" ~: histograma 4 (1, 1) [1, 2, 4] ~?= agregar 4 (agregar 2 (agregar 1 (vacio 4 (1, 1)))), -- ! esto deberia dar error porque no puede ser un rango nulo
      "Datos fuera del rango" ~: histograma 4 (1, 5) [-10, 0, 6, 10] ~?= agregar (-10) (agregar 0 (agregar 6 (agregar 10 (vacio 4 (1, 5))))),
      "Datos en el mismo casillero" ~: histograma 4 (1, 5) [1, 1.5, 1.8] ~?= agregar 1.8 (agregar 1.5 (agregar 1 (vacio 4 (1, 5)))),
      "Todos mis datos son el mismo" ~: histograma 4 (1, 5) [3, 3, 3, 3] ~?= agregar 3 (agregar 3 (agregar 3 (agregar 3 (vacio 4 (1, 5)))))      
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ "Histograma vacio casillero único tam 1" ~: casilleros (vacio 1 (0, 1))
        ~?= [ Casillero infinitoNegativo  0.0              0   0.0,
              Casillero 0.0               1.0              0   0.0,
              Casillero 1.0               infinitoPositivo 0   0.0
            ],"Histograma vacio 1_elem tam_1" ~: 
      "Histograma vacio casillero único tam 2" ~: casilleros (vacio 1 (0, 2))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 infinitoPositivo 0 0.0
            ],
      "Histograma vacio casillero único inicio 1" ~: casilleros (vacio 1 (1, 2))
        ~?= [ Casillero infinitoNegativo 1.0 0 0.0,
              Casillero 1.0 2.0 0 0.0,
              Casillero 2.0 infinitoPositivo 0 0.0
            ],
      "Histograma inicio negativo" ~: casilleros (vacio 2 ((-10), 10))
        ~?= [ Casillero infinitoNegativo (-10.0) 0 0.0,
              Casillero (-10.0)   0     0 0.0,
              Casillero 0.0       10.0  0 0.0,
              Casillero 10.0      infinitoPositivo 0 0.0
            ],
      "Histograma vacio _elem tam_1 inicio parametrizado" ~: casilleros (vacio 2 (0, 1))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 0.5 0 0.0,
              Casillero 0.5 1.0 0 0.0,
              Casillero 1.0 infinitoPositivo 0 0.0
            ],
      "Histograma casilleros parametrizados" ~: casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Agregar un elemento" ~: casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Agregar mismo elemento repetidamente" ~: casilleros (agregar 2 (agregar 2 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 2 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Agregar elementos distintos" ~: casilleros (agregar 1 (agregar 2 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 1 50.0,
              Casillero 2.0 4.0 1 50.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      "Agregar fuera de rango inferior" ~: casilleros (agregar (-1) (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 1 100.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
     "Agregar fuera de rango superior" ~: casilleros (agregar 100 (agregar 10 (vacio 3 (0, 6))))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 1 100.0
            ]
    ]

testsRecr :: Test
testsRecr =
  test
    [ completar
    ]

testsFold :: Test
testsFold =
  test
    [ completar
    ]

testsEval :: Test
testsEval =
  test
    [ fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
      fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
      fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
      completar
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [completar]

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
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
