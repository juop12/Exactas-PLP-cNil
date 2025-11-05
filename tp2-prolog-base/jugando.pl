% Ejercicio 1

%! matriz(+F, +C, -M) es verdadero si M es una matriz de F filas y C columnas. 
%! Cuando M no está instanciada el predicado debe generar una matriz con variables no instanciadas en las celdas
matriz(0,C,[]).
matriz(F,C,[F1|Fs]) :- F > 0, C >= 0, Fp is F - 1, length(F1, C), matriz(Fp, C, Fs).

% Ejercicio 2

%! replicar(+Elem, +N, -Lista) es cierto cuando Lista es una lista de longitud N, 
%! donde cada elemento es igual a Elem.

% replicar(X, 0, []).
% replicar(X, N, [X|T]) :- N > 0, Np is N-1, replicar(X, Np, T).

replicar(X,N,L) :- length(L, N), maplist(=(X),L).


% Ejercicio 3

%! posicion(+I,+J,+M,-Elem) es True si M[I][J] == Elem, con I y J en rango de la matriz M.
posicion(I,J,M,Elem) :- nth1(I, M, Fila_iesima), nth1(J, Fila_iesima, Elem).

%! celda_traspuesta(+F,+C,+M,-Mt) es True si la celda (F,C) de M tiene el mismo elemento que la celda (C,F) de Mt.
celda_transpuesta(F,C,M,Mt) :- posicion(F,C,M,Elem), posicion(C,F,Mt,Elem).

%! chequearTransposicion(+F,+C,+M,-Mt) es True cuando todas las posiciones 0 <= i,j <= F,C cumplen que la celda (i,j) de M y la (j,i) de Mt coinciden.
chequearTransposicion(1,1,M,Mt) :- celda_transpuesta(1,1,M,Mt).
chequearTransposicion(1,C,M,Mt) :- C > 1, celda_transpuesta(1,C,M,Mt), Cp is C-1, chequearTransposicion(1,Cp,M,Mt).
chequearTransposicion(F,1,M,Mt) :- F > 1, celda_transpuesta(F,1,M,Mt), Fp is F-1, chequearTransposicion(Fp,1,M,Mt).
chequearTransposicion(F,C,M,Mt) :- F > 1, C > 1, celda_transpuesta(F,C,M,Mt), Fp is F-1, chequearTransposicion(Fp,C,M,Mt), Cp is C-1, chequearTransposicion(F,Cp,M,Mt).

%! transponer(+M, -MT) es cierto cuando MT es la matriz transpuesta de M. La transpuesta,
%! MT, tiene como filas las columnas de M y viceversa. Asumir que M es una matriz bien formada (todas las filas tienen
%! la misma longitud)
transponer(M, Mt) :- length(M, F), nth1(1,M,F1), length(F1, C), chequearTransposicion(F,C,M,Mt).
