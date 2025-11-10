% Ejercicio 1

%! matriz(+F, +C, -M) 
% Es verdadero si M es una matriz de F filas y C columnas. 
% Cuando M no está instanciada el predicado debe generar una matriz con variables no instanciadas en las celdas
% Se asume F y C 
matriz(F,C,[F1|T]):- 
	length([F1|T], F),
	length(F1,C),
	length(Cs, F),
	maplist(=(C), Cs), 
	maplist(length, [F1|T], Cs). 

% Ejercicio 2

%! replicar(+Elem, +N, -Lista) 
% Es cierto cuando Lista es una lista de longitud N, donde cada elemento unifica con Elem.
replicar(Elem,N,L) :- 
	length(L, N),		
	maplist(=(Elem),L).	

% Ejercicio 3

%! unirCabezasAFilas(-Cabezas, -Filas, +Matriz) 
% Es verdadero cuando los elementos de Cabezas son los primeros elementos de cada fila de Filas en la Matriz (i.e: la primera columna)
unirCabezasAFilas([],[],[]). 
unirCabezasAFilas([C|CT],[F|FT],[[C|F]|M]) :- unirCabezasAFilas(CT, FT, M).

%! transponer(+M, -MT) 
% Cierto cuando MT es la matriz transpuesta de M. La transpuesta, MT, tiene como filas las columnas de M y viceversa. 
% Asumir que M es una matriz bien formada (todas las filas tienen la misma longitud)
transponer([],[]).
transponer([[]|_], []).
transponer(M, [Mt1|Mst]) :- unirCabezasAFilas(Mt1, Ms, M), transponer(Ms, Mst).

% TODO: Verificar la instanciación

% Predicado dado armarNono/3
armarNono(RF, RC, nono(M, RS)) :-
	length(RF, F),
	length(RC, C),
	matriz(F, C, M),
	transponer(M, Mt),
	zipR(RF, M, RSFilas),
	zipR(RC, Mt, RSColumnas),
	append(RSFilas, RSColumnas, RS).

zipR([], [], []).
zipR([R|RT], [L|LT], [r(R,L)|T]) :- zipR(RT, LT, T).

% Ejercicio 4

%! pintadaParcial(+L, +R, ?Resto)
% Es verdadero cuando L = BloqueO ++ BloqueX ++ Resto, donde:
% - BloqueX es lista de "x's" longitud R 
% - BloqueO es una lista de todas "o's" (puede ser vacia)
% - Resto es una lista por pintar o parcialmente pintada. (de cualquier color)
pintadaParcial(L, R, Resto) :- 
	append(Prefijo,Sufijo, L),  		 
	replicar(o,_,Prefijo), 				 
	replicar(x,R,BloqueX), 				 
	append(BloqueX, Resto, Sufijo).

%! pintadasValidas(+R)
% Genera las posibles pintadas válidas para una restricción R.
% R será de la forma r(Res, Celdas) donde Res es una lista de enteros que representan las restricciones y
% Celdas es una lista de variables (parcialmente no instanciadas) que representan las celdas
pintadasValidas(r([],L)) :- 
	replicar(o,_,L).					  % Cuando no hay restricciones, L es toda blanca
	
pintadasValidas(r([R|[]], L)) :- 		
	pintadaParcial(L, R, Resto),
	pintadasValidas(r([],Resto)).		 % Resto esta bien pintada (como no hay restricciones, tiene que ser todo blanco)
		
pintadasValidas(r([R|RS], L)) :- 		 
	RS \= [], 					
	pintadaParcial(L,R,[o|Resto]),
	pintadasValidas(r(RS,Resto)).		 % Resto esta bien pintado sujeto a las demás restricciones  
	
% Ejercicio 5 (PREGUNTAR A BRIAN -> ¿Es esto backtracking?)

%! resolverNaive(+NN)
% Resuelve un nonograma NN usando backtracking, utilizando pintadas validas como auxiliar.
% Asume que ya es un nonograma valido -> Matriz y restricciones bien formadas.
resolverNaive(nono(_,Restricciones)) :- maplist(pintadasValidas, Restricciones). 

% Ejercicio 6

%! combinar(+Combinaciones, -Lista).
% Es verdadero cuando Lista es la lista donde cada posición está instanciada sii esa posición es igual en todas las listas de Combinaciones.
combinar([L],L).
combinar([P1,P2|P], L) :- 
	maplist(combinarCelda, P1, P2, Lp), % Lp es la combinacion entre P1 y P2
	combinar([Lp|P], L).		    	% L es la combinacion entre Lp y el resto de las pintadas

%! pintarObligatorias(+R)
% Pinta las celdas que son obligatoriamente "x" o "o". Esto es viendo todas las posibilidades de pintadas válidas para la restricción R
pintarObligatorias(r(R,L)) :-
	findall(L, pintadasValidas(r(R,L)), ListaDePintadasValidas), % Conseguimos todas las formas validas de pintar la L que me pasaron.
	combinar(ListaDePintadasValidas, L).					     % Veamos que L sea la combinacion entre todas las formas de pintar validas. 

% Predicado dado combinarCelda/3
combinarCelda(A, B, _) :- var(A), var(B).
combinarCelda(A, B, _) :- nonvar(A), var(B).
combinarCelda(A, B, _) :- var(A), nonvar(B).
combinarCelda(A, B, A) :- nonvar(A), nonvar(B), A = B.
combinarCelda(A, B, _) :- nonvar(A), nonvar(B), A \== B.

% Ejercicio 7

%! deducir1Pasada(+NN)
% Aplica el predicado pintarObligatorias/1 a todas las restricciones del nonograma NN.
deducir1Pasada(nono(_,R)) :- maplist(pintarObligatorias,R).

% Predicado dado
cantidadVariablesLibres(T, N) :- term_variables(T, LV), length(LV, N).

% Predicado dado
deducirVariasPasadas(NN) :-
	NN = nono(M,_),
	cantidadVariablesLibres(M, VI), % VI = cantidad de celdas sin instanciar en M en este punto
	deducir1Pasada(NN),
	cantidadVariablesLibres(M, VF), % VF = cantidad de celdas sin instanciar en M en este punto
	deducirVariasPasadasCont(NN, VI, VF).

% Predicado dado
deducirVariasPasadasCont(_, A, A). % Si VI = VF entonces no hubo más cambios y frenamos.
deducirVariasPasadasCont(NN, A, B) :- A =\= B, deducirVariasPasadas(NN).

% Ejercicio 8

%! hayUnaRestriccionConCantidadDeLibresMenorQue(+RS,-N)
% Es verdadero cuando existe una restriccion en RS con alguna variable libre pero menor cantidad de variables libres que N. 
hayUnaRestriccionConCantidadDeLibresMenorQue(RS, N):- 
	member(R2, RS),
	cantidadVariablesLibres(R2, N2),
	Nm1 is N - 1,
	between(1, Nm1, N2).

%! restriccionConMenosLibres(+NN, -R)
% Es verdadero cuando R es la restricción (o una de las restricciones) del nonograma NN que tiene la menor cantidad de celdas no instanciadas,
% pero que tenga al menos una celda no instanciada
restriccionConMenosLibres(nono(_,RS), R) :- 
	member(R, RS),												% | 
	cantidadVariablesLibres(R, N), 								% | Generar algun candidato con mis requisitos
	N > 0, 														% |
	not(hayUnaRestriccionConCantidadDeLibresMenorQue(RS,N)).	% 	Testear que sea de los que tienen "menos libres"

% Ejercicio 9

%! resolverDeduciendo(+NN)
% Resuelve un nonograma NN de manera más eficiente que resolverNaive/1.
resolverDeduciendo(NN):- 
	deducirVariasPasadas(NN), 
	cantidadVariablesLibres(NN, 0).
resolverDeduciendo(NN):-
	deducirVariasPasadas(NN),
	restriccionConMenosLibres(NN,R),
	!,
	pintadasValidas(R),
	resolverDeduciendo(NN).

% Ejercicio 10

solucionUnica(NN) :- 
	not((findall(NN, resolverDeduciendo(NN), L), length(L,N), N =\= 1)).

% solucionUnica(NN) :- 
% 	findall(NN, resolverDeduciendo(NN), L), length(L,N), N =:= 1.

% solucionUnica(NN):- nn(ID, NN), resolverDeduciendo(NN), !, not((nn(ID, NN2), resolverDeduciendo(NN2), NN2 \= NN)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Ejemplos de nonogramas    %
%        NO MODIFICAR          %
%    pero se pueden agregar    %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fáciles
nn(0,  NN) :- armarNono([[1],[2]],[[],[2],[1]], NN). % SI
nn(1,  NN) :- armarNono([[4],[2,1],[2,1],[1,1],[1]],[[4],[3],[1],[2],[3]], NN). % SI
nn(2,  NN) :- armarNono([[4],[3,1],[1,1],[1],[1,1]],[[4],[2],[2],[1],[3,1]], NN). % SI
nn(3,  NN) :- armarNono([[2,1],[4],[3,1],[3],[3,3],[2,1],[2,1],[4],[4,4],[4,2]], [[1,2,1],[1,1,2,2],[2,3],[1,3,3],[1,1,1,1],[2,1,1],[1,1,2],[2,1,1,2],[1,1,1],[1]], NN).
 %SI
nn(4,  NN) :- armarNono([[1, 1], [5], [5], [3], [1]], [[2], [4], [4], [4], [2]], NN).%SI
nn(5,  NN) :- armarNono([[], [1, 1], [], [1, 1], [3]], [[1], [1, 1], [1], [1, 1], [1]], NN).%NO
nn(6,  NN) :- armarNono([[5], [1], [1], [1], [5]], [[1, 1], [2, 2], [1, 1, 1], [1, 1], [1, 1]], NN).%SI
nn(7,  NN) :- armarNono([[1, 1], [4], [1, 3, 1], [5, 1], [3, 2], [4, 2], [5, 1], [6, 1], [2, 3, 2], [2, 6]], [[2, 1], [1, 2, 3], [9], [7, 1], [4, 5], [5], [4], [2, 1], [1, 2, 2], [4]], NN).
%SI
nn(8,  NN) :- armarNono([[5], [1, 1], [1, 1, 1], [5], [7], [8, 1], [1, 8], [1, 7], [2, 5], [7]], [[4], [2, 2, 2], [1, 4, 1], [1, 5, 1], [1, 8], [1, 7], [1, 7], [2, 6], [3], [3]], NN).
%SI
nn(9,  NN) :- armarNono([[4], [1, 3], [2, 2], [1, 1, 1], [3]], [[3], [1, 1, 1], [2, 2], [3, 1], [4]], NN). %SI
nn(10, NN) :- armarNono([[1], [1], [1], [1, 1], [1, 1]], [[1, 1], [1, 1], [1], [1], [ 1]], NN).% Tiene varias soluciones , %NO
nn(11, NN) :- armarNono([[1, 1, 1, 1], [3, 3], [1, 1], [1, 1, 1, 1], [8], [6], [10], [6], [2, 4, 2], [1, 1]], [[2, 1, 2], [4, 1, 1], [2, 4], [6], [5], [5], [6], [2, 4], [4, 1, 1], [2, 1, 2]], NN). 
%SI
nn(12, NN) :- armarNono([[9], [1, 1, 1, 1], [10], [2, 1, 1], [1, 1, 1, 1], [1, 10], [1, 1, 1], [1, 1, 1], [1, 1, 1, 1, 1], [1, 9], [1, 2, 1, 1, 2], [2, 1, 1, 1, 1], [2, 1, 3, 1], [3, 1], [10]], [[], [9], [2, 2], [3, 1, 2], [1, 2, 1, 2], [3, 11], [1, 1, 1, 2, 1], [1, 1, 1, 1, 1, 1], [3, 1, 3, 1, 1], [1, 1, 1, 1, 1, 1], [1, 1, 1, 3, 1, 1], [3, 1, 1, 1, 1], [1, 1, 2, 1], [11], []], NN).
nn(13, NN) :- armarNono([[2], [1,1], [1,1], [1,1], [1], [], [2], [1,1], [1,1], [1,1], [1]], [[1], [1,3], [3,1,1], [1,1,3], [3]], NN).
%NO
nn(14, NN) :- armarNono([[1,1], [1,1], [1,1], [2]], [[2], [1,1], [1,1], [1,1]], NN).
%NO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Predicados auxiliares     %
%        NO MODIFICAR          %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! completar(+S)
%
% Indica que se debe completar el predicado. Siempre falla.
completar(S) :- write("COMPLETAR: "), write(S), nl, fail.

%! mostrarNono(+NN)
%
% Muestra una estructura nono(...) en pantalla
% Las celdas x (pintadas) se muestran como ██.
% Las o (no pintasdas) se muestran como ░░.
% Las no instanciadas se muestran como ¿?.
mostrarNono(nono(M,_)) :- mostrarMatriz(M).

%! mostrarMatriz(+M)
%
% Muestra una matriz. Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarMatriz(M) :-
	M = [F|_], length(F, Cols),
	mostrarBorde('╔',Cols,'╗'),
	maplist(mostrarFila, M),
	mostrarBorde('╚',Cols,'╝').

mostrarBorde(I,N,F) :-
	write(I),
	stringRepeat('══', N, S),
	write(S),
	write(F),
	nl.

stringRepeat(_, 0, '').
stringRepeat(Str, N, R) :- N > 0, Nm1 is N - 1, stringRepeat(Str, Nm1, Rm1), string_concat(Str, Rm1, R).

%! mostrarFila(+M)
%
% Muestra una lista (fila o columna). Solo funciona si las celdas
% son valores x, o, o no instanciados.
mostrarFila(Fila) :-
	write('║'),
	maplist(mostrarCelda, Fila),
	write('║'),
	nl.

mostrarCelda(C) :- nonvar(C), C = x, write('██').
mostrarCelda(C) :- nonvar(C), C = o, write('░░').
mostrarCelda(C) :- var(C), write('¿?').
