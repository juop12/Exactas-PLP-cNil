% Ejercicio 1

%! matriz(+F, +C, -M) 
% Es verdadero si M es una matriz de F filas y C columnas. 
% Cuando M no está instanciada el predicado debe generar una matriz con variables no instanciadas en las celdas
% Se asume F y C 
matriz(F,C,M):- 
	length(M, F),
	length(Cs, F),
	maplist(=(C), Cs), 
	maplist(length, M, Cs). 

% Ejercicio 2

%! replicar(+Elem, +N, -Lista)* 
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
	
% Ejercicio 5

%! resolverNaive(+NN)
% Resuelve un nonograma NN usando backtracking, utilizando pintadas válidas como auxiliar.
% Asume que ya es un nonograma valido -> Matriz y restricciones bien formadas.
resolverNaive(nono(_,Restricciones)) :- maplist(pintadasValidas, Restricciones). 

% Ejercicio 6

%! combinar(+Combinaciones, +Lista).
% Es verdadero cuando Lista es la lista donde cada posición está instanciada sii esa posición es igual en todas las listas de Combinaciones. 
combinar([L],L).
combinar([C1,C2|P], L) :- 
	maplist(combinarCelda, C1, C2, Lc), % Lc es la combinación entre C1 y C2
	combinar([Lc|P], L).		    	% L es la combinación entre Lc y el resto de las pintadas

%! pintarObligatorias(+R)
% Pinta las celdas que son obligatoriamente "x" o "o". Esto es viendo todas las posibilidades de pintadas válidas para la restricción R
pintarObligatorias(r(R,L)) :-
	findall(L, pintadasValidas(r(R,L)), ListaDePintadasValidas), % Conseguimos todas las formas válidas de pintar la L que me pasaron.
	combinar(ListaDePintadasValidas, L).					     % Veamos que L sea la combinación entre todas las formas de pintar válidas. 

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

%! hayUnaRestriccionConCantidadDeLibresMenorQue(+RS,+N)
% Es verdadero cuando existe una restricción en RS con alguna variable libre pero menor cantidad de variables libres que N. 
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
	cantidadVariablesLibres(R, N), 								% | Generar algún candidato con mis requisitos
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
	!,									%***
	pintadasValidas(R),
	resolverDeduciendo(NN).

% ***)  Si tengo más de una restricción, elijo una y, en caso de conseguir alguna solución del nono, no vuelvo atrás en mi decision. 
%		La razón? Hacer eso me evita obtener soluciones repetidas. Eventualmente las otras restricciones van a ser pintadas.

% Ejercicio 10

%! solucionUnica(+NN).
% Es verdadero cuando el nonograma NN tiene una única solucion.
solucionUnica(NN) :- 
 	findall(NN, resolverDeduciendo(NN), L), length(L,N), N =:= 1.

/*
Ejercicio 11- Análisis de Nonos 👴	
	| Completar la tabla con el analisis de los nonogramas predefinidos.
	| Indicar qué consultas se usaron para averiguar cada uno de los datos

Para que Prolog no acorte las listas al mostrarlas en consola hicimos la siguiente query:

	?- set_prolog_flag(answer_write_options, [max_depth(0)]).

Para completar la columna de dimensiones hicimos la siguiente query que nos da una lista de tuplas (ID, F, C)
donde ID es el número de nono, F es la cantidad de filas de la matriz del nono y C la cantidad de columnas.

	?- findall((ID,F,C), (between(0,14,ID), nn(ID, nono(M,_)), matriz(F,C,M)), L). 

Para completar la columna de "¿Tiene solución única?" hicimos la siguiente query que nos da una lista de los ID de los nonos
cuya solucion es única. Aquellos que no están deducimos que tienen más de una solución (ya sabemos que todos tienen solución). 

	?- findall(ID, (between(0,14,ID), nn(ID,NN), solucionUnica(NN)), L). 

Para completar la columna de "¿Es deducible sin backtracking?" hicimos la siguiente query que nos da una lista de los ID de los nonos
resolubles únicamente con la lógica de deducirVariasPasadas. Aquellos que no están, deducimos que no son deducibles (🦆) sin backtracking. 

	?- findall(ID, (between(0,14,ID), nn(ID, NN), nn(ID,NN2), deducirVariasPasadas(NN), resolverDeduciendo(NN2), NN2 == NN), L).

El resultado final es esta tabla:

======================================================================================
||	N	|| 	Tamaño	||	¿Tiene solución única?	||	¿Es deducible sin backtracking?	||
======================================================================================
||	0	|| 	2x3		||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	1	|| 	5x5		||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	2	|| 	5x5		||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	3	|| 	10x10	||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	4	|| 	5x5		||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	5	|| 	5x5		||			Si				||				No					||
--------------------------------------------------------------------------------------
||	6	|| 	5x5		||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	7	|| 	10x10	||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	8	|| 	10x10	||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	9	|| 	5x5		||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	10	|| 	5x5		||			No				||				No					||
--------------------------------------------------------------------------------------
||	11	|| 	10x10	||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	12	|| 	15x15	||			Si				||				Si					||
--------------------------------------------------------------------------------------
||	13	|| 	11x5	||			Si				||				No					||
--------------------------------------------------------------------------------------
||	14	|| 	4x4		||			Si				||				No					||
--------------------------------------------------------------------------------------

Ejercicio 12.
	| Indicar si el predicado replicar/3 es reversible en el segundo argumento.
	| En concreto se pide analizar si replicar(+Elem, -N, -Lista) funciona correctamente.

Recordemos cómo está implementado:

replicar(Elem,N,L) :- 
	length(L, N),		
	maplist(=(Elem),L).	

Si N está instanciada entonces length(L,N) se encarga de unificar a L con una lista de todos [los]? % TODO
elementos no instanciados de la longitud deseada y luego maplist(...) unifica cada elemento de L con Elem.

Si N no está instanciada entonces length(L,N) se encarga de generar todas las posibles listas no instanciadas de longitud >= 0 
e instancia en la variable L dichas listas y en la variable N dichas longitudes. 
Por cada posible L y N que length genere, maplist(...) unifica los elementos de esos L como en el caso anterior. 

Notar que la generación es infinita, entonces al querer usarlo dentro de otro predicado uno debería tener cuidado. 
Si desea una cantidad acotada de listas debería acotar la N o la L luego de llamar replicar(Elem, N, L).

Hicimos provecho de la reversibilidad en N en el ejercicio de pintadasValidas, por ejemplo. 

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                              %
%    Ejemplos de nonogramas    %
%        NO MODIFICAR          %
%    pero se pueden agregar    %
%                              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Fáciles
nn(0,  NN) :- armarNono([[1],[2]],[[],[2],[1]], NN).
nn(1,  NN) :- armarNono([[4],[2,1],[2,1],[1,1],[1]],[[4],[3],[1],[2],[3]], NN).
nn(2,  NN) :- armarNono([[4],[3,1],[1,1],[1],[1,1]],[[4],[2],[2],[1],[3,1]], NN).
nn(3,  NN) :- armarNono([[2,1],[4],[3,1],[3],[3,3],[2,1],[2,1],[4],[4,4],[4,2]], [[1,2,1],[1,1,2,2],[2,3],[1,3,3],[1,1,1,1],[2,1,1],[1,1,2],[2,1,1,2],[1,1,1],[1]], NN).
nn(4,  NN) :- armarNono([[1, 1], [5], [5], [3], [1]], [[2], [4], [4], [4], [2]], NN).
nn(5,  NN) :- armarNono([[], [1, 1], [], [1, 1], [3]], [[1], [1, 1], [1], [1, 1], [1]], NN).
nn(6,  NN) :- armarNono([[5], [1], [1], [1], [5]], [[1, 1], [2, 2], [1, 1, 1], [1, 1], [1, 1]], NN).
nn(7,  NN) :- armarNono([[1, 1], [4], [1, 3, 1], [5, 1], [3, 2], [4, 2], [5, 1], [6, 1], [2, 3, 2], [2, 6]], [[2, 1], [1, 2, 3], [9], [7, 1], [4, 5], [5], [4], [2, 1], [1, 2, 2], [4]], NN).
nn(8,  NN) :- armarNono([[5], [1, 1], [1, 1, 1], [5], [7], [8, 1], [1, 8], [1, 7], [2, 5], [7]], [[4], [2, 2, 2], [1, 4, 1], [1, 5, 1], [1, 8], [1, 7], [1, 7], [2, 6], [3], [3]], NN).
nn(9,  NN) :- armarNono([[4], [1, 3], [2, 2], [1, 1, 1], [3]], [[3], [1, 1, 1], [2, 2], [3, 1], [4]], NN).
nn(10, NN) :- armarNono([[1], [1], [1], [1, 1], [1, 1]], [[1, 1], [1, 1], [1], [1], [ 1]], NN).
nn(11, NN) :- armarNono([[1, 1, 1, 1], [3, 3], [1, 1], [1, 1, 1, 1], [8], [6], [10], [6], [2, 4, 2], [1, 1]], [[2, 1, 2], [4, 1, 1], [2, 4], [6], [5], [5], [6], [2, 4], [4, 1, 1], [2, 1, 2]], NN).
nn(12, NN) :- armarNono([[9], [1, 1, 1, 1], [10], [2, 1, 1], [1, 1, 1, 1], [1, 10], [1, 1, 1], [1, 1, 1], [1, 1, 1, 1, 1], [1, 9], [1, 2, 1, 1, 2], [2, 1, 1, 1, 1], [2, 1, 3, 1], [3, 1], [10]], [[], [9], [2, 2], [3, 1, 2], [1, 2, 1, 2], [3, 11], [1, 1, 1, 2, 1], [1, 1, 1, 1, 1, 1], [3, 1, 3, 1, 1], [1, 1, 1, 1, 1, 1], [1, 1, 1, 3, 1, 1], [3, 1, 1, 1, 1], [1, 1, 2, 1], [11], []], NN).
nn(13, NN) :- armarNono([[2], [1,1], [1,1], [1,1], [1], [], [2], [1,1], [1,1], [1,1], [1]], [[1], [1,3], [3,1,1], [1,1,3], [3]], NN).
nn(14, NN) :- armarNono([[1,1], [1,1], [1,1], [2]], [[2], [1,1], [1,1], [1,1]], NN).

% Buscamos una imagen de un CNil en https://www.nonograms.org/nonograms/i/77295
% Un hombre mayor
nn(15, NN) :- armarNono(
	[[3,2],[2,2],[1,2],[1,1],[3,1,1,3],[1,1,1,2,1,1,1],[1,1,1,1,1,1],[2,2],[1,1,1,1],[1,6,1],[1,1],[3,3],[12]], %filas
	[[3],[1,1,1],[8,2],[1,3],[2,1,1,2],[1,1,1,2,1],[1,1,1,1],[1,1,1],[1,1,1,2,1],[2,1,1,2],[2,3],[8,2],[1,1,1],[3]], %columnas
	NN). 

% Un nonograma de 25x20 de un Mapache, pero está invertido (los colores), queda como tarea pensar cómo invertir nonogramas, o usar modo claro para apreciarlo .
nn(16, NN) :- armarNono(
	[[4,4],[2,2,8,2,2],[3,6,7,3],[2,7,1,7],[10,6],[6,1,4],[4,1,1,1,2],[3,7,2,4],[1,5,8,3],[8,9,4],[12,10],[11,3,6],[11,2,3],[7,2],[4,5],[1,1,5,1],[3,2,3,2],[5,3,1,2],[6,3,2],[7]], %filas
	[[2],[2,1,1,2],[3,3,4,2],[7,4,3],[1,5,6,2],[2,2,7,3],[5,8,3],[4,8,2],[3,8,2],[4,1,5,1],[4,1,4,1],[4,6,1,1],[1,2,6,2],[2,3,1],[3,1,3,2],[2,6,2,1],[3,6,3,1],[3,10],[3,5,4,1],[4,1,3,2,1],[2,3,1,2,1],[1,4,6,2],[6,5,2],[3,2,5],[1,1,1]], %columnas
	NN).


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
