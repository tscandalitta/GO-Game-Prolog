:- module(proylcc,
	[
		emptyBoard/1,
		goMove/4,
		getNulasCapturadas/3
	]).

	emptyBoard([
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
		 	 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"],
			 ["-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-","-"]
			 ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% goMove(+Board, +Player, +Pos, -RBoard)
%
% RBoard es la configuración resultante de reflejar la movida del jugador Player
% en la posición Pos a partir de la configuración Board.

goMove(Board, Player, [R,C], RBoard):-
		replace(Row, R, NRow, Board, TableroAux), replace("-", C, Player, Row, NRow),
		getAdyacentes([R,C],Adyacentes),
		getColorContrario(Player,CC),
		devolverCapturadas(TableroAux,Adyacentes,Player,CC,Capturadas),    %Devuelve las capturadas por la ficha que puse recien
		Capturadas\=[],       %Si no es vacio, capture algunas fichas
		eliminarCapturadas(TableroAux,Capturadas,RBoard).

goMove(Board, Player, [R,C], RBoard):-
    replace(Row, R, NRow, Board, TableroAux), replace("-", C, Player, Row, NRow),
		getAdyacentes([R,C],Adyacentes),
		getColorContrario(Player,CC),
		devolverCapturadas(TableroAux,Adyacentes,Player,CC,Capturadas),
		Capturadas=[],     % Si no capture niguna ficha, me fijo si no es suicidio
		not(capturada(TableroAux,[R,C],Player,CC,[],_CapturadasMias)),   %Si no estoy capturado, no es suicidio, coloco la ficha y listo
		RBoard=TableroAux.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% devolverCapturadas(+Board, +Adyacentes, +Color, +ColorContrario, -Capturadas).
%
% Verifica si cada una de las fichas en la lista de Adyacentes está capturada.
% Capturadas es el resultado de la union de los conjuntos de c/u de las fichas capturadas.

devolverCapturadas(_Board,[],_Color,_CC,[]).

devolverCapturadas(Board,[P|Ps],Color,CC,Capturadas):-
		obtenerContenido(Board,P,CC),
		capturada(Board,P,CC,Color,[],Caps1),
		devolverCapturadas(Board,Ps,Color,CC,Caps2),
		unir(Caps1,Caps2,Capturadas).

devolverCapturadas(Board,[_P|Ps],Color,CC,Capturadas):-
		devolverCapturadas(Board,Ps,Color,CC,Capturadas).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminar(+Board, +Pos, -RBoard)
%
% RBoard es la configuración resultante de eliminar la ficha en la posicion Pos
% a partir de la configuración Board.

eliminar(Board, [R,C], RBoard):-
    replace(Row, R, NRow, Board, RBoard), replace(_, C, "-", Row, NRow).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminarCapturadas(+Board, +Capturadas, -RBoard)
%
% RBoard es la configuración resultante de eliminar todas las fichas contenidas en la
% lista Capturadas a partir de la configuración Board.

eliminarCapturadas(Board,[],Board).

eliminarCapturadas(Board,[Pos|Capturadas],NBoard):-
		eliminarCapturadas(Board,Capturadas,BoardAux),
		eliminar(BoardAux,Pos,NBoard).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
		XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% capturada(+Board, +Pos, +Color, +Visitados, -Capturadas).
%
% Verifica si la ficha de color Color en la posicion Pos del tablero Board está capturada por
% ColorContrario y devuelve el conjunto capturado en Capturadas.

capturada(Board,Pos,Color,ColorContrario,Visitados,Capturadas):-
		obtenerContenido(Board,Pos,Color),
		getAdyacentes(Pos,Adyacentes),
		analizarAdyacentes(Board,Adyacentes,[Pos|Visitados],Color,ColorContrario,Capts),
		Capturadas=[Pos|Capts].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% analizarAdyacentes(+Board, +Adyacentes, +Visitados, +ColorContrario).
%
% Verifica que todos los miembros de la lista Adyacentes cumplan con la
% condicion de captura.

analizarAdyacentes(_Board,[],_Visit,_Color,_CC,[]).

analizarAdyacentes(Board,[X|Xs],Visitados,Color,CC,Capturadas):-
		condicionDeCaptura(Board,X,Visitados,Color,CC,Capt1),       %Capts1 contiene las posiciones capturadas que se verificaron para X
		diferencia(Xs,Capt1,XsAux),                                % Elimino de Xs todas las que ya verifique antes, que estan en Capt1  CLAVE MAGICA
		analizarAdyacentes(Board,XsAux,Visitados,Color,CC,Capt2),
		unir(Capt1,Capt2,Capturadas).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% condicionDeCaptura(+Board, +Pos, +Visitados, +Color, +ColorContrario, -Capturadas).
%
% Verifica si la ficha en la posicion Pos del tablero Board cumple con la condición de captura.

condicionDeCaptura(Board,Pos,_Visitados,_Color,CC,[]):-
		obtenerContenido(Board,Pos,CC).

condicionDeCaptura(Board,Pos,Visitados,Color,_CC,[]):-
		member(Pos,Visitados),
		obtenerContenido(Board,Pos,Color).

condicionDeCaptura(Board,Pos,Visitados,Color,CC,Capturadas):-
		not(member(Pos,Visitados)),
		obtenerContenido(Board,Pos,Color),
		capturada(Board,Pos,Color,CC,Visitados,Capturadas).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtenerContenido(+Board, +Pos, -Contenido).
%
% Devuelve el contenido de la posición Pos en Board.

obtenerContenido(Board, [R,C], Contenido):-
    replace(Row, R, NRow, Board, _RBoard),
		replace(Contenido, C, Contenido, Row, NRow).


crearListaNulas(Board,ListaNulas,Color):- recorrerMatriz(Board,0,0,ListaNulas,Color,Board).

recorrerMatriz([],_F,_C,[],_Color,_Board).
recorrerMatriz([X|Xs],F,C,Lista,Color,Board):-
		recorrerLista(Board,X,F,C,Lista1,Color), F1 is F+1,
		recorrerMatriz(Xs,F1,C,Lista2,Color,Board), append(Lista1,Lista2,Lista).

recorrerLista(_Board,[],_,_,[],_Color).

recorrerLista(Board,[X|Xs],F,C,[[F,C]|ListaAux],Color):-
		X="-",
		getAdyacentes([F,C],Adyacentes),
		hayAlgunAdyacenteDeColor(Board,Adyacentes,Color),
		C1 is C+1,
		recorrerLista(Board,Xs,F,C1,ListaAux,Color).

recorrerLista(Board,[_X|Xs],F,C,ListaNulas,Color):-
		C1 is C+1,
		recorrerLista(Board,Xs,F,C1,ListaNulas,Color).

hayAlgunAdyacenteDeColor(Board,[X|Xs],Color):-
		obtenerContenido(Board,X,Color);
		hayAlgunAdyacenteDeColor(Board,Xs,Color).

getNulasCapturadas(Board,CapsNegras,CapsBlancas):-
		crearListaNulas(Board,ListaNulasBlancas,"w"),
		crearListaNulas(Board,ListaNulasNegras,"b"),
		getCapturadasPor(Board,ListaNulasNegras,"b",CapsNegras),
		getCapturadasPor(Board,ListaNulasBlancas,"w",CapsBlancas).

getCapturadasPor(_Board,[],_Color,[]).

getCapturadasPor(Board,[Pos|ListaNulas],Color,Capturadas):-
		capturada(Board,Pos,"-",Color,[],Caps1),
		diferencia(ListaNulas,Caps1,ListaAux),                 %%%%%%%%%%%%%%%  CLAVE MAGICA
		getCapturadasPor(Board,ListaAux,Color,Caps2),
		append(Caps1,Caps2,Capturadas).

getCapturadasPor(Board,[_Pos|ListaNulas],Color,Capturadas):-
		getCapturadasPor(Board,ListaNulas,Color,Capturadas).

diferencia([],_,[]).
diferencia([X|Xs],Ys,Zs):- member(X,Ys), diferencia(Xs,Ys,Zs).
diferencia([X|Xs],Ys,Zs):- not(member(X,Ys)), diferencia(Xs,Ys,Z1), Zs=[X|Z1].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getColorContrario(?Color, ?ColorContrario).
%

getColorContrario("w","b").
getColorContrario("b","w").


obtenerConjunto(Board,Pos,Color,[Pos|Conjunto]):-
		getAdyacentes(Pos,Adyacentes),
		sonDeMiColor(Board,Adyacentes,Color,AdyacentesColor),
		verAdyacentesMiColor(Board,AdyacentesColor,[Pos],Color,Conjunto).

verAdyacentesMiColor(_Board,[],_Visitados,_Color,[]).

verAdyacentesMiColor(Board,[X|Xs],Visitados,Color,Conjunto):-
		not(member(X,Visitados)),
		getAdyacentes(X,Adyacentes),

		sonDeMiColor(Board,Adyacentes,Color,AdyacentesColor),
		unir(Xs,AdyacentesNoVisitados,ListaARecorrer),
		diferencia(AdyacentesColor,Visitados,AdyacentesNoVisitados),
		verAdyacentesMiColor(Board,ListaARecorrer,[X|Visitados],Color,Conj1),
		Conjunto=[X|Conj1].

verAdyacentesMiColor(Board,[X|Xs],Visitados,Color,Conjunto):-
		%%%%%%%%%    CORREGIR ACA FLACO! FIJATE QUE ESTAS METIENDO LOS MISMOS %%%%%%%%%
		verAdyacentesMiColor(Board,Xs,[X|Visitados],Color,Conjunto).


sonDeMiColor(_Board,[],_Color,[]).

sonDeMiColor(Board,[X|Xs],Color,Adyacentes):-
		obtenerContenido(Board,X,Color1),
		Color=Color1,
		sonDeMiColor(Board,Xs,Color,Conj1),
		Adyacentes=[X|Conj1].

sonDeMiColor(Board,[_X|Xs],Color,Adyacentes):-
		sonDeMiColor(Board,Xs,Color,Adyacentes).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getAdyacentes(+Pos, -Adyacentes).
%
% Adyacentes contiene todas las posiciones adyacentes a Pos.

getAdyacentes([R,C],Adyacentes):-
	R1 is R-1,
	R2 is R+1,
	C1 is C-1,
	C2 is C+1,
	getPosicion([R,C1],Pos1,18),
	getPosicion([R,C2],Pos2,18),
	getPosicion([R1,C],Pos3,18),
	getPosicion([R2,C],Pos4,18),
	AdyacentesAux=[Pos1,Pos2,Pos3,Pos4],
	borrarPosVacias(AdyacentesAux,Adyacentes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getPosicion(+Pos, -Posicion).
%
% Si Pos está dentro de los límites del tablero, Posicion es Pos.
% Caso contrario, Posicion es [].

getPosicion([R,C],[],Limite):-
	R<0;
	R>Limite;
	C<0;
	C>Limite.

getPosicion([R,C],[R,C],Limite):-
	R>=0,
	R=<Limite,
	C>=0,
	C=<Limite.

borrarPosVacias([],[]).

borrarPosVacias([[]|Xs],Ys):-
		borrarPosVacias(Xs,Ys).

borrarPosVacias([P|Xs],[P|Ys]):-
		P\=[],
		borrarPosVacias(Xs,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% unir(+Xs, +Ys, -Zs).
%
% Zs es el resultado de unir los conjuntos Xs e Ys.

unir([],Ys,Ys).

unir([X|Xs],Ys,Zs):-
    member(X,Ys), unir(Xs,Ys,Zs).

unir([X|Xs],Ys,[X|Z1]):-
    not(member(X,Ys)), unir(Xs,Ys,Z1).
