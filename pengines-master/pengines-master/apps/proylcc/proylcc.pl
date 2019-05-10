:- module(proylcc,
	[
		emptyBoard/1,
		goMove/4
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
% capturada(+Board, +Pos, +Color, +Visitados).
%
% Verifica si la ficha de color Color en la posicion Pos del tablero Board está capturada por ColorContrario.

capturada(Board,Pos,Color,ColorContrario,Visitados,Capturadas):-
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
		condicionDeCaptura(Board,X,Visitados,Color,CC,Capt1),
		analizarAdyacentes(Board,Xs,[X|Visitados],Color,CC,Capt2),
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
    replace(Row, R, NRow, Board, _RBoard), replace(Contenido, C, Contenido, Row, NRow).



crearListaNulas(Board,ListaNulas):-recorrerMatriz(Board,0,0,ListaNulas).

recorrerMatriz([],_,_,[]).
recorrerMatriz([X|Xs],F,C,Lista):-recorrerLista(X,F,C,Lista1),F1 is F+1 ,recorrerMatriz(Xs,F1,C,Lista2),append(Lista1,Lista2,Lista).

recorrerLista([],_,_,[]).
recorrerLista([X|Xs],F,C,[[F,C]|ListaAux]):-X="-", C1 is C+1, recorrerLista(Xs,F,C1,ListaAux).
recorrerLista([X|Xs],F,C,ListaNulas):-C1 is C+1,recorrerLista(Xs,F,C1,ListaNulas).

getNulasCapturadas(Board,CapturadasNegras,CapturadasBlancas):-
		crearListaNulas(Board,ListaNulas),
		findall(Pos,(member(Pos,ListaNulas),capturada(Board,Pos,"-","b",[],_Conj1)),CampturadasNegras),
		findall(Pos,(member(Pos,ListaNulas),capturada(Board,Pos,"-","w",[],_Conj)),CampturadasNegras).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getColorContrario(?Color, ?ColorContrario).
%

getColorContrario("w","b").
getColorContrario("b","w").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getAdyacentes(+Pos, -Adyacentes).
%
% Adyacentes contiene todas las posiciones adyacentes a Pos.

getAdyacentes([0,0],[[1,0],[0,1]]).
getAdyacentes([0,18],[[0,17],[1,18]]).
getAdyacentes([18,0],[[18,1],[17,0]]).
getAdyacentes([18,18],[[18,17],[17,18]]).
getAdyacentes([0,C],[[1,C],[0,C1],[0,C2]]):- C\=0, C1 is C-1, C2 is C+1.
getAdyacentes([18,C],[[17,C],[18,C1],[18,C2]]):- C\=18, C1 is C-1, C2 is C+1.
getAdyacentes([R,0],[[R,1],[R1,0],[R2,0]]):- R\=0, R1 is R-1, R2 is R+1.
getAdyacentes([R,18],[[R,17],[R1,18],[R2,18]]):- R\=18, R1 is R-1, R2 is R+1.
getAdyacentes([R,C],Adyacentes):-R\=0, C\=0,  R\=18, C\=18, R1 is R-1, R2 is R+1, C1 is C-1, C2 is C+1,
								Adyacentes=[[R1,C],[R2,C],[R,C1],[R,C2]].


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
