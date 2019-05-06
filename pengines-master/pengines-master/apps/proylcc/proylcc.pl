:- module(proylcc,
	[
		emptyBoard/1,
		goMove/4,
		eliminar/3,
		obtenerContenido/3
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
		devolverCapturadas(TableroAux,Adyacentes,Player,Capturadas),    %Devuelve las capturadas por la ficha que puse recien
		Capturadas\=[],       %Si no es vacio, capture algunas fichas
		eliminarCapturadas(TableroAux,Capturadas,RBoard).

goMove(Board, Player, [R,C], RBoard):-
    replace(Row, R, NRow, Board, TableroAux), replace("-", C, Player, Row, NRow),
		getAdyacentes([R,C],Adyacentes),
		devolverCapturadas(TableroAux,Adyacentes,Player,Capturadas),
		Capturadas=[],     % Si no capture niguna ficha, me fijo si no es suicidio
		not(capturada(TableroAux,[R,C],Player,[],_CapturadasMias)),   %Si no estoy capturado, no es suicidio, coloco la ficha y listo
		RBoard=TableroAux.


devolverCapturadas(_Board,[],_Color,[]).

devolverCapturadas(Board,[P|Ps],Color,Capturadas):-
		getColorContrario(Color,CC),
		capturada(Board,P,CC,[],Caps1),
		devolverCapturadas(Board,Ps,Color,Caps2),
		unir(Caps1,Caps2,Capturadas).

devolverCapturadas(Board,[_P|Ps],Color,Capturadas):-
		devolverCapturadas(Board,Ps,Color,Capturadas).

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
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
		XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).

%%C es el color de pos, puedo pasar el color contrario para verificar las nulas tmb
%para ver si la pos esta capturadad por ese color.

unir([],Ys,Ys).
unir([X|Xs],Ys,Zs):- member(X,Ys), unir(Xs,Ys,Zs).
unir([X|Xs],Ys,Zs):- not(member(X,Ys)), unir(Xs,Ys,Z1), Zs=[X|Z1].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% capturada(+Board, +Pos, +Color, +Visitados).
%
eliminarCapturadas(Board,[],Board).
eliminarCapturadas(Board,[Pos|Capturadas],NBoard):-
		eliminarCapturadas(Board,Capturadas,BoardAux), eliminar(BoardAux,Pos,NBoard).

capturada(Board,Pos,Color,Visitados,Capturadas):-
		getAdyacentes(Pos,Adyacentes), getColorContrario(Color,ColorContrario),
		analizarAdyacentes(Board,Adyacentes,[Pos|Visitados],ColorContrario,Capts),
		Capturadas=[Pos|Capts].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% analizarAdyacentes(+Board, +Adyacentes, +Visitados, +ColorContrario).
%
% Verifica que todos los miembros de la lista Adyacentes cumplan con la
% condicion de captura.

analizarAdyacentes(_Board,[],_Visit,_CC,[]).

analizarAdyacentes(Board,[X|Xs],Visitados,CC,Capturadas):-
		condicionDeCaptura(Board,X,Visitados,CC,Capt1),
		analizarAdyacentes(Board,Xs,[X|Visitados],CC,Capt2),
		unir(Capt1,Capt2,Capturadas).  %%%%%%%%%%%%%%%%%%%%TEMPORAL, SIRVE???? ME DEVUELVEN POS REPETIDAS CAPT1 Y CAPT2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% condicionDeCaptura(+Board, +Pos, +Visitados, +ColorContrario, -Capturadas).
%
% Verifica si la ficha en la posicion Pos está capturada.

condicionDeCaptura(Board,Pos,_Visitados,CC,[]):-
		obtenerContenido(Board,Pos,CC).

condicionDeCaptura(Board,Pos,Visitados,CC,[]):-
		member(Pos,Visitados), getColorContrario(CC,Color),
		obtenerContenido(Board,Pos,Color).

condicionDeCaptura(Board,Pos,Visitados,CC,Capturadas):-
		not(member(Pos,Visitados)), getColorContrario(CC,Color),
		obtenerContenido(Board,Pos,Color), capturada(Board,Pos,Color,Visitados,Capturadas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtenerContenido(+Board, +Pos, -Contenido).
%

obtenerContenido(Board, [R,C], Contenido):-
    replace(Row, R, NRow, Board, _RBoard), replace(Contenido, C, Contenido, Row, NRow).

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
