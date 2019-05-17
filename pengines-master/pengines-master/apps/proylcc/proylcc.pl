:- module(proylcc,
	[
		emptyBoard/1,
		goMove/4,
		getNulasCapturadas/3,
		moverInteligencia/2
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

sizeTablero(18).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROCEDIMIENTOS DE JUGADAS: MOVIMIENTOS Y ELIMINACIÓN %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% goMove(+Board, +Player, +Pos, -RBoard)
%
% RBoard es la configuración resultante de reflejar la movida del jugador Player
% en la posición Pos a partir de la configuración Board.

goMove(Board, Player, [R,C], RBoard):-
		replace(Row, R, NRow, Board, TableroAux), replace("-", C, Player, Row, NRow),
		getAdyacentes([R,C],Adyacentes),
		getColorContrario(Player,CC),
		devolverCapturadas(TableroAux,Adyacentes,Player,CC,Capturadas), % Devuelve las capturadas por la ficha que puse recien
		Capturadas\=[],       																					% Si no es vacio, capture algunas fichas
		eliminarCapturadas(TableroAux,Capturadas,RBoard).

goMove(Board, Player, [R,C], RBoard):-
    replace(Row, R, NRow, Board, TableroAux), replace("-", C, Player, Row, NRow),
		getAdyacentes([R,C],Adyacentes),
		getColorContrario(Player,CC),
		devolverCapturadas(TableroAux,Adyacentes,Player,CC,Capturadas),
		Capturadas=[],     																						  % Si no capture niguna ficha, me fijo si no es suicidio
		not(capturada(TableroAux,[R,C],Player,CC,[],_CapturadasMias)),  % Si no estoy capturado, no es suicidio, coloco la ficha y listo
		RBoard=TableroAux.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getColorContrario(?Color, ?ColorContrario).
%

getColorContrario("w","b").
getColorContrario("b","w").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% devolverCapturadas(+Board, +Adyacentes, +Color, +ColorContrario, -Capturadas).
%
% Para cada una de las posiciones en la lista Adyacentes verifica si dicha posicion
% está capturada por el Color y une los conjuntos capturados resultantes
% (si es que existen) en Capturadas.

devolverCapturadas(_Board,[],_Color,_ColorContrario,[]).

devolverCapturadas(Board,[Pos|Ps],Color,ColorContrario,Capturadas):-
		obtenerContenido(Board,Pos,ColorContrario),
		capturada(Board,Pos,ColorContrario,Color,[],Capts1),
		devolverCapturadas(Board,Ps,Color,ColorContrario,Capts2),
		unir(Capts1,Capts2,Capturadas).

devolverCapturadas(Board,[_Pos|Ps],Color,ColorContrario,Capturadas):-
		devolverCapturadas(Board,Ps,Color,ColorContrario,Capturadas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminar(+Board, +Pos, -RBoard)
%
% RBoard es la configuración resultante de eliminar la ficha en la posicion Pos
% a partir de la configuración Board.

eliminar(Board, [R,C], RBoard):-
    replace(Row, R, NRow, Board, RBoard), replace(_, C, "-", Row, NRow).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% eliminarCapturadas(+Board, +Capturadas, -RBoard)
%
% RBoard es la configuración resultante de eliminar todas las fichas contenidas
% en la lista Capturadas a partir de la configuración Board.

eliminarCapturadas(Board,[],Board).

eliminarCapturadas(Board,[Pos|Capturadas],NBoard):-
		eliminarCapturadas(Board,Capturadas,BoardAux),
		eliminar(BoardAux,Pos,NBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% replace(?X, +XIndex, +Y, +Xs, -XsY)
%

replace(X, 0, Y, [X|Xs], [Y|Xs]).

replace(X, XIndex, Y, [Xi|Xs], [Xi|XsY]):-
    XIndex > 0,
		XIndexS is XIndex - 1,
    replace(X, XIndexS, Y, Xs, XsY).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROCEDIMIENTOS DE VERIFICACIÓN DE CAPTURA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% capturada(+Board, +Pos, +Color, +Visitados, -Capturadas).
%
% Verifica si la ficha de color Color en la posicion Pos del tablero Board está
% capturada por ColorContrario y devuelve el conjunto capturado en Capturadas.

capturada(Board,Pos,Color,ColorContrario,Visitados,Capturadas):-
		obtenerContenido(Board,Pos,Color),
		getAdyacentes(Pos,Adyacentes),
		analizarAdyacentes(Board,Adyacentes,[Pos|Visitados],Color,ColorContrario,Capts),
		Capturadas=[Pos|Capts].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% analizarAdyacentes(+Board, +Adyacentes, +Visitados, +ColorContrario).
%
% Verifica que todas las posiciones de la lista Adyacentes cumplan con la
% condición de captura.

analizarAdyacentes(_Board,[],_Visitados,_Color,_ColorContrario,[]).

analizarAdyacentes(Board,[P|Adyacentes],Visitados,Color,ColorContrario,Capturadas):-
		condicionDeCaptura(Board,P,Visitados,Color,ColorContrario,Capts1),  % Capts1 contiene las posiciones capturadas que se verificaron para la captura de P
		diferencia(Adyacentes,Capts1,AdyacentesAux),                        % Elimino de Adyacentes todas las posiciones que ya se verificaron en Capts1, método más eficiente
		analizarAdyacentes(Board,AdyacentesAux,Visitados,Color,ColorContrario,Capts2),
		unir(Capts1,Capts2,Capturadas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% condicionDeCaptura(+Board, +Pos, +Visitados, +Color, +ColorContrario, -Capturadas).
%
% Verifica si la ficha en la posicion Pos cumple con alguna de las siguientes
% condiciones de captura:

% Pos es del color contrario.
condicionDeCaptura(Board,Pos,_Visitados,_Color,CC,[]):-
		obtenerContenido(Board,Pos,CC).

% Pos es del color Color y ya fue visitada.
condicionDeCaptura(Board,Pos,Visitados,Color,_CC,[]):-
		member(Pos,Visitados),
		obtenerContenido(Board,Pos,Color).

% Pos es del color Color, no fue visitada y está capturada.
condicionDeCaptura(Board,Pos,Visitados,Color,CC,Capturadas):-
		not(member(Pos,Visitados)),
		obtenerContenido(Board,Pos,Color),
		capturada(Board,Pos,Color,CC,Visitados,Capturadas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% obtenerContenido(+Board, +Pos, -Contenido).
%
% Devuelve el contenido de la posición Pos en Board.

obtenerContenido(Board, [R,C], Contenido):-
    replace(Row, R, NRow, Board, _RBoard),
		replace(Contenido, C, Contenido, Row, NRow).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROCEDIMIENTOS DE OBTENCION DE ADYACENTES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getAdyacentes(+Pos, -Adyacentes).
%
% Adyacentes contiene todas las posiciones adyacentes a Pos.

getAdyacentes([R,C],Adyacentes):-
		R1 is R-1,
		R2 is R+1,
		C1 is C-1,
		C2 is C+1,
		sizeTablero(Size),
		getPosicion([R,C1],Pos1,Size),
		getPosicion([R,C2],Pos2,Size),
		getPosicion([R1,C],Pos3,Size),
		getPosicion([R2,C],Pos4,Size),
		AdyacentesAux=[Pos1,Pos2,Pos3,Pos4],
		borrarPosVacias(AdyacentesAux,Adyacentes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getPosicion(+Pos, -Posicion).
%
% Si la posición Pos está dentro de los límites del tablero, Posicion es Pos.
% Caso contrario, Posicion es vacío.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% borrarPosVacias(+Adyacentes, -AdyacentesNoVacias).
%
% AdayenctesNoVacias contiene todas las posiciones de Adaycentes que no son vacias.

borrarPosVacias([],[]).

borrarPosVacias([[]|Adyacentes],AdyacentesNoVacias):-
		borrarPosVacias(Adyacentes,AdyacentesNoVacias).

borrarPosVacias([P|Adyacentes],[P|AdyacentesNoVacias]):-
		P\=[],
		borrarPosVacias(Adyacentes,AdyacentesNoVacias).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROCEDIMIENTOS DE OBTENCION DE NULAS CAPTURADAS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getNulasCapturadas(+Board,-CapturadasNegras,-CapturadasBlancas).
%
% CapturadasNegras contiene todas las posiciones vacias que están capturadas por
% fichas negras y CapturadasBlancas las que estan capturadas por fichas blancas.

getNulasCapturadas(Board,CapturadasNegras,CapturadasBlancas):-
		crearListaNulas(Board,ListaNulasNegras,"b"),
		crearListaNulas(Board,ListaNulasBlancas,"w"),
		getCapturadasPor(Board,ListaNulasNegras,"b",CapturadasNegras),
		getCapturadasPor(Board,ListaNulasBlancas,"w",CapturadasBlancas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% getCapturadasPor(+Board,+ListaNulas,+Color,-Capturadas).
%
% Capturadas contiene todas las posiciones vacias de ListaNulas que están
% capturadas por el color Color.

getCapturadasPor(_Board,[],_Color,[]).

getCapturadasPor(Board,[Pos|ListaNulas],Color,Capturadas):-
		capturada(Board,Pos,"-",Color,[],Capts1),							 % Capts1 contiene las posiciones capturadas que se verificaron para la captura de Pos
		diferencia(ListaNulas,Capts1,ListaAux),                 % Elimino de ListaNulas todas las posiciones que ya se verificaron en Capts1
		getCapturadasPor(Board,ListaAux,Color,Capts2),
		append(Capts1,Capts2,Capturadas).

getCapturadasPor(Board,[_Pos|ListaNulas],Color,Capturadas):-
		getCapturadasPor(Board,ListaNulas,Color,Capturadas).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% crearListaNulas(+Board,-ListaNulas,+Color).
%
% ListaNulas contiene todas las posiciones nulas/vacías del tablero que sean
% adyacentes a al menos una ficha de color. Con esto reducimos el conjunto de
% fichas sobre el cual verificaremos la captura al final del juego.

crearListaNulas(Board,ListaNulas,Color):-
		recorrerMatriz(Board,0,0,ListaNulas,Color,Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% recorrerMatriz(+Xs,+F,+C,-ListaNulas,+Color,+Board).
%
% Funciona en conjunto con recorrerLista.
% Por cada lista X de la lista Xs

recorrerMatriz([],_F,_C,[],_Color,_Board).

recorrerMatriz([X|Xs],F,C,Lista,Color,Board):-
		recorrerLista(Board,X,F,C,Lista1,Color),
		F1 is F+1,
		recorrerMatriz(Xs,F1,C,Lista2,Color,Board),
		append(Lista1,Lista2,Lista).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% recorrerLista(+Board,+Xs,+F,+C,-ListaNulas,+Color).
%
%

recorrerLista(_Board,[],_,_,[],_Color).

recorrerLista(Board,["-"|Xs],F,C,[[F,C]|ListaAux],Color):-
		getAdyacentes([F,C],Adyacentes),
		hayAlgunAdyacenteDeColor(Board,Adyacentes,Color),
		C1 is C+1,
		recorrerLista(Board,Xs,F,C1,ListaAux,Color).

recorrerLista(Board,[_X|Xs],F,C,ListaNulas,Color):-
		C1 is C+1,
		recorrerLista(Board,Xs,F,C1,ListaNulas,Color).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% hayAlgunAdyacenteDeColor(+Board,+Adyacentes,+Color).
%
% Verifica si alguna de las posiciones de la lista Adyacentes es del color Color.

hayAlgunAdyacenteDeColor(Board,[Pos|Adyacentes],Color):-
		obtenerContenido(Board,Pos,Color);
		hayAlgunAdyacenteDeColor(Board,Adyacentes,Color).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PROCEDIMIENTOS DE OPERACION SOBRE CONJUNTOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% diferencia(+Xs, +Ys, -Zs).
%
% Zs es el resultado de computar la resta de conjuntos Xs-Ys.

diferencia([],_,[]).

diferencia([X|Xs],Ys,Zs):-
		member(X,Ys),
		diferencia(Xs,Ys,Zs).

diferencia([X|Xs],Ys,Zs):-
		not(member(X,Ys)),
		diferencia(Xs,Ys,Z1),
		Zs=[X|Z1].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% unir(+Xs, +Ys, -Zs).
%
% Zs es el resultado de unir los conjuntos Xs e Ys.

unir([],Ys,Ys).

unir([X|Xs],Ys,Zs):-
    member(X,Ys), unir(Xs,Ys,Zs).

unir([X|Xs],Ys,[X|Z1]):-
    not(member(X,Ys)), unir(Xs,Ys,Z1).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% INTELIGENCIA DE LA MAQUINA %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% moverInteligencia(+Board,-NBoard).
%
% NBoard es la configuracion resultante de realizar una jugada a partir de Board.

% 1. La inteligencia intenta jugar a la defensiva, es decir, si el contrario
% puede colocar una ficha en una posicion Pos y capturar, la inteligencia coloca
% una ficha en esa posicion.
moverInteligencia(Board,NBoard):-
		crearListaNulas(Board,Adyacentes,"w"),
		verSiCaptura(Board,Adyacentes,PosAProteger,"b","w"),       % veo si hay alguna posicion en la que me capturen
		PosAProteger\=[],																		%tengo que proteger esta pos
		goMove(Board,"w",PosAProteger,NBoard).							% pongo ficha en esa posicion

% 2. Si no hay peligro de captura, juega a la ofensiva.
% Coloca fichas adyacentes a las del contrario para intentar capturarlo.
moverInteligencia(Board,NBoard):-
		crearListaNulas(Board,Adyacentes,"b"),
		colocarFicha(Board,Adyacentes,NBoard).

% 3. Si no hay posiciones disponibles adyacentes al contrario (muy raro), coloca
% una ficha an una posicion adyacente a las de su color.
moverInteligencia(Board,NBoard):-
		crearListaNulas(Board,[Pos|Adyacentes],"w"),
		verSiCaptura(Board,[Pos|Adyacentes],PosAProteger,"b","w"),
		PosAProteger=[],																			% no hay que proteger ninguna pos
		goMove(Board,"w",Pos,NBoard).													% pongo ficha en cualquier lado

% 4. Aqui juega agresivamente, es decir, busca posiciones en las que pueda capturar
% fichas del enemigo y coloca una ficha ahi.
% Ultima opcion ya que es un metodo muy lento. Se podria optimizar
moverInteligencia(Board,NBoard):-
		crearListaNulas(Board,[Pos|Adyacentes],"b"),
		verSiCaptura(Board,[Pos|Adyacentes],PosAColocar,"w","b"),
		PosAColocar=[],																			% no hay que proteger ninguna pos
		goMove(Board,"w",PosAColocar,NBoard).

% 5. En el caso de que el jugador pase en el primer turno y no haya fichas en el
% tablero, se coloca una ficha en una posicion arbitraria.
moverInteligencia(Board,NBoard):-
		goMove(Board,"w",[12,10],NBoard).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% colocarFicha(+Board,+Posiciones,-NBoard)
%
% Busca una posicion de la lista de Posiciones tal que al colocar una ficha suya,
% esta no quede capturada por el contrario y la coloca.

colocarFicha(Board,[Pos|_Posiciones],NBoard):-
		goMove(Board,"w",Pos,NBoard).

colocarFicha(Board,[_Pos|Posiciones],NBoard):-
		colocarFicha(Board,Posiciones,NBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% verSiMeCaptura(+Board,+Posiciones,-Pos,+Color,+ColorContrario).
%
% Simula una jugada del jugador Color para ver si existe una posicion
% en la lista Posiciones tal que si el jugador Color coloca una ficha en esa posicion,
% capture fichas de ColorContrario. Retorna esa posicion si existe, caso contrario
% retorna vacio.

verSiCaptura(_Board,[],[],_Color,_CC).

verSiCaptura(Board,[[R,C]|_Adyacentes],[R,C],Color,ColorContrario):-
	replace(Row, R, NRow, Board, TableroAux), replace("-", C, Color, Row, NRow),
	getAdyacentes([R,C],Adyacentes),
	devolverCapturadas(TableroAux,Adyacentes,Color,ColorContrario,Capturadas), % Devuelve las capturadas por la ficha que puse recien
	Capturadas\=[].

verSiCaptura(Board,[[R,C]|Adyacentes],Pos,Color,ColorContrario):-
	replace(Row, R, NRow, Board, TableroAux), replace("-", C, Color, Row, NRow),
	getAdyacentes([R,C],Ady),
	devolverCapturadas(TableroAux,Ady,Color,ColorContrario,Capturadas), % Devuelve las capturadas por la ficha que puse recien
	Capturadas=[],
	verSiCaptura(Board,Adyacentes,Pos,Color,ColorContrario).
