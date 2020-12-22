%% New operators
:- op(800,xfx,'=>').

%=====List functions=====
%------------------------------
% Reemplazar todas las coincidencias
%------------------------------
replaceAll(_, _, [], []).
replaceAll(X, Y, [X|T], [Y|TRes]):-
    replaceAll(X, Y, T, TRes).
replaceAll(X, Y, [H|T], [H|TRes]):-
    replaceAll(X, Y, T, TRes).

%------------------------------
% Checar si el elemento existe
%------------------------------
existsElement(X,[X|_]).
existsElement(X,[_|T]):-
	existsElement(X,T).

%------------------------------
% Eliminar un elemento
%------------------------------
deleteElement(_,[],[]).

deleteElement(X,[X|T],N):-
	deleteElement(X,T,N).

deleteElement(X,[H|T],[H|N]):-
	deleteElement(X,T,N),
	X\=H.

%------------------------------
% Cambiar un elemento
%------------------------------
changeElement(X,Y,[X|T],[Y|N]):-
	changeElement(X,Y,T,N).

changeElement(X,Y,[H|T],[H|N]):-
	changeElement(X,Y,T,N).

%------------------------------
% Encontrar una propiedad
%------------------------------
findProperty([class(Class, _, Properties, _, _)|T], Class, Properties).

findProperty([H|T], Class, Properties):-
    findProperty(T, Class, Properties).

%------------------------------
% Encontrar una propiedad
%------------------------------
getAllClasses([], []).

getAllClasses([H|T], [H|M]):-
    getAll(T, M).

%------------------------------
% Agregar nueva propiedad a una clase:
%------------------------------
addClassProperty(Class, Name, Value):-
    open_kb('kb.txt',ActualKB),
    findProperty(ActualKB, Class, ActualProperties),
    appendProperty(
        ActualProperties,
        Name,
        Value,
        NewProperties),
    replaceAll(
		class(Class, Parent, ActualProperties, R, O),
		class(Class, Parent, NewProperties, R, O),
		ActualKB,
		NewKB),
    save_kb('nueva_kb.txt', NewKB).

appendProperty(ActualProperties, Name, yes, NewProperties):-
	append(ActualProperties, [[Name, 0]], NewProperties).

appendProperty(ActualProperties, Name, no, NewProperties):-
	append(ActualProperties, [[not(Name), 0]], NewProperties).

appendProperty(ActualProperties, Name, Value, NewProperties):-
	append(ActualProperties, [[Name=>Value, 0]], NewProperties).

%------------------------------
% Agregar nueva propiedad a un objeto:
%------------------------------
addObjectProperty(Object, NewProperty, Value) :-
    open_kb('kb.txt', ActualKB),
    forEachClassAdd(ActualKB).

forEachClassAdd([]).
forEachClassAdd([class(_, _, _, _, Objects)|T])
    forEachClassAdd(T),
	existsElement([id=>Object, Properties, Relations], Objects),
	replaceAll(
        [id=>Object, Properties, Relations],
        [id=>Object, NewProperties, Relations],
        Objects,
        NewObjects),
	replaceAll(
        class(Class, Parent, P, R, Objects),
        class(Class, Parent, P, R, NewObjects),
        ActualKB,
        NewKB),
	appendProperty(Properties, NewProperty, Value, NewProperties),
    save_kb('nueva_kb.txt', NewKB).

%------------------------------
% Eliminar valor de una propiedad
%------------------------------
deleteClassProperty(Class, Property):-
    open_kb('kb.txt', ActualKB),
    findProperty(ActualKB, Class, Properties),
	deleteAllWithProperty(Property, Properties, Aux),
	deleteElement([not(Property), _], Aux, Aux2),
	deleteElement([Property, _], Aux2, NewProperties),
    save_kb('nueva_kb.txt', NewKB).

deleteAllWithProperty(_, [], []).

deleteAllWithProperty(X, [[X=>_,_]|T], N):-
	deleteAllWithProperty(X, T, N).

deleteAllWithProperty(X, [H|T], [H|N]):-
	deleteAllWithProperty(X, T, N).

%------------------------------
% Eliminar valor de un objeto
%------------------------------
deleteObjectProperty(Object, Property):-
    open_kb('kb.txt', ActualKB),
    forEachClassDelete(ActualKB).

forEachClassDelete([|T])
    forEachClassAdd(T),
	isElement([id=>Object, Properties, Relations], Objects),
	changeElement(
        [id=>Object, Properties, Relations],
        [id=>Object, NewProperties, Relations],
        Objects,
        NewObjects),
	deleteAllWithProperty(Property, Properties, Aux),
	deleteElement([not(Property),_], Aux, Aux2),
	deleteElement([Property,_], Aux2, NewProperties),
    save_kb('nueva_kb.txt', NewKB).
