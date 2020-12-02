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
% Agregar nueva propiedad a una clase:
%------------------------------
addClassProperty(Class, Name, Value):-
    open_kb('kb.txt',ActualKB),
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
    open_kb('kb.txt', ActualKB)
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
	appendProperty(Properties, NewProperty, Value, NewProperties).
    save_kb('nueva_kb.txt', NewKB).

