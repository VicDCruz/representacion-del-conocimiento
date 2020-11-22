%% New operators
:- op(800,xfx,'=>').

%=====List functions=====
replaceAll(_, _, [], []).
replaceAll(X, Y, [X|T], [Y|TRes]):-
    replaceAll(X, Y, T, TRes).
replaceAll(X, Y, [H|T], [H|TRes]):-
    replaceAll(X, Y, T, TRes).

%=====2b=====
%addClassProperty(Class, Property, ActualKB, NewKB):-
%    append(ActualKB, Res, NewKB)

%addProperty()
