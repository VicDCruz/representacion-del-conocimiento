%% New operators
:- op(800,xfx,'=>').

%=====List functions=====
replaceAll(_, _, [], []).
replaceAll(X, Y, [X|T], [Y|TRes]):-
    replaceAll(X, Y, T, TRes).
replaceAll(X, Y, [H|T], [H|TRes]):-
    replaceAll(X, Y, T, TRes).

searchClass(Name, [], [])
searchClass(Name, [class(Name, _, _, _, _)|T], class(Name, _, _, _, _)).
searchClass(X, [Y|T], R):-
    searchClass(X, T, R).

%=====2b=====
addClassProperty(Class, Name, Value, ActualKB, NewKB):-
    searchClass(Class, ActualKB, class(Class, Parent, ActualProperties, R, O)),
    appendProperty(ActualProperties, Name, Value, NewProperties).
    replaceAll(class(Class, Parent, ActualProperties, R, O),
        class(Class, Parent, NewProperties, R, O),
        ActualKB, NewKB),

appendProperty(ActualProperties, Name, yes, NewProperties):-
	append(ActualProperties, [[Name, 0]], NewProperties).

appendProperty(ActualProperties, Name, no, NewProperties):-
	append(ActualProperties, [[not(Name), 0]], NewProperties).

appendProperty(ActualProperties, Name, Value, NewProperties):-
	append(ActualProperties, [[Name=>Value, 0]], NewProperties).
