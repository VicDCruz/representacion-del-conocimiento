%--------------------------------------------------
% Load and Save from files
%--------------------------------------------------


%KB open and save

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).

save_kb(Route,KB):-
	open(Route,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

%compile an atom string of characters as a prolog term
atom_to_term(ATOM, TERM) :-
	atom(ATOM),
	atom_to_chars(ATOM,STR),
	atom_to_chars('.',PTO),
	append(STR,PTO,STR_PTO),
	read_from_chars(STR_PTO,TERM).

:- op(800,xfx,'=>').


%Cambiar elementos de la KB

cambiar_elemento(_,_,[],[]).

cambiar_elemento(X,Y,[X|T],[Y|N]):-
	cambiar_elemento(X,Y,T,N).

cambiar_elemento(X,Y,[H|T],[H|N]):-
	cambiar_elemento(X,Y,T,N).

%------------------------------
% Agregar nueva clase:
%------------------------------
% Definir la base antes: open_kb('kb.txt',KB)

agregar_clase(Nueva_clase,Padre,Nueva_KB) :-
	open_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KB),
	append(KB,[class(Nueva_clase,Padre,[],[],[])],Nueva_KB),
    save_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',Nueva_KB).

%------------------------------
% Agregar nueva objeto:
%------------------------------

agregar_objeto(Nuevo_objeto,Clase, Nueva_KB) :-
	open_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objectos),class(Clase,Padre,Prop,Rel,Nuevos_objectos),KB,Nueva_KB),
	append(Objectos,[[id=>Nuevo_objeto,[],[]]],Nuevos_objectos),
    save_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',Nueva_KB).

%--------------------------------------------------
% 3a Elimina Clases u objetos
%--------------------------------------------------

eliminar_elemento(_,[],[]).

eliminar_elemento(X,[X|T],N):-
	eliminar_elemento(X,T,N).

eliminar_elemento(X,[H|T],[H|N]):-
	eliminar_elemento(X,T,N),
	X\=H.

%------------------------------
% Elimina una clase
%------------------------------

eliminar_clase(Clase, KBFinal) :-
	open_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KB),
    eliminar_elemento(class(Clase,Padre,_,_,_),KB,KBAux),
	cambiar_padre(Clase,Padre,KBAux,KBAux2),
	delete_relations_with_object(Clase,KBAux2,KBFinal),%411 Esto es del repo
	save_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KBFinal).

cambiar_padre(_,_,[],[]).

cambiar_padre(PadreAntiguo,PadreNuevo,[class(Clase,PadreAntiguo,Prop,Rel,Objetos)|T],[class(Clase,PadreNuevo,Prop,Rel,Objetos)|N]):-
	cambiar_padre(PadreAntiguo,PadreNuevo,T,N).

cambiar_padre(PadreAntiguo,PadreNuevo,[H|T],[H|N]):-
	cambiar_padre(PadreAntiguo,PadreNuevo,T,N).

%-----------------------------------------------------
delete_relations_with_object(_,[],[]).%367

delete_relations_with_object(Object,[class(C,M,P,R,O)|T],[class(C,M,P,NewR,NewO)|NewT]):-
	cancel_relation(Object,R,NewR),
	del_relations(Object,O,NewO),
	delete_relations_with_object(Object,T,NewT).

cancel_relation(_,[],[]).%380

cancel_relation(Object,[[_=>Object,_]|T],NewT):-
	cancel_relation(Object,T,NewT).

cancel_relation(Object,[[not(_=>Object),_]|T],NewT):-
	cancel_relation(Object,T,NewT).

cancel_relation(Object,[[V=>Lst,W]|T],NewT):-
        is_list(Lst),
        eliminar_elemento(Object,Lst,NewLst),%390 original es deleteElement()
        cancel_relation(Object,T,Tmp),
        length(NewLst,Size),
        ( Size==1
         -> [Head|_] = NewLst, NewT = [[V=>Head,W]|Tmp]
         ;  ( Size>1
             ->  NewT = [[V=>NewLst,W]|Tmp]
             ; NewT = Tmp
            )
        ).

del_relations(_,[],[]).%374

del_relations(Object,[[id=>N,P,R]|T],[[id=>N,P,NewR]|NewT]):-
	cancel_relation(Object,R,NewR),
	del_relations(Object,T,NewT).

%-----------------------------------------------------



%------------------------------
% Elimina un objeto:
%------------------------------

elemento_en_arreglo(X,[X|_]).
elemento_en_arreglo(X,[_|T]):-
	elemento_en_arreglo(X,T).


%% Remueve un objeto
eliminar_objeto(Objeto, KBFinal) :-
	open_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,Rel,ObjetoNuevo),KB,KBAux),
	elemento_en_arreglo([id=>Objeto|Propiedades],Objetos),
	eliminar_elemento([id=>Objeto|Propiedades],Objetos,ObjetoNuevo),
	delete_relations_with_object(Objeto,KBAux,KBFinal),%365 codigo del repo
	save_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KBFinal).


%------------------------------
% 1d Todas las clases a las que pertenece un objeto
%------------------------------

clases_perteneciente_objeto(_,[],desconocida):-!.

clases_perteneciente_objeto(Objeto,[class(Clase,_,_,_,Objetos)|_],Clase):-
	elemento_en_arreglo([id=>Objeto,_,_],Objetos),!.

clases_perteneciente_objeto(Objeto,[_|T],Clase):-
	clases_perteneciente_objeto(Objeto,T,Clase).

%% Clases de un objeto
clase_objeto(Objeto, Return) :-
	open_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KB),
	clases_perteneciente_objeto(Objeto,KB,Return).

%------------------------------
% 1e Todas las propiedades de un objeto o clase
%------------------------------

prop_objeto(_,[],desconocida):-!.

prop_objeto(Objeto,[class(_,_,Prop,_,Objetos)|_],Prop):-
	elemento_en_arreglo([id=>Objeto,_,_],Objetos),!.

prop_objeto(Objeto,[_|T],Prop):-
	prop_objeto(Objeto,T,Prop).

%% Propiedades de un objeto
propiedad_objeto(Objeto, Return) :-
	open_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KB),
	prop_objeto(Objeto,KB,Return).

%------------------------------

prop_clase(_,[],desconocida):-!.

prop_clase(Clase,[class(Class,_,Prop,_,_)|_],Prop):-
	Clase == Class,!.

prop_clase(Clase,[_|T],Prop):-
	prop_clase(Clase,T,Prop).

%% Propiedades de una clase
propiedad_clase(Clase, Return) :-
	open_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KB),
	prop_clase(Clase,KB,Return).

%------------------------------
% 1f Todas las relaciones de un objeto o clase
%------------------------------

rel_objeto(_,[],desconocida):-!.

rel_objeto(Objeto,[class(_,_,_,Rel,Objetos)|_],Rel):-
	elemento_en_arreglo([id=>Objeto,_,_],Objetos),!.

rel_objeto(Objeto,[_|T],Rel):-
	rel_objeto(Objeto,T,Rel).

%% Relacion de un objeto
relacion_objeto(Objeto, Return) :-
	open_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KB),
	rel_objeto(Objeto,KB,Return).

%------------------------------

rel_clase(_,[],desconocida):-!.

rel_clase(Clase,[class(Class,_,_,Rel,_)|_],Rel):-
	Clase == Class,!.

rel_clase(Clase,[_|T],Rel):-
	rel_clase(Clase,T,Rel).

%% Relacion de una clase
relacion_clase(Clase, Return) :-
	open_kb('C:/Users/Ingenieria/Downloads/Prolog/kb.txt',KB),
	rel_clase(Clase,KB,Return).












%AYUDA
%	write('Hello World!'),
%	class(Clase,Padre,Prop,Rel,Objectos)