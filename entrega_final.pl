%--------------------------------------------------
% Guardar y Cargar Archivos
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
:- op(850,xfx,'=>>').

% Si un elemento pertenece a una lista
es_elemento(X,[X|_]).
es_elemento(X,[_|Y]):-
	es_elemento(X,Y).

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

agregar_clase(Nueva_clase,Padre,KB,Nueva_KB) :-
	append(KB,[class(Nueva_clase,Padre,[],[],[])],Nueva_KB),
    save_kb('nueva_kb.txt',Nueva_KB).

%------------------------------
% Agregar nuevo objeto:  
%------------------------------

agregar_objeto(Nuevo_objeto,Clase,KB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objectos),class(Clase,Padre,Prop,Rel,Nuevos_objectos),KB,Nueva_KB),
	append(Objectos,[[id=>Nuevo_objeto,[],[]]],Nuevos_objectos),
    save_kb('nueva_kb.txt',Nueva_KB).


%-------------------------------------
% Eliminar una relación de una clase:  
%-------------------------------------

% Eliminar todos los elementos de una lista con una misma propiedad:  


eliminar_elementos_misma_prop(_,[],[]).

eliminar_elementos_misma_prop(X,[X=>_|L],R):-
	eliminar_elementos_misma_prop(X,L,R).

eliminar_elementos_misma_prop(X,[[X=>_,_]|L],R):-
	eliminar_elementos_misma_prop(X,L,R).

eliminar_elementos_misma_prop(X,[Y|L],[Y|R]):-
	eliminar_elementos_misma_prop(X,L,R).


% Eliminar todos los elementos de una lista con una misma propiedad negada:  

eliminar_elementos_misma_prop_negada(_,[],[]).

eliminar_elementos_misma_prop_negada(X,[not(X=>_)|L],R):-
	eliminar_elementos_misma_prop_negada(X,L,R).

eliminar_elementos_misma_prop_negada(X,[[not(X=>_),_]|L],R):-
	eliminar_elementos_misma_prop_negada(X,L,R).

eliminar_elementos_misma_prop_negada(X,[Y|L],[Y|R]):-
	eliminar_elementos_misma_prop_negada(X,L,R).



eliminar_relacion_clase(Clase,not(Relacion),KB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,NuevasRels,Objectos),KB,Nueva_KB),
	eliminar_elementos_misma_prop_negada(Relacion,Rels,NuevasRels).

eliminar_relacion_clase(Clase,Relacion,KB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,NuevasRels,Objectos),KB,Nueva_KB),
	eliminar_elementos_misma_prop(Relacion,Rels,NuevasRels).

%-------------------------------------
% Eliminar una relación de un objeto:  
%-------------------------------------

eliminar_relacion_objeto(Objecto,not(Relacion),KB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,Rels,NuevosObjectos),KB,Nueva_KB),
	es_elemento([id=>Objecto,Propiedades,Relaciones],Objectos),
	cambiar_elemento([id=>Objecto,Propiedades,Relaciones],[id=>Objecto,Propiedades,NuevasRelaciones],Objectos,NuevosObjectos),
	eliminar_elementos_misma_prop_negada(Relacion,Relaciones,NuevasRelaciones),
	save_kb('nueva_kb.txt',Nueva_KB).

eliminar_relacion_objeto(Objecto,Relacion,KB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,Rels,NuevosObjectos),KB,Nueva_KB),
	es_elemento([id=>Objecto,Propiedades,Relaciones],Objectos),
	cambiar_elemento([id=>Objecto,Propiedades,Relaciones],[id=>Objecto,Propiedades,NuevasRelaciones],Objectos,NuevosObjectos),
	eliminar_elementos_misma_prop(Relacion,Relaciones,NuevasRelaciones),
	save_kb('nueva_kb.txt',Nueva_KB).

%-------------------------------------
% Extensión de una relación:  
%-------------------------------------

todos_los_objetos([],[]).
todos_los_objetos([class(_,_,_,_,Objs)|L],R):-
	append(Objs,[],R1),
	todos_los_objetos(L,R2),!,
	append(R1,R2,R).
	%save_kb('objetos.txt',R).

objetos_con_misma_relacion(_,[],[]).
objetos_con_misma_relacion(X,[[_,_,[]]|L],R):-
	objetos_con_misma_relacion(X,L,R),!.
objetos_con_misma_relacion(X,[[_,_,[[]]]|L],R):-
	objetos_con_misma_relacion(X,L,R),!.
objetos_con_misma_relacion(X,[[Id,Prop,[[X=>Y,Val]|LR]]|L],[[Id,Prop,[[X=>Y,Val]]]|R]):-
	objetos_con_misma_relacion(X,[[Id,Prop,LR]|L],R),!.
objetos_con_misma_relacion(X,[[Id,Prop,[[_=>_,_]|LR]]|L],R):-
	objetos_con_misma_relacion(X,[[Id,Prop,LR]|L],R),!.

acomoda_objetos([],[]).
acomoda_objetos([[id=>X,_,[[_=>[Y|Z],_]]]|L],[X:[Y|Z]|R]):-
	acomoda_objetos(L,R),!.
acomoda_objetos([[id=>X,_,[[_=>Y,_]]]|L],[X:[Y]|R]):-
	acomoda_objetos(L,R),!.

clase_de_objeto(_,[],[]).
clase_de_objeto(X,[class(Name,_,_,_,[[id=>X|_]])|_],Name).
clase_de_objeto(X,[class(Name,_,_,_,[[id=>X|_]|_])|_],Name).
clase_de_objeto(X,[class(_,_,_,_,[])|L],R):-
	clase_de_objeto(X,L,R),!.
clase_de_objeto(X,[class(N,P,Prop,Rel,[[id=>_|_]|LR])|L],R):-
	clase_de_objeto(X,[class(N,P,Prop,Rel,LR)|L],R),!.

clases_padre(none,_,_,[]).
clases_padre(_,_,[],[]).
clases_padre(A,_,[class(A,none,_,_,_)|_],[]).
clases_padre(A,KB,[class(A,P,_,_,_)|_],[P|R]):-
	clases_padre(P,KB,KB,R),!.
clases_padre(A,KB,[class(_,_,_,_,_)|L],R):-
	clases_padre(A,KB,L,R),!.

misma_relacion(_,_,[],[]).
misma_relacion(_,N,[class(N,_,_,[],_)|_],[]).
misma_relacion(X,N,[class(N,_,_,[[X=>Y,_]|_],_)|L],[Y|R]):-
	misma_relacion(X,N,L,R).
misma_relacion(X,N,[class(N,_,_,[[X=>_,_]|LR],_)|L],R):-
	misma_relacion(X,N,[class(N,_,_,LR,_)|L],R),!.
misma_relacion(X,N,[class(_,_,_,_,_)|L],R):-
	misma_relacion(X,N,L,R),!.

relaciones_herencia(_,[],_,[]).
relaciones_herencia(X,[Clase|LC],KB,R):-
	misma_relacion(X,Clase,KB,R1),
	relaciones_herencia(X,LC,KB,R2),!,
	append(R1,R2,R).

relaciones_de_clase(_,_,[],[]).
relaciones_de_clase(X,KB,[Obj:Val|L],[Obj:Resultado|R]):-
	clase_de_objeto(Obj,KB,ClaseActual),
	clases_padre(ClaseActual,KB,KB,ClasesPadre),
	append([ClaseActual],ClasesPadre,Clases),
	relaciones_herencia(X,Clases,KB,RH),
	append(Val,RH,Resultado),
	relaciones_de_clase(X,KB,L,R),!.

extension_relacion(Relacion,KB,Extension):-
	todos_los_objetos(KB,Objetos),
	objetos_con_misma_relacion(Relacion,Objetos,NuevosObjectos),
	acomoda_objetos(NuevosObjectos,Resultado),
	relaciones_de_clase(Relacion,KB,Resultado,Extension).