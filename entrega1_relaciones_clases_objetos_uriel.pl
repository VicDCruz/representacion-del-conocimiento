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

append([],Y,Y).
append([H|X],Y,[H|Z]) :- 
	append(X,Y,Z).

append_relacion(Rel,not(Nueva_rel),Otro,NRel):-
	append(Rel,[[not(Nueva_rel=>Otro),0]],NRel).
append_relacion(Rel,Nueva_rel,Otro,NRel):-
	append(Rel,[[Nueva_rel=>Otro,0]],NRel).

append_preferencia(Prop,Nueva_pref,Peso,NProp):-
	append(Prop,[[Nueva_pref,Peso]],NProp).

verifica_elemento(X,[X|_]).
verifica_elemento(X,[_|Z]):-
    verificaElemento(X,Z),!.

%------------------------------
% Agregar nueva relacion de clase:  
%------------------------------
% Definir la base antes: open_kb('kb.txt',KB)

agregar_relacion_clase(Clase1,Clase2,Nueva_rel,KB,Nueva_KB):-
	cambiar_elemento(class(Clase1,Padre,Prop,Rel,Objetos),class(Clase1,Padre,Prop,NRel,Objetos),KB,Nueva_KB),
	append_relacion(Rel,Nueva_rel,Clase2,NRel),
    save_kb('nueva_kb.txt',Nueva_KB).

agregar_preferencia_relacion_clase(Clase,Nueva_pref,Peso,KB,Nueva_KB):-
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,NRel,Objetos),KB,Nueva_KB),
	append_preferencia(Rel,Nueva_pref,Peso,NRel).
	save_kb('nueva_kb.txt',Nueva_KB).

%------------------------------
% Agregar nueva relacion de objeto:  
%------------------------------

agregar_relacion_objeto(Objeto1,Objeto2,Nueva_rel,KB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,Rel,NObjetos),KB,Nueva_KB),
	verifica_elemento([id=>Objeto1,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto1,Propiedades,Relaciones],[id=>Objeto1,Propiedades,Nuevas_rel],Objetos,NObjetos),
	append_relacion(Relaciones,Nueva_rel,Objeto2,Nuevas_rel).
	save_kb('nueva_kb.txt',Nueva_KB).

agregar_preferencia_relacion_objeto(Objeto,Nueva_pref,Peso,KB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,Rel,NObjetos),KB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Propiedades,Nuevas_rel],Objects,NObjetos),
	append_preferencia(Relaciones,Nueva_pref,Peso,Nuevas_rel).
	save_kb('nueva_kb.txt',Nueva_KB).

%------------------------------
% Ejemplo:  
%------------------------------

%Cargar la base en una lista, imprimir la lista en consola y guardar todo en un nuevo archivo.
%No olvides poner las rutas correctas para localizar el archivo kb.txt en tu computadora!!!

%ejemplo:-
%	open_kb('D:/Documentos/MCIC/Materias/Inteligencia Artificial/Proyectos/Representación del conocimiento/Entrega_1/kb.txt',KB),
%	write('KB: '),
%	write(KB),
%	save_kb('D:/Documentos/MCIC/Materias/Inteligencia Artificial/Proyectos/Representación del conocimiento/Entrega_1/nueva_kb.txt',KB).