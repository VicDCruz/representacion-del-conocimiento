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

:-op(800,xfx,'=>').
:-op(800,xfx,'==>').
:-op(800,xfx,'=>>').


%Cambiar elementos de la KB
cambiar_elemento(_,_,[],[]).
cambiar_elemento(X,Y,[X|T],[Y|N]):-
	cambiar_elemento(X,Y,T,N).
cambiar_elemento(X,Y,[H|T],[H|N]):-
	cambiar_elemento(X,Y,T,N).

%Cambiar realaciones de la KB
cambiar_relacion(_,_,[],[]).
cambiar_relacion(Nombre,Nuevo_nombre,[[Relacion=>Nombre,Peso]|T],[[Relacion=>Nuevo_nombre,Peso]|NT]):-
	cambiar_relacion(Nombre,Nuevo_nombre,T,NT).
cambiar_relacion(Nombre,Nuevo_nombre,[[not(Relacion=>Nombre),Peso]|T],[[not(Relacion=>Nuevo_nombre),Peso]|NT]):-
	cambiar_relacion(Nombre,Nuevo_nombre,T,NT).
cambiar_relacion(Nombre,Nuevo_nombre,[H|T],[H|NT]):-
	cambiar_relacion(Nombre,Nuevo_nombre,T,NT).

cambiar_relaciones(_,_,[],[]).
cambiar_relaciones(Objeto,Nuevo_nombre,[[id=>Nombre,Prop,Rel]|T],[[id=>Nombre,Prop,NRel]|NT]):-
	cambiar_relacion(Objeto,Nuevo_nombre,Rel,NRel),
	cambiar_relaciones(Objeto,Nuevo_nombre,T,NT).

cambiar_relaciones_con_objeto(_,_,[],[]).
cambiar_relaciones_con_objeto(Objeto,Nuevo_nombre,[clase(Clase,Padre,Prop,Rel,Objetos)|T],[clase(Clase,Padre,Prop,NRel,NObjetos)|NT]):-
	cambiar_relaciones(Objeto,Nuevo_nombre,Objetos,NObjetos),
	cambiar_relacion(Objeto,Nuevo_nombre,Rel,NRel),
	cambiar_relaciones_con_objeto(Objeto,Nuevo_nombre,T,NT).

%append([],Y,Y).
%append([H|X],Y,[H|Z]) :- 
%	append(X,Y,Z).

append_relacion(Rel,not(Nueva_rel),Otro,NRel):-
	append(Rel,[[not(Nueva_rel=>Otro),0]],NRel).
append_relacion(Rel,Nueva_rel,Otro,NRel):-
	append(Rel,[[Nueva_rel=>Otro,0]],NRel).

append_preferencia(Prop,Nueva_pref,Peso,NProp):-
	append(Prop,[[Nueva_pref,Peso]],NProp).

verifica_elemento(X,[X|_]).
verifica_elemento(X,[_|Z]):-
    verifica_elemento(X,Z).

cambia_herencia(_,_,[],[]).
cambia_herencia(Padre,Nuevo_padre,[clase(Clase,Padre,Prop,Rel,Objetos)|T],[class(Clase,Nuevo_padre,Prop,Rel,Objetos)|N]):-
	cambia_herencia(Padre,Nuevo_padre,T,N).
cambia_herencia(Padre,Nuevo_padre,[H|T],[H|N]):-
	cambia_herencia(Padre,Nuevo_padre,T,N).



%------------------------------
% Agregar nueva relacion de clase:  
%------------------------------
agregar_relacion_clase(Clase1,Clase2,Nueva_rel,KB,Nueva_KB):-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	cambiar_elemento(clase(Clase1,Padre,Prop,Rel,Objetos),clase(Clase1,Padre,Prop,NRel,Objetos),KB,Nueva_KB),
	append_relacion(Rel,Nueva_rel,Clase2,NRel),
    save_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\nueva_kb.txt',Nueva_KB).

agregar_preferencia_relacion_clase(Clase,Nueva_pref,Peso,KB,Nueva_KB):-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	cambiar_elemento(clase(Clase,Padre,Prop,Rel,Objetos),clase(Clase,Padre,Prop,NRel,Objetos),KB,Nueva_KB),
	append_preferencia(Rel,Nueva_pref,Peso,NRel),
	save_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\nueva_kb.txt',Nueva_KB).

%------------------------------
% Agregar nueva relacion de objeto:  
%------------------------------
agregar_relacion_objeto(Objeto1,Objeto2,Nueva_rel,KB,Nueva_KB) :-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	cambiar_elemento(clase(Clase,Padre,Prop,Rel,Objetos),clase(Clase,Padre,Prop,Rel,NObjetos),KB,Nueva_KB),
	verifica_elemento([id=>Objeto1,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto1,Propiedades,Relaciones],[id=>Objeto1,Propiedades,Nuevas_rel],Objetos,NObjetos),
	append_relacion(Relaciones,Nueva_rel,Objeto2,Nuevas_rel),
	save_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\nueva_kb.txt',Nueva_KB).

agregar_preferencia_relacion_objeto(Objeto,Nueva_pref,Peso,KB,Nueva_KB) :-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	cambiar_elemento(clase(Clase,Padre,Prop,Rel,Objetos),clase(Clase,Padre,Prop,Rel,NObjetos),KB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Propiedades,Nuevas_rel],Objetos,NObjetos),
	append_preferencia(Relaciones,Nueva_pref,Peso,Nuevas_rel),
	save_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\nueva_kb.txt',Nueva_KB).

%------------------------------
% Modificar el nombre de una clase:  
%------------------------------

cambiar_nombre_clase(Clase,Nuevo_nombre,KB,Nueva_KB) :-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	cambiar_elemento(clase(Clase,Padre,Prop,Rel,Objetos),clase(Nuevo_nombre,Padre,Prop,Rel,Objetos),KB,Tmp_KB),
	cambia_herencia(Clase,Nuevo_nombre,Tmp_KB,Tmp_KB2),
	cambiar_relaciones_con_objeto(Clase,Nuevo_nombre,Tmp_KB,Nueva_KB),
	save_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\nueva_kb.txt',Nueva_KB).

%------------------------------
% Modificar el nombre de un objeto:  
%------------------------------
cambiar_nombre_objeto(Objeto,Nuevo_nombre,KB,Nueva_KB) :-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	cambiar_elemento(clase(Clase,Padre,Prop,Rel,Objetos),clase(Clase,Padre,Prop,Rel,NObjetos),KB,Tmp_KB),
	verifica_elemento([id=>Objeto|Propiedades],Objetos),
	cambiar_elemento([id=>Objeto|Propiedades],[id=>Nuevo_nombre|Propiedades],Objetos,NObjetos),
	cambiar_relaciones_con_objeto(Objeto,Nuevo_nombre,Tmp_KB,Nueva_KB),
	save_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\nueva_kb.txt',Nueva_KB).







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