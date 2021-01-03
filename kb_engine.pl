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

append_propiedad(Props,Nueva_prop,yes,Nueva_props):-
	append(Props,[[Nueva_prop,0]],Nueva_props).
append_propiedad(Props,Nueva_prop,no,Nueva_props):-
	append(Props,[[not(Nueva_prop),0]],Nueva_props).
append_propiedad(Props,Nueva_prop,Valor,Nueva_props):-
	append(Props,[[Nueva_prop=>Valor,0]],Nueva_props).

verifica_elemento(X,[X|_]).
verifica_elemento(X,[_|Z]):-
    verifica_elemento(X,Z).

cambia_herencia(_,_,[],[]).
cambia_herencia(Padre,Nuevo_padre,[clase(Clase,Padre,Prop,Rel,Objetos)|T],[clase(Clase,Nuevo_padre,Prop,Rel,Objetos)|N]):-
	cambia_herencia(Padre,Nuevo_padre,T,N).
cambia_herencia(Padre,Nuevo_padre,[H|T],[H|N]):-
	cambia_herencia(Padre,Nuevo_padre,T,N).

borrar_elementos_misma_propiedad(_,[],[]).
borrar_elementos_misma_propiedad(X,[[X=>_,_]|T],N):-
	borrar_elementos_misma_propiedad(X,T,N).
borra_elementos_misma_propiedad(X,[H|T],[H|N]):-
	borrar_elementos_misma_propiedad(X,T,N).

borrar_elementos_misma_propiedad_negada(_,[],[]).
borrar_elementos_misma_propiedad_negada(X,[[not(X=>_),_]|T],N):-
	borrar_elementos_misma_propiedad_negada(X,T,N).
borrar_elementos_misma_propiedad_negada(X,[H|T],[H|N]):-
	borrar_elementos_misma_propiedad_negada(X,T,N).


eliminar_elemento(_,[],[]).
eliminar_elemento(X,[X|T],N):-
	eliminar_elemento(X,T,N).
eliminar_elemento(X,[H|T],[H|N]):-
	eliminar_elemento(X,T,N),
	X\=H.

eliminar_propiedad_objeto(Objeto,Propiedad,OriginalKB,Nueva_KB) :-
	cambiar_elemento(clase(Clase,Padre,Props,Rel,Objetos),clase(Clase,Padre,Props,Rel,NObjetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Nueva_pro,Relaciones],Objetos,NObjetos),
	borrar_elementos_misma_propiedad(Propiedad,Propiedades,Tmp),
	eliminar_elemento([not(Propiedad),_],Tmp,Tmp2),
	eliminar_elemento([Propiedad,_],Tmp2,Nueva_pro).

eliminar_propiedad_clase(Clase,Propiedad,OriginalKB,Nueva_KB) :-
	cambiar_elemento(clase(Clase,Padre,Props,Rel,Objetos),clase(Clase,Padre,Nueva_props,Rel,Objetos),OriginalKB,Nueva_KB),
	borrar_elementos_misma_propiedad(Propiedad,Props,Tmp),
	eliminar_elemento([not(Propiedad),_],Tmp,Tmp2),
	eliminar_elemento([Propiedad,_],Tmp2,Nueva_props).
agregar_propiedad_clase(Clase,Nueva_prop,Valor,OriginalKB,Nueva_KB) :-
	cambiar_elemento(clase(Clase,Padre,Props,Rel,Objetos),clase(Clase,Padre,Nueva_props,Rel,Objetos),OriginalKB,Nueva_KB),
	append_propiedad(Props,Nueva_prop,Valor,Nueva_props).
agregar_propiedad_objeto(Objeto,Nueva_prop,Valor,OriginalKB,Nueva_KB) :-
	cambiar_elemento(clase(Clase,Padre,Props,Rel,Objetos),clase(Clase,Padre,Props,Rel,NObjetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Nueva_pro,Relaciones],Objetos,NObjetos),
	append_propiedad(Propiedades,Nueva_prop,Valor,Nueva_pro).

verifica_objeto(_,[],unknown):-!.
verifica_objeto(Objeto,[clase(_,_,_,_,O)|_],no):-
	verifica_elemento([id=>not(Objeto),_,_],O).
verifica_objeto(Objeto,[clase(_,_,_,_,O)|_],yes):-
	verifica_elemento([id=>Objeto,_,_],O).
verifica_objeto(Objeto,[_|T],Respuesta):-
	verifica_objeto(Objeto,T,Respuesta),!.

verifica_objeto_lista(Objeto,KB,Res):-
	verifica_objeto(Objeto,KB,Res),!.
verifica_objeto_lista([],_,yes):-!.
verifica_objeto_lista([H|_],KB,unknown):-
	verifica_objeto(H,KB,unknown).
verifica_objeto_lista([H|_],KB,no):-
	verifica_objeto(H,KB,no).
verifica_objeto_lista([H|T],KB,Res):-
	verifica_objeto(H,KB,yes),
	verifica_objeto_lista(T,KB,Res).

verifica_clase(_,[],unknown):-!.
verifica_clase(Clase,[clase(not(Class),_,_,_,_)|_],no):-!.
verifica_clase(Clase,[clase(Clase,_,_,_,_)|_],yes):-!.
verifica_clase(Clase,[_|T],Respuesta):-
	verifica_clase(Clase,T,Respuesta).

verifica_clase_lista(Clase,KB,Res):-
	verifica_clase(Clase,KB,Res),!.
verifica_clase_lista([],_,yes):-!.
verifica_clase_lista([H|_],KB,unknown):-
	verifica_clase(H,KB,unknown).
verifica_clase_lista([H|_],KB,no):-
	verifica_clase(H,KB,no).
verifica_clase_lista([H|T],KB,Res):-
	verifica_clase(H,KB,yes),
	verifica_clase_lista(T,KB,Res).


elimina_relacion_objeto(Objeto,not(Relacion),OriginalKB,Nueva_KB) :-
	cambiar_elemento(clase(Clase,Padre,Props,Rels,Objetos),clase(Clase,Padre,Props,Rels,Nuevos_objetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Object,Propiedades,NRelaciones],Objetos,Nuevos_objetos),
	borrar_elementos_misma_propiedad_negada(Relacion,Relaciones,NRelaciones).
elimina_relacion_objeto(Objeto,Relacion,OriginalKB,Nueva_KB) :-
	cambiar_elemento(clase(Clase,Padre,Props,Rels,Objetos),clase(Clase,Padre,Props,Rels,Nuevos_objetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Propiedades,NRelaciones],Objetos,Nuevos_objetos),
	borrar_elementos_misma_propiedad(Relacion,Relaciones,NRelaciones).

agrega_relacion_objeto(Objeto,Nueva_rel,Otro_objeto,OriginalKB,Nueva_KB) :-
	cambiar_elemento(clase(Clase,Padre,Props,Rels,Objetos),clase(Clase,Padre,Props,Rels,Nuevos_objetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Propiedades,NRelaciones],Objetos,Nuevos_objetos),
	append_relacion(Relaciones,Nueva_rel,Otro_objeto,NRelaciones).

elimina_relacion_clase(Clase,not(Relacion),OriginalKB,Nueva_KB) :-
	cambiar_elemento(clase(Clase,Padre,Props,Rels,Objetos),clase(Clase,Padre,Props,NRelaciones,Objetos),OriginalKB,Nueva_KB),
	borrar_elementos_misma_propiedad_negada(Relacion,Rels,NRelaciones).
elimina_relacion_clase(Clase,Relacion,OriginalKB,Nueva_KB) :-
	cambiar_elemento(clase(Clase,Padre,Props,Rels,Objetos),clase(Clase,Padre,Props,NRelaciones,Objetos),OriginalKB,Nueva_KB),
	borrar_elementos_misma_propiedad(Relacion,Rels,NRelaciones).

agrega_relacion_clase(Clase,Nueva_relacion,Otra_clase,OriginalKB,Nueva_KB) :-
	cambiar_elemento(clase(Clase,Padre,Props,Rels,Objetos),clase(Clase,Padre,Props,NRelaciones,Objetos),OriginalKB,Nueva_KB),
	append_relacion(Rels,Nueva_relacion,Otra_clase,NRelaciones).


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
% Modificar el valor de una propiedad específica de un objeto 
%------------------------------
cambiar_valor_propiedad_objeto(Objeto,Propiedad,Nuevo_valor,KB,Nueva_KB):-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	eliminar_propiedad_objeto(Objeto,Propiedad,KB,TemporalKB),
	agregar_propiedad_objeto(Objeto,Propiedad,Nuevo_valor,TemporalKB,Nueva_KB),
	save_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\nueva_kb.txt',Nueva_KB).

%------------------------------
% Modificar el valor de una propiedad específica de una clase 
%------------------------------
cambiar_valor_propiedad_clase(Clase,Propiedad,Nuevo_valor,KB,Nueva_KB):-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	eliminar_propiedad_clase(Clase,Propiedad,KB,TemporalKB),
	agregar_propiedad_clase(Clase,Propiedad,Nuevo_valor,TemporalKB,Nueva_KB),
	save_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\nueva_kb.txt',Nueva_KB).

%------------------------------
% Modificar con quien tiene una relacion específica una clase 
%------------------------------
cambiar_valor_relacion_objeto(Objeto,Relacion,Nuevo_objeto_relacionado,KB,Nueva_KB):-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	verifica_objeto_lista(Nuevo_objeto_relacionado,KB,yes),
	elimina_relacion_objeto(Objeto,Relacion,KB,TemporalKB),
	agrega_relacion_objeto(Objeto,Relacion,Nuevo_objeto_relacionado,TemporalKB,Nueva_KB),
	save_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\nueva_kb.txt',Nueva_KB).


%------------------------------
% Modificar con quien tiene una relacion específica una clase 
%------------------------------
cambiar_valor_relacion_clase(Clase,Relacion,Nueva_clase_relacionada,KB,Nueva_KB):-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	verifica_clase_lista(Nueva_clase_relacionada,KB,yes),
	elimina_relacion_clase(Clase,Relacion,KB,TemporalKB),
	agrega_relacion_clase(Clase,Relacion,Nueva_clase_relacionada,TemporalKB,Nueva_KB),
	save_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\nueva_kb.txt',Nueva_KB).




%------------------------------
% La extensión de una clase (el conjunto de todos los objetos que pertenecen a la misma, ya
%sea porque se declaren directamente o porque están en la cerradura de la relación de
%herencia).
%------------------------------

hijos_clase(Clase,KB,Respuesta):-
	verifica_clase(Clase,KB,yes),
	hijos_clase(Clase,KB,Respuesta),!.

hijos_clase(_,_,unknown).

hijos_clase(_,[],[]).

hijos_clase(Clase,[clase(Hijo,Clase,_,_,_)|T],Hijos):-
	hijos_clase(Clase,T,Hermanos),!,	
	append([Hijo],Hermanos,Hijos).

hijos_clase(Clase,[_|T],Hijos):-
	hijos_clase(Clase,T,Hijos).	
	

extrae_nombres_objetos([],[]):-!.

extrae_nombres_objetos([[id=>Nombre,_,_]|T],Objetos):-
	extrae_nombres_objetos(T,Resto),
	append([Nombre],Resto,Objetos).

hijos_lista_clase([],_,[]).

hijos_lista_clase([Hijo|T],KB,Nietos,):-
	hijos_clase(Hijo,KB,Hijos),
	hijos_lista_clase(T,KB,Primos),
	append(Hijos,Primos,Nietos,).

descendientes_clase(Clase,KB,Descendientes):-
	verifica_clase(Clase,KB,yes),
	hijos_clase(Clase,KB,Hijos),
	todos_descendientes_clase(Hijos,KB,Descendientes),!.

descendientes_clase(_,_,unknown).

todos_descendientes_clase([],_,[]).

todos_descendientes_clase(Clases,KB,Descendientes):-
	hijos_lista_clase(Clases,KB,Hijos),
	todos_descendientes_clase(Hijos,KB,Resto_descendientes),!,
	append(Clases,Resto_descendientes,Descendientes).

objetos_solo_clase(_,[],unknown):-!.

objetos_solo_clase(Clase,[clase(Clase,_,_,_,O)|_],Objetos):-
	extrae_nombres_objetos(O,Objetos),!.

objetos_solo_clase(Clase,[_|T],Objetos):-
	objetos_solo_clase(Clase,T,Objetos),!.

objetos_clase(Clase,KB,Objetos):-	
	verifica_clase(Clase,KB,yes),
	objetos_solo_clase(Clase,KB,Objetos_clase),
	descendientes_clase(Clase,KB,Hijos),
	objetos_descendientes_todos_clase(Hijos,KB,Descendientes_objetos),
	append(Objetos_clase,Descendientes_objetos,Objetos),!.

objetos_clase(_,_,unknown).

objetos_descendientes_todos_clase([],_,[]).

objetos_descendientes_todos_clase([Clase|T],KB,Todos_objetos):-
	open_kb('D:\\Documentos\\MCIC\\Materias\\Inteligencia_Artificial\\Proyectos\\Representacion_del_conocimiento\\Entrega_1\\TaxonomiaNoMonotonica_art.txt',KB),
	objetos_solo_clase(Clase,KB,Objetos),
	objetos_descendientes_todos_clase(T,KB,Resto),
	append(Objetos,Resto,Todos_objetos),!.	


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