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

%=================================================================================================
%==== URIEL ====
hijos_clase(Clase,KB,Respuesta):-
	verifica_clase(Clase,KB,yes),
	hijos_clase(Clase,KB,Respuesta),!.

hijos_clase(_,_,unknown).

hijos_clase(_,[],[]).

hijos_clase(Clase,[class(Hijo,Clase,_,_,_)|T],Hijos):-
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

objetos_solo_clase(Clase,[class(Clase,_,_,_,O)|_],Objetos):-
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
cambiar_relaciones_con_objeto(Objeto,Nuevo_nombre,[class(Clase,Padre,Prop,Rel,Objetos)|T],[class(Clase,Padre,Prop,NRel,NObjetos)|NT]):-
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
cambia_herencia(Padre,Nuevo_padre,[class(Clase,Padre,Prop,Rel,Objetos)|T],[class(Clase,Nuevo_padre,Prop,Rel,Objetos)|N]):-
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
	cambiar_elemento(class(Clase,Padre,Props,Rel,Objetos),class(Clase,Padre,Props,Rel,NObjetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Nueva_pro,Relaciones],Objetos,NObjetos),
	borrar_elementos_misma_propiedad(Propiedad,Propiedades,Tmp),
	eliminar_elemento([not(Propiedad),_],Tmp,Tmp2),
	eliminar_elemento([Propiedad,_],Tmp2,Nueva_pro).

eliminar_propiedad_clase(Clase,Propiedad,OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rel,Objetos),class(Clase,Padre,Nueva_props,Rel,Objetos),OriginalKB,Nueva_KB),
	borrar_elementos_misma_propiedad(Propiedad,Props,Tmp),
	eliminar_elemento([not(Propiedad),_],Tmp,Tmp2),
	eliminar_elemento([Propiedad,_],Tmp2,Nueva_props).
agregar_propiedad_class(Clase,Nueva_prop,Valor,OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rel,Objetos),class(Clase,Padre,Nueva_props,Rel,Objetos),OriginalKB,Nueva_KB),
	append_propiedad(Props,Nueva_prop,Valor,Nueva_props).
agregar_propiedad_objeto(Objeto,Nueva_prop,Valor,OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rel,Objetos),class(Clase,Padre,Props,Rel,NObjetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Nueva_pro,Relaciones],Objetos,NObjetos),
	append_propiedad(Propiedades,Nueva_prop,Valor,Nueva_pro).

verifica_objeto(_,[],unknown):-!.
verifica_objeto(Objeto,[class(_,_,_,_,O)|_],no):-
	verifica_elemento([id=>not(Objeto),_,_],O).
verifica_objeto(Objeto,[class(_,_,_,_,O)|_],yes):-
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
verifica_clase(Clase,[class(not(Class),_,_,_,_)|_],no):-!.
verifica_clase(Clase,[class(Clase,_,_,_,_)|_],yes):-!.
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
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objetos),class(Clase,Padre,Props,Rels,Nuevos_objetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Object,Propiedades,NRelaciones],Objetos,Nuevos_objetos),
	borrar_elementos_misma_propiedad_negada(Relacion,Relaciones,NRelaciones).
elimina_relacion_objeto(Objeto,Relacion,OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objetos),class(Clase,Padre,Props,Rels,Nuevos_objetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Propiedades,NRelaciones],Objetos,Nuevos_objetos),
	borrar_elementos_misma_propiedad(Relacion,Relaciones,NRelaciones).

agrega_relacion_objeto(Objeto,Nueva_rel,Otro_objeto,OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objetos),class(Clase,Padre,Props,Rels,Nuevos_objetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Propiedades,NRelaciones],Objetos,Nuevos_objetos),
	append_relacion(Relaciones,Nueva_rel,Otro_objeto,NRelaciones).

elimina_relacion_clase(Clase,not(Relacion),OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objetos),class(Clase,Padre,Props,NRelaciones,Objetos),OriginalKB,Nueva_KB),
	borrar_elementos_misma_propiedad_negada(Relacion,Rels,NRelaciones).
elimina_relacion_clase(Clase,Relacion,OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objetos),class(Clase,Padre,Props,NRelaciones,Objetos),OriginalKB,Nueva_KB),
	borrar_elementos_misma_propiedad(Relacion,Rels,NRelaciones).

agrega_relacion_clase(Clase,Nueva_relacion,Otra_clase,OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objetos),class(Clase,Padre,Props,NRelaciones,Objetos),OriginalKB,Nueva_KB),
	append_relacion(Rels,Nueva_relacion,Otra_clase,NRelaciones).

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

%==== VICTOR ====
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
% Saber si existe una clase:
%------------------------------
isClass(_, [], null):-!.

isClass(Class, [class(not(Class),_,_,_,_)|_], no):-!.

isClass(Class, [class(Class,_,_,_,_)|_], yes):-!.

isClass(Class, [_|T], Answer):-
	isClass(Class, T, Answer).

isClassList(Class, KB, Ans):-
	isClass(Class, KB, Ans),!.

isClassList([], _, yes):-!.

isClassList([H|_], KB, null):-
	isClass(H, KB, null).

isClassList([H|_], KB, no):-
	isClass(H, KB, no).

isClassList([H|T], KB, Ans):-
	isClass(H, KB, yes),
	isClassList(T, KB, Ans).

%------------------------------
% Saber si existe un objeto:
%------------------------------
isObject(_, [], null):-!.

isObject(Object,[class(_, _, _, _, O)|_], no):-
	existsElement([id=>not(Object), _, _], O).

isObject(Object, [class(_, _, _, _, O)|_], yes):-
	existsElement([id=>Object, _, _], O).

isObject(Object, [_|T], Answer):-
	isObject(Object, T, Answer),!.

isObjectList(Object, KB, Ans):-
	isObject(Object, KB, Ans),!.
isObjectList([], _, yes):-!.
isObjectList([H|_], KB, null):-
	isObject(H, KB, null).
isObjectList([H|_], KB, no):-
	isObject(H, KB, no).
isObjectList([H|T], KB, Ans):-
	isObject(H, KB, yes),
	isObjectList(T, KB, Ans).

%------------------------------
% Obtener nombres de objetos de una clase:
%------------------------------
getNamesObjectsClass(_, [], null):-!.

getNamesObjectsClass(Class, [class(Class,_,_,_,O)|_], Objects):-
	getNamesObjects(O, Objects),!.

getNamesObjectsClass(Class,[_|T], Objects):-
	getNamesObjectsClass(Class, T, Objects),!.
	
getNamesObjects([], []):-!.

getNamesObjects([[id=>Name,_,_]|T], NewA):-
	getNamesObjects(T, OldA),
	append([Name], OldA, NewA).

%------------------------------
% Obtener hijos de una clase:
%------------------------------
getClassChildren(_, [], []).

getClassChildren(Class, [class(Son, Class, _, _, _)|T], Children):-
	getClassChildren(Class, T, Brothers),!,
	append([Son], Brothers, Children).

getClassChildren(Class, [_|T], Children):-
	getClassChildren(Class, T, Children).

%------------------------------
% Obtener hijos de una lista de clases
%------------------------------
getClassesChildren([], _, []).

getClassesChildren([H|T], KB, Children):-
	getClassChildren(H, KB, R1),
	getClassesChildren(T, KB, R2),
	append(R1, R2, Children).

%------------------------------
% Obtener todos los descendientes de una clase:
%------------------------------
getDescendantsClass(Class, KB, Descendants):-
	isClass(Class, KB, yes),
	getClassChildren(Class, KB, Sons),
	getAllDescendantsClass(Sons, KB, Descendants),!.

getDescendantsClass(_, _, null).

getAllDescendantsClass([], _, []).

getAllDescendantsClass(Classes, KB, Descendants):-
	getClassesChildren(Classes, KB, Sons),
	getAllDescendantsClass(Sons, KB, RestOfDescendants),!,
	append(Classes, RestOfDescendants, Descendants).

%------------------------------
% Obtener nombre de los objetos
%------------------------------
getObjectsNames([], []):-!.

getObjectsNames([[id=>Name, _, _]|T], Objects):-
	getObjectsNames(T, Rest),
	append([Name], Rest, Objects).

%------------------------------
% Obtener objetos dentro de una clase:
%------------------------------
getObjectsInClass(_, [], null):-!.

getObjectsInClass(Class, [class(Class, _, _, _, O)|_], Objects):-
	getObjectsNames(O, Objects),!.

getObjectsInClass(Class, [_|T], Objects):-
	getObjectsInClass(Class, T, Objects),!.

%------------------------------
% Obtener objetos de los descendientes de una clase:
%------------------------------
getDescendantsObjects([], _, []).

getDescendantsObjects([Class|T], KB, Res):-
	getObjectsInClass(Class, KB, Objects),
	getDescendantsObjects(T, KB, OldChildren),
	append(Objects, OldChildren, Res),!.

%------------------------------
% Obtener todos los objetos de una clase:
%------------------------------

getClassObjects(Class, KB, R):-
	isClass(Class, KB, yes),
	getDescendantsClass(Class, KB, Sons),
	getDescendantsObjects(Sons, KB, OldChildren),
	getNamesObjectsClass(Class, KB, ClassName),
	append(ClassName, OldChildren, R),!.

getClassObjects(_, _, null).

%------------------------------
% Obtener propiedades adentro de un objeto:
%------------------------------
getPropertiesInObject(_, [], []).

getPropertiesInObject(Object, [class(_, _, _, _, O)|_], Properties):-
	existsElement([id=>Object, Properties, _], O),!.

getPropertiesInObject(Object, [_|T], Properties):-
	getPropertiesInObject(Object, T, Properties).

%------------------------------
% Obtener propiedades de un objeto:
%------------------------------
getClassOfObject(_, [], null):-!.

getClassOfObject(Object,[class(Class, _, _, _, O)|_], Class):-
	existsElement([id=>Object, _, _], O),!.

getClassOfObject(Object, [_|T], Class):-
	getClassOfObject(Object, T, Class).

%------------------------------
% Obtener padre de una clase:
%------------------------------
getClassParent(_, [], null).

getClassParent(Class, [class(Class, Parent, _, _, _)|_], Parent):-!.

getClassParent(Class, [_|T], Parent):-
	getClassParent(Class, T, Parent).

%------------------------------
% Obtener los antecesores de una clase:
%------------------------------
getAncestorsList(top, _, []):-!.

getAncestorsList(Class, KB, Ancestors):-
	getClassParent(Class, KB, Parent),
	append([Parent], GrandParents, Ancestors),
	getAncestorsList(Parent, KB, GrandParents).

%------------------------------
% Obtener propiedades solo en una clase:
%------------------------------
getPropertiesInClass(_, [], []).

getPropertiesInClass(Class, [class(Class, _, Properties, _, _)|_], Properties).

getPropertiesInClass(Class, [_|T], Properties):-
	getPropertiesInClass(Class, T, Properties).

%------------------------------
% Unir propiedades de los ancestros:
%------------------------------
mergeAncestorsProperties([], _, []).

mergeAncestorsProperties([H|T], KB, Res):-
	mergeAncestorsProperties(T, KB, U),
	getPropertiesInClass(H, KB, Properties),
	append(Properties, ['UNKNOWN'], NewProperties),
	append(NewProperties, U, Res).

%------------------------------
% Eliminar propiedades:
%------------------------------
deleteSameProperties(_, [], []).

deleteSameProperties(X, [X=>_|T], N):-
	deleteSameProperties(X, T, N).

deleteSameProperties(X, [H|T], [H|N]):-
	deleteSameProperties(X, T, N).

%------------------------------
% Eliminar propiedades negadas:
%------------------------------
deleteSameNegatedProperties(_, [], []).

deleteSameNegatedProperties(X, [not(X=>_)|T], N):-
	deleteSameNegatedProperties(X, T, N).

deleteSameNegatedProperties(X, [H|T], [H|N]):-
	deleteSameNegatedProperties(X, T, N).

%------------------------------
% Filtrar por propiedades unicas:
%------------------------------
filterUniqueProperties([], []).

filterUniqueProperties([P=>V|T], [P=>V|U]):-
	deleteSameProperties(P, T, R1),
	deleteElement(not(P=>V), R1, R2),
	filterUniqueProperties(R2, U),!.

filterUniqueProperties([not(P=>V)|T], [not(P=>V)|U]):-
	deleteSameNegatedProperties(P, T, R1),
	deleteElement(P=>V, R1, R2),
	filterUniqueProperties(R2, U),!.

filterUniqueProperties([not(H)|T], [not(H)|U]):-
	deleteElement(not(H), T, R1),
	deleteElement(H, R1, R2),
	filterUniqueProperties(R2, U),!.

filterUniqueProperties([H|T],[H|U]):-
	deleteElement(H, T, R1),
	deleteElement(not(H), R1, R2),
	filterUniqueProperties(R2, U),!.

%------------------------------
% Obtener propiedades de un objeto:
%------------------------------
getObjectProperties(Object, KB, AllProperties):-
	isObject(Object, KB, yes),
	getPropertiesInObject(Object, KB, ObjectProperties),
	getClassOfObject(Object, KB, Class),
	getAncestorsList(Class, KB, Ancestors),
	mergeAncestorsProperties([Class|Ancestors], KB, ClassProperties),
	append(ObjectProperties, ['UNKNOWN'], ObjectProperties2),
	append(ObjectProperties2, ClassProperties, Temp),
	filterUniqueProperties(Temp, AllProperties),!.

getObjectProperties(_, _, null).

%------------------------------
% Encontrar valor de una propiedad:
%------------------------------
searchPropertyValue(_, [], null).

searchPropertyValue(Attribute, [Attribute=>Value|_], Value).

searchPropertyValue(Attribute, [not(Attribute)|_], no).

searchPropertyValue(Attribute, [Attribute|_], yes).

searchPropertyValue(Attribute, [_|T], Value):-
	searchPropertyValue(Attribute, T, Value).

%------------------------------
% Obtener valor de una propiedad de un objeto:
%------------------------------
getObjectPropertyValue(Object, Property, KB, Value):-
	getObjectPropertyValue(Object, KB, yes),
	getObjectProperties(Object, KB, Properties),
	searchPropertyValue(Property, Properties, Value).

getObjectPropertyValue(_, _, _, null).

%==== DANIEL ====

%=== IVAN ====
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

%=================================================================================================
%==== PROYECTO ====
%=================================================================================================
%------------------------------
% 1(a) La extensión de una clase (el conjunto de todos los objetos que pertenecen a la misma, ya
%sea porque se declaren directamente o porque están en la cerradura de la relación de
%herencia).
%------------------------------
objetos_descendientes_todos_clase([],_,[]).

objetos_descendientes_todos_clase([Clase|T],Todos_objetos):-
	open_kb('C:\\Users\\uriel\\Documents\\GitHub\\representacion-del-conocimiento\\kb.txt',KB),
	objetos_solo_clase(Clase,KB,Objetos),
	objetos_descendientes_todos_clase(T,KB,Resto),
	append(Objetos,Resto,Todos_objetos),!.	

%------------------------------
% 1(b) Consultar extension de propiedad
%------------------------------
obtener_extension_propiedad(Property, Result):-
    open_kb('kb.txt', ActualKB),
	getClassObjects(top, ActualKB, AllObjects),
	filterObjectsByProperty(ActualKB, Property, AllObjects, Objects),
	deleteNullProperty(Objects, Result).

filterObjectsByProperty(_, _, [], []):-!.

filterObjectsByProperty(ActualKB, Property, [H|T], [H:Value|NewT]):-
	getObjectPropertyValue(H,Property, ActualKB, Value),!,
	filterObjectsByProperty(ActualKB, Property, T, NewT).

deleteNullProperty([], []).

deleteNullProperty([_:null|T], NewT):-
	deleteNullProperty(T, NewT),!.

deleteNullProperty([_:[null]|T], NewT):-
	deleteNullProperty(T, NewT),!.

deleteNullProperty([X:Y|T], [X:Y|NewT]):-
	deleteNullProperty(T, NewT),!.

%-------------------------------------
% 1(c) Extensión de una relación:  
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

existe_relacion([],desconocida):- !.
existe_relacion([X|Y],[X|Y]).

acomoda_objetos(desconocida,desconocida):- !.
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

relaciones_de_clase(_,_,desconocida,desconocida):- !.
relaciones_de_clase(_,_,[],[]).
relaciones_de_clase(X,KB,[Obj:Val|L],[Obj:Resultado|R]):-
	clase_de_objeto(Obj,KB,ClaseActual),
	clases_padre(ClaseActual,KB,KB,ClasesPadre),
	append([ClaseActual],ClasesPadre,Clases),
	relaciones_herencia(X,Clases,KB,RH),
	append(Val,RH,Resultado),
	relaciones_de_clase(X,KB,L,R),!.

extension_relacion(Relacion,Extension):-
	open_kb('kb.txt',KB),
	todos_los_objetos(KB,Objetos),
	objetos_con_misma_relacion(Relacion,Objetos,NuevosObjectos),
	existe_relacion(NuevosObjectos,Existe),
	acomoda_objetos(Existe,Resultado),
	relaciones_de_clase(Relacion,KB,Resultado,Extension).

%------------------------------
% 1(d) Todas las clases a las que pertenece un objeto
%------------------------------

clases_perteneciente_objeto(_,[],desconocida):-!.

clases_perteneciente_objeto(Objeto,[class(Clase,_,_,_,Objetos)|_],Clase):-
	elemento_en_arreglo([id=>Objeto,_,_],Objetos),!.

clases_perteneciente_objeto(Objeto,[_|T],Clase):-
	clases_perteneciente_objeto(Objeto,T,Clase).

%% Clases de un objeto
clase_objeto(Objeto, Return) :-
	open_kb('kb.txt',KB),
	clases_perteneciente_objeto(Objeto,KB,Return).

%------------------------------
% 1(e) Todas las propiedades de un objeto o clase
%------------------------------

prop_objeto(_,[],desconocida):-!.

prop_objeto(Objeto,[class(_,_,Prop,_,Objetos)|_],Prop):-
	elemento_en_arreglo([id=>Objeto,_,_],Objetos),!.

prop_objeto(Objeto,[_|T],Prop):-
	prop_objeto(Objeto,T,Prop).

%% Propiedades de un objeto
propiedad_objeto(Objeto, Return) :-
	open_kb('kb.txt',KB),
	prop_objeto(Objeto,KB,Return).

%------------------------------

prop_clase(_,[],desconocida):-!.

prop_clase(Clase,[class(Class,_,Prop,_,_)|_],Prop):-
	Clase == Class,!.

prop_clase(Clase,[_|T],Prop):-
	prop_clase(Clase,T,Prop).

%% Propiedades de una clase
propiedad_clase(Clase, Return) :-
	open_kb('kb.txt',KB),
	prop_clase(Clase,KB,Return).

%------------------------------
% 1(f) Todas las relaciones de un objeto o clase
%------------------------------

rel_objeto(_,[],desconocida):-!.

rel_objeto(Objeto,[class(_,_,_,Rel,Objetos)|_],Rel):-
	elemento_en_arreglo([id=>Objeto,_,_],Objetos),!.

rel_objeto(Objeto,[_|T],Rel):-
	rel_objeto(Objeto,T,Rel).

%% Relacion de un objeto
relacion_objeto(Objeto, Return) :-
	open_kb('kb.txt',KB),
	rel_objeto(Objeto,KB,Return).

%------------------------------

rel_clase(_,[],desconocida):-!.

rel_clase(Clase,[class(Class,_,_,Rel,_)|_],Rel):-
	Clase == Class,!.

rel_clase(Clase,[_|T],Rel):-
	rel_clase(Clase,T,Rel).

%% Relacion de una clase
relacion_clase(Clase, Return) :-
	open_kb('kb.txt',KB),
	rel_clase(Clase,KB,Return).

%------------------------------
% 2(a) Agregar nueva clase:  
%------------------------------
% Definir la base antes: open_kb('kb.txt',KB)

agregar_clase(Nueva_clase,Padre,Nueva_KB) :-
	open_kb('kb.txt',KB),
	append(KB,[class(Nueva_clase,Padre,[],[],[])],Nueva_KB),
    save_kb('kb.txt',Nueva_KB).

%------------------------------
% 2(a) Agregar nuevo objeto:  
%------------------------------

agregar_objeto(Nuevo_objeto,Clase,Nueva_KB) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objectos),class(Clase,Padre,Prop,Rel,Nuevos_objectos),KB,Nueva_KB),
	append(Objectos,[[id=>Nuevo_objeto,[],[]]],Nuevos_objectos),
    save_kb('kb.txt',Nueva_KB).

%------------------------------
% 2(b) Agregar nueva propiedad a una clase:
%------------------------------
agregar_propiedad_clase(Class, Name, Value):-
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
    save_kb('kb.txt', NewKB).

appendProperty(ActualProperties, Name, yes, NewProperties):-
	append(ActualProperties, [[Name, 0]], NewProperties).

appendProperty(ActualProperties, Name, no, NewProperties):-
	append(ActualProperties, [[not(Name), 0]], NewProperties).

appendProperty(ActualProperties, Name, Value, NewProperties):-
	append(ActualProperties, [[Name=>Value, 0]], NewProperties).

%------------------------------
% 2(b) Agregar nueva propiedad a un objeto:
%------------------------------
agregar_propiedad_objeto(Object, NewProperty, Value) :-
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
    save_kb('kb.txt', NewKB).

%------------------------------
% 2(c) Agregar nueva relacion de clase:  
%------------------------------
agregar_relacion_clase(Clase1,Clase2,Nueva_rel,Nueva_KB):-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase1,Padre,Prop,Rel,Objetos),class(Clase1,Padre,Prop,NRel,Objetos),KB,Nueva_KB),
	append_relacion(Rel,Nueva_rel,Clase2,NRel),
    save_kb('kb.txt',Nueva_KB).

agregar_preferencia_relacion_clase(Clase,Nueva_pref,Peso,Nueva_KB):-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,NRel,Objetos),KB,Nueva_KB),
	append_preferencia(Rel,Nueva_pref,Peso,NRel),
	save_kb('kb.txt',Nueva_KB).

%------------------------------
% 2(c) Agregar nueva relacion de objeto:  
%------------------------------
agregar_relacion_objeto(Objeto1,Objeto2,Nueva_rel,Nueva_KB) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,Rel,NObjetos),KB,Nueva_KB),
	verifica_elemento([id=>Objeto1,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto1,Propiedades,Relaciones],[id=>Objeto1,Propiedades,Nuevas_rel],Objetos,NObjetos),
	append_relacion(Relaciones,Nueva_rel,Objeto2,Nuevas_rel),
	save_kb('kb.txt',Nueva_KB).

agregar_preferencia_relacion_objeto(Objeto,Nueva_pref,Peso,Nueva_KB) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,Rel,NObjetos),KB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Propiedades,Nuevas_rel],Objetos,NObjetos),
	append_preferencia(Relaciones,Nueva_pref,Peso,Nuevas_rel),
	save_kb('kb.txt',Nueva_KB).

%--------------------------------------------------
% 3(a) Elimina Clases u objetos
%--------------------------------------------------

eliminar_elemento(_,[],[]).

eliminar_elemento(X,[X|T],N):-
	eliminar_elemento(X,T,N).

eliminar_elemento(X,[H|T],[H|N]):-
	eliminar_elemento(X,T,N),
	X\=H.

%------------------------------
% 3(a) Elimina una clase
%------------------------------

eliminar_clase(Clase, KBFinal) :-
	open_kb('kb.txt',KB),
    eliminar_elemento(class(Clase,Padre,_,_,_),KB,KBAux),
	cambiar_padre(Clase,Padre,KBAux,KBAux2),
	delete_relations_with_object(Clase,KBAux2,KBFinal),%411 Esto es del repo
	save_kb('kb.txt',KBFinal).

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

%------------------------------
% 3(a) Elimina un objeto:
%------------------------------

elemento_en_arreglo(X,[X|_]).
elemento_en_arreglo(X,[_|T]):-
	elemento_en_arreglo(X,T).


%% Remueve un objeto
eliminar_objeto(Objeto, KBFinal) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,Rel,ObjetoNuevo),KB,KBAux),
	elemento_en_arreglo([id=>Objeto|Propiedades],Objetos),
	eliminar_elemento([id=>Objeto|Propiedades],Objetos,ObjetoNuevo),
	delete_relations_with_object(Objeto,KBAux,KBFinal),%365 codigo del repo
	save_kb('kb.txt',KBFinal).

%------------------------------
% 3(b) Eliminar valor de una propiedad
%------------------------------
eliminar_propiedad_clase(Class, Property):-
    open_kb('kb.txt', ActualKB),
    findProperty(ActualKB, Class, Properties),
	deleteAllWithProperty(Property, Properties, Aux),
	deleteElement([not(Property), _], Aux, Aux2),
	deleteElement([Property, _], Aux2, NewProperties),
    save_kb('kb.txt', NewKB).

deleteAllWithProperty(_, [], []).

deleteAllWithProperty(X, [[X=>_,_]|T], N):-
	deleteAllWithProperty(X, T, N).

deleteAllWithProperty(X, [H|T], [H|N]):-
	deleteAllWithProperty(X, T, N).

%------------------------------
% 3(b) Eliminar valor de un objeto
%------------------------------
eliminar_propiedad_objeto(Object, Property):-
    open_kb('kb.txt', ActualKB),
    forEachClassDelete(ActualKB).

forEachClassDelete([|T])
    forEachClassAdd(T),
	existsElement([id=>Object, Properties, Relations], Objects),
	changeElement(
        [id=>Object, Properties, Relations],
        [id=>Object, NewProperties, Relations],
        Objects,
        NewObjects),
	deleteAllWithProperty(Property, Properties, Aux),
	deleteElement([not(Property),_], Aux, Aux2),
	deleteElement([Property,_], Aux2, NewProperties),
    save_kb('kb.txt', NewKB).

%-------------------------------------
% 3(c) Eliminar una relación de una clase:  
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



eliminar_relacion_clase(Clase,not(Relacion),Nueva_KB) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,NuevasRels,Objectos),KB,Nueva_KB),
	eliminar_elementos_misma_prop_negada(Relacion,Rels,NuevasRels),
	save_kb('kb.txt', Nueva_KB).

eliminar_relacion_clase(Clase,Relacion,Nueva_KB) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,NuevasRels,Objectos),KB,Nueva_KB),
	eliminar_elementos_misma_prop(Relacion,Rels,NuevasRels),
	save_kb('kb.txt', Nueva_KB).

%-------------------------------------
% 3(c) Eliminar una relación de un objeto:  
%-------------------------------------

eliminar_relacion_objeto(Objecto,not(Relacion),Nueva_KB) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,Rels,NuevosObjectos),KB,Nueva_KB),
	es_elemento([id=>Objecto,Propiedades,Relaciones],Objectos),
	cambiar_elemento([id=>Objecto,Propiedades,Relaciones],[id=>Objecto,Propiedades,NuevasRelaciones],Objectos,NuevosObjectos),
	eliminar_elementos_misma_prop_negada(Relacion,Relaciones,NuevasRelaciones),
	save_kb('kb.txt',Nueva_KB).

eliminar_relacion_objeto(Objecto,Relacion,Nueva_KB) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,Rels,NuevosObjectos),KB,Nueva_KB),
	es_elemento([id=>Objecto,Propiedades,Relaciones],Objectos),
	cambiar_elemento([id=>Objecto,Propiedades,Relaciones],[id=>Objecto,Propiedades,NuevasRelaciones],Objectos,NuevosObjectos),
	eliminar_elementos_misma_prop(Relacion,Relaciones,NuevasRelaciones),
	save_kb('kb.txt',Nueva_KB).

%------------------------------
% 4(a) Modificar el nombre de una clase:  
%------------------------------
cambiar_nombre_clase(Clase,Nuevo_nombre,Nueva_KB) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Nuevo_nombre,Padre,Prop,Rel,Objetos),KB,Tmp_KB),
	cambia_herencia(Clase,Nuevo_nombre,Tmp_KB,Tmp_KB2),
	cambiar_relaciones_con_objeto(Clase,Nuevo_nombre,Tmp_KB,Nueva_KB),
	save_kb('kb.txt',Nueva_KB).

%------------------------------
% 4(a) Modificar el nombre de un objeto:  
%------------------------------
cambiar_nombre_objeto(Objeto,Nuevo_nombre,Nueva_KB) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,Rel,NObjetos),KB,Tmp_KB),
	verifica_elemento([id=>Objeto|Propiedades],Objetos),
	cambiar_elemento([id=>Objeto|Propiedades],[id=>Nuevo_nombre|Propiedades],Objetos,NObjetos),
	cambiar_relaciones_con_objeto(Objeto,Nuevo_nombre,Tmp_KB,Nueva_KB),
	save_kb('kb.txt',Nueva_KB).

%------------------------------				
% 4(b) Modificar el valor de una propiedad específica de un objeto 
%------------------------------
cambiar_valor_propiedad_objeto(Objeto,Propiedad,Nuevo_valor,Nueva_KB):-
	open_kb('kb.txt',KB),
	eliminar_propiedad_objeto(Objeto,Propiedad,KB,TemporalKB),
	agregar_propiedad_objeto(Objeto,Propiedad,Nuevo_valor,TemporalKB,Nueva_KB),
	save_kb('kb.txt',Nueva_KB).

%------------------------------
% 4(b) Modificar el valor de una propiedad específica de una clase 
%------------------------------
cambiar_valor_propiedad_clase(Clase,Propiedad,Nuevo_valor,Nueva_KB):-
	open_kb('kb.txt',KB),
	eliminar_propiedad_clase(Clase,Propiedad,KB,TemporalKB),
	agregar_propiedad_clase(Clase,Propiedad,Nuevo_valor,TemporalKB,Nueva_KB),
	save_kb('kb.txt',Nueva_KB).

%------------------------------
% 4(c) Modificar con quien tiene una relacion específica una clase 
%------------------------------
cambiar_valor_relacion_objeto(Objeto,Relacion,Nuevo_objeto_relacionado,Nueva_KB):-
	open_kb('kb.txt',KB),
	verifica_objeto_lista(Nuevo_objeto_relacionado,KB,yes),
	elimina_relacion_objeto(Objeto,Relacion,KB,TemporalKB),
	agrega_relacion_objeto(Objeto,Relacion,Nuevo_objeto_relacionado,TemporalKB,Nueva_KB),
	save_kb('kb.txt',Nueva_KB).


%------------------------------
% 4(c) Modificar con quien tiene una relacion específica una clase 
%------------------------------
cambiar_valor_relacion_clase(Clase,Relacion,Nueva_clase_relacionada,Nueva_KB):-
	open_kb('kb.txt',KB),
	verifica_clase_lista(Nueva_clase_relacionada,KB,yes),
	elimina_relacion_clase(Clase,Relacion,KB,TemporalKB),
	agrega_relacion_clase(Clase,Relacion,Nueva_clase_relacionada,TemporalKB,Nueva_KB),
	save_kb('kb.txt',Nueva_KB).
