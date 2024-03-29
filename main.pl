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
	verifica_clase(Clase,KB,si),
	hijos_clase(Clase,KB,Respuesta),!.

hijos_clase(_,[],[]).

hijos_clase(Clase,[class(Hijo,Clase,_,_,_)|T],Hijos):-
	hijos_clase(Clase,T,Hermanos),!,	
	append([Hijo],Hermanos,Hijos).

hijos_clase(Clase,[_|T],Hijos):-
	hijos_clase(Clase,T,Hijos).	
	
hijos_clase(_,_,desconocida).

extrae_nombres_objetos([],[]):-!.

extrae_nombres_objetos([[id=>Nombre,_,_]|T],Objetos):-
	extrae_nombres_objetos(T,Resto),
	append([Nombre],Resto,Objetos).

hijos_lista_clase([],_,[]).

hijos_lista_clase([Hijo|T],KB,Nietos):-
	hijos_clase(Hijo,KB,Hijos),
	hijos_lista_clase(T,KB,Primos),
	append(Hijos,Primos,Nietos).

descendientes_clase(Clase,KB,Descendientes):-
	verifica_clase(Clase,KB,si),
	hijos_clase(Clase,KB,Hijos),
	todos_descendientes_clase(Hijos,KB,Descendientes),!.

descendientes_clase(_,_,desconocida).

todos_descendientes_clase([],_,[]).

todos_descendientes_clase(Clases,KB,Descendientes):-
	hijos_lista_clase(Clases,KB,Hijos),
	todos_descendientes_clase(Hijos,KB,Resto_descendientes),!,
	append(Clases,Resto_descendientes,Descendientes).

getOnlyObjectsInClass(_,[],unknown):-!.

getOnlyObjectsInClass(Class,[class(Class,_,_,_,O)|_],Objects):-
	extractObjectsNames(O,Objects),!.

getOnlyObjectsInClass(Class,[_|T],Objects):-
	getOnlyObjectsInClass(Class,T,Objects),!.
	
extractObjectsNames([],[]):-!.

extractObjectsNames([[id=>Name,_,_]|T],Objects):-
	extractObjectsNames(T,Rest),
	append([Name],Rest,Objects).

objetos_solo_clase(Class,KB,Objects):-
	isClass(Class,KB,si),
	getOnlyObjectsInClass(Class,KB,ObjectsInClass),
	getDescendantsClass(Class,KB,Sons),
	getAllObjectsDescendantsClasses(Sons,KB,DescendantObjects),
	append(ObjectsInClass,DescendantObjects,Objects),!.
objetos_solo_clase(_,_,desconocida).

getAllObjectsDescendantsClasses([],_,[]).

getAllObjectsDescendantsClasses([Class|T],KB,AllObjects):-
	objetos_solo_clase(Class,KB,Objects),
	getAllObjectsDescendantsClasses(T,KB,Rest),
	append(Objects,Rest,AllObjects),!.

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
	append(Rel,[[not(Nueva_rel)=>Otro,0]],NRel).
append_relacion(Rel,Nueva_rel,Otro,NRel):-
	append(Rel,[[Nueva_rel=>Otro,0]],NRel).

append_preferencia(Prop,Nueva_pref,Peso,NProp):-
	append(Prop,[[Nueva_pref,Peso]],NProp).

append_propiedad(Props,Nueva_prop,si,Nueva_props):-
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
agregar_propiedad_clase(Clase,Nueva_prop,Valor,OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rel,Objetos),class(Clase,Padre,Nueva_props,Rel,Objetos),OriginalKB,Nueva_KB),
	append_propiedad(Props,Nueva_prop,Valor,Nueva_props).
agregar_propiedad_objeto(Objeto,Nueva_prop,Valor,OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rel,Objetos),class(Clase,Padre,Props,Rel,NObjetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Nueva_pro,Relaciones],Objetos,NObjetos),
	append_propiedad(Propiedades,Nueva_prop,Valor,Nueva_pro).

verifica_objeto(_,[],desconocida):-!.
verifica_objeto(Objeto,[class(_,_,_,_,O)|_],no):-
	verifica_elemento([id=>not(Objeto),_,_],O).
verifica_objeto(Objeto,[class(_,_,_,_,O)|_],si):-
	verifica_elemento([id=>Objeto,_,_],O).
verifica_objeto(Objeto,[_|T],Respuesta):-
	verifica_objeto(Objeto,T,Respuesta),!.

verifica_objeto_lista(Objeto,KB,Res):-
	verifica_objeto(Objeto,KB,Res),!.
verifica_objeto_lista([],_,si):-!.
verifica_objeto_lista([H|_],KB,desconocida):-
	verifica_objeto(H,KB,desconocida).
verifica_objeto_lista([H|_],KB,no):-
	verifica_objeto(H,KB,no).
verifica_objeto_lista([H|T],KB,Res):-
	verifica_objeto(H,KB,si),
	verifica_objeto_lista(T,KB,Res).

verifica_clase(_,[],desconocida):-!.
verifica_clase(_,[class(not(_),_,_,_,_)|_],no):-!.
verifica_clase(Clase,[class(Clase,_,_,_,_)|_],si):-!.
verifica_clase(Clase,[_|T],Respuesta):-
	verifica_clase(Clase,T,Respuesta).

verifica_clase_lista(Clase,KB,Res):-
	verifica_clase(Clase,KB,Res),!.
verifica_clase_lista([],_,si):-!.
verifica_clase_lista([H|_],KB,desconocida):-
	verifica_clase(H,KB,desconocida).
verifica_clase_lista([H|_],KB,no):-
	verifica_clase(H,KB,no).
verifica_clase_lista([H|T],KB,Res):-
	verifica_clase(H,KB,si),
	verifica_clase_lista(T,KB,Res).


elimina_relacion_objeto(Objeto,not(Relacion),OriginalKB,Nueva_KB) :-
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objetos),class(Clase,Padre,Props,Rels,Nuevos_objetos),OriginalKB,Nueva_KB),
	verifica_elemento([id=>Objeto,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto,Propiedades,Relaciones],[id=>Objeto,Propiedades,NRelaciones],Objetos,Nuevos_objetos),
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
changeElement(_, _, [], []).

changeElement(X, Y, [X|T], [Y|N]):-
	changeElement(X, Y, T, N).

changeElement(X, Y, [H|T], [H|N]):-
	changeElement(X, Y, T, N).

%------------------------------
% Encontrar una propiedad
%------------------------------
findProperty([class(Class, _, Properties, _, _)|_], Class, Properties).

findProperty([_|T], Class, Properties):-
    findProperty(T, Class, Properties).

%------------------------------
% Saber si existe una clase:
%------------------------------
isClass(_, [], desconocida):-!.

isClass(Class, [class(not(Class),_,_,_,_)|_], no):-!.

isClass(Class, [class(Class,_,_,_,_)|_], si):-!.

isClass(Class, [_|T], Answer):-
	isClass(Class, T, Answer).

isClassList(Class, KB, Ans):-
	isClass(Class, KB, Ans),!.

isClassList([], _, si):-!.

isClassList([H|_], KB, desconocida):-
	isClass(H, KB, desconocida).

isClassList([H|_], KB, no):-
	isClass(H, KB, no).

isClassList([H|T], KB, Ans):-
	isClass(H, KB, si),
	isClassList(T, KB, Ans).

%------------------------------
% Saber si existe un objeto:
%------------------------------
isObject(_, [], desconocida):-!.

isObject(Object,[class(_, _, _, _, O)|_], no):-
	existsElement([id=>not(Object), _, _], O).

isObject(Object, [class(_, _, _, _, O)|_], si):-
	existsElement([id=>Object, _, _], O).

isObject(Object, [_|T], Answer):-
	isObject(Object, T, Answer),!.

isObjectList(Object, KB, Ans):-
	isObject(Object, KB, Ans),!.
isObjectList([], _, si):-!.
isObjectList([H|_], KB, desconocida):-
	isObject(H, KB, desconocida).
isObjectList([H|_], KB, no):-
	isObject(H, KB, no).
isObjectList([H|T], KB, Ans):-
	isObject(H, KB, si),
	isObjectList(T, KB, Ans).

%------------------------------
% Obtener nombres de objetos de una clase:
%------------------------------
getNamesObjectsClass(_, [], desconocida):-!.

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
	isClass(Class, KB, si),
	getClassChildren(Class, KB, Sons),
	getAllDescendantsClass(Sons, KB, Descendants),!.

getDescendantsClass(_, _, desconocida).

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
getObjectsInClass(_, [], desconocida):-!.

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
	isClass(Class, KB, si),
	getDescendantsClass(Class, KB, Sons),
	getDescendantsObjects(Sons, KB, OldChildren),
	getNamesObjectsClass(Class, KB, ClassName),
	append(ClassName, OldChildren, R),!.

getClassObjects(_, _, desconocida).

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
getClassOfObject(_, [], desconocida):-!.

getClassOfObject(Object,[class(Class, _, _, _, O)|_], Class):-
	existsElement([id=>Object, _, _], O),!.

getClassOfObject(Object, [_|T], Class):-
	getClassOfObject(Object, T, Class).

%------------------------------
% Obtener padre de una clase:
%------------------------------
getClassParent(_, [], desconocida).

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
	append(Properties, ['desconocida'], NewProperties),
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
% Eliminar elementos con la misma propiedad:
%------------------------------
deleteAllElementsWithSameProperty(_,[],[]).

deleteAllElementsWithSameProperty(X,[[X=>_,_]|T],N):-
	deleteAllElementsWithSameProperty(X,T,N).

deleteAllElementsWithSameProperty(X,[H|T],[H|N]):-
	deleteAllElementsWithSameProperty(X,T,N).

%------------------------------
% Obtener propiedades de un objeto:
%------------------------------
deleteAllElementsWithSamePropertySingle(_,[],[]).

deleteAllElementsWithSamePropertySingle(X,[X=>_|T],N):-
	deleteAllElementsWithSamePropertySingle(X,T,N).

deleteAllElementsWithSamePropertySingle(X,[H|T],[H|N]):-
	deleteAllElementsWithSamePropertySingle(X,T,N).

deleteAllElementsWithSameNegatedPropertySingle(_,[],[]).

deleteAllElementsWithSameNegatedPropertySingle(X,[not(X=>_)|T],N):-
	deleteAllElementsWithSameNegatedPropertySingle(X,T,N).

deleteAllElementsWithSameNegatedPropertySingle(X,[H|T],[H|N]):-
	deleteAllElementsWithSameNegatedPropertySingle(X,T,N).

delete_repeated_properties([],[]).

delete_repeated_properties([P=>V|T],[P=>V|NewT]):-
	deleteAllElementsWithSamePropertySingle(P,T,L1),
	deleteElement(not(P=>V),L1,L2),
	delete_repeated_properties(L2,NewT),!.

delete_repeated_properties([not(P=>V)|T],[not(P=>V)|NewT]):-
	deleteAllElementsWithSameNegatedPropertySingle(P,T,L1),
	deleteElement(P=>V,L1,L2),
	delete_repeated_properties(L2,NewT),!.

delete_repeated_properties([not(H)|T],[not(H)|NewT]):-
	deleteElement(not(H),T,L1),
	deleteElement(H,L1,L2),
	delete_repeated_properties(L2,NewT),!.

delete_repeated_properties([H|T],[H|NewT]):-
	deleteElement(H,T,L1),
	deleteElement(not(H),L1,L2),
	delete_repeated_properties(L2,NewT),!.

delete_repeated_abductions([],[]).

delete_repeated_abductions([El:Pref|T],[El:Pref|NewT]):-
	deleteAbuction(El,T,L2),
	delete_repeated_abductions(L2,NewT),!.

deleteAbuction(_,[],[]):-!.
deleteAbuction((Abd=>Val),[(Abd=>_):_|T],NewL):-
	deleteAbuction((Abd=>Val),T,NewL).
deleteAbuction(Abd,[Abd:_|T],NewL):-
	deleteAbuction(Abd,T,NewL).
deleteAbuction(Abd,[H:HV|T],[H:HV|NewL]):-
	deleteAbuction(Abd,T,NewL).

unir_lista([],L,L).
unir_lista([H|T],L,[H|M]):-
	unir_lista(T,L,M).

parte_de(E,[E|_]).
parte_de(E,[_|T]):-
	parte_de(E,T).

ordenar(L, S):- 
	permutacion(L, S), 
	ordered(S). 
permutacion([], []). 
permutacion(L, [H|R]):- 
	uno(L, H, L1), 
	permutacion(L1, R). 
uno([H|T], H, T). 
uno([X|R], H, [X|T]):- 
	uno(R, H, T). 
ordered([]). 
ordered([_]). 
ordered([X,Y|T]):-
	X=[_,ValX],
	Y=[_,ValY],
	ValX=<ValY,
	ordered([Y|T]). 

preordenar([],_,[]).
preordenar(['?'|Pref],Aux,PrefF):-
	ordenar(Aux,AuxO),
	preordenar(Pref,[],PrefO),
	unir_lista(AuxO,PrefO,PrefF).
preordenar([H|Pref],Aux,PrefO):-
	preordenar(Pref,[H|Aux],PrefO).

%%%Prefer handler

prefer(Prop,NewProp):-
	%print(Prop),
	prefer_extract(Prop,PropE,Pref),
	delete_repeated_properties(PropE,PropEE),
	preordenar(Pref,[],PrefO),
	prefer_handler(PrefO,PropEE,NewProp).

prefer_extract([],[],[]).
prefer_extract([[H,Peso]|T],TProp,[[H,Peso]|TP]):-
	Peso\=0,
	prefer_extract(T,TProp,TP).
prefer_extract([[H,0]|T],[H|TProp],TP):-
	prefer_extract(T,TProp,TP).
prefer_extract([_|T],TProp,['?'|TP]):-
	prefer_extract(T,TProp,TP).


prefer_handler([],NewProp,NewProp).
%Caso 1.1 preferencia x,y => x,y
prefer_handler([[(Pref=>'-')=>>(El=>'-'),_]|T],Prop,NewProp):-
	delete_repeated_properties(Prop,NProp),
	parte_de((Pref=>Val),NProp),
	unir_lista(Prop,[El=>Val],NP),
	prefer_handler(T,NP,NewProp).
%Caso 1.2 preferencia x,y => x,val
prefer_handler([[(Pref=>'-')=>>(El=>ValE),_]|T],Prop,NewProp):-
	parte_de((Pref=>_),Prop),
	unir_lista(Prop,[El=>ValE],NP),
	prefer_handler(T,NP,NewProp).
%caso 2 preferencia x,val=>x,valE
prefer_handler([[(Pref=>Val)=>>(El=>ValE),_]|T],Prop,NewProp):-
	delete_repeated_properties(Prop,NProp),
	parte_de((Pref=>Val),NProp),
	unir_lista(Prop,[El=>ValE],NP),
	prefer_handler(T,NP,NewProp).
%caso 3.1 preferencia x => x , x,val=>x, x=>x,val
prefer_handler([[Pref=>>El,_]|T],Prop,NewProp):-
	parte_de(Pref,Prop),
	unir_lista(Prop,[El],NP),
	prefer_handler(T,NP,NewProp).
%caso 3.2 preferencia x,y=>x
prefer_handler([[(Pref=>'-')=>>El,_]|T],Prop,NewProp):-
	parte_de((Pref=>_),Prop),
	unir_lista(Prop,[El],NP),
	prefer_handler(T,NP,NewProp).
%caso 3.3 preferencia '-'=>x,val
prefer_handler([['-'=>>(El=>Val),_]|T],Prop,NewProp):-
	unir_lista(Prop,[El=>Val],NP),
	prefer_handler(T,NP,NewProp).
%caso 3.4 preferencia '-'=>x
prefer_handler([['-'=>>El,_]|T],Prop,NewProp):-
	unir_lista(Prop,[El],NP),
	prefer_handler(T,NP,NewProp).
%caso 4.1 antecedentes de preferencia caso 1 x,y => x,y
prefer_handler([[(Pref=>'-')=>>(El=>'-'),_]|T],Prop,NewProp):-
	parte_de([_=>>(Pref=>_),_],T),
	prefer_handler(T,Prop,PropA),
	delete_repeated_properties(PropA,NPropA),
	parte_de((Pref=>Val),NPropA),
	unir_lista(Prop,[El=>Val],NP),
	prefer_handler(T,NP,NewProp).
%caso 4.2 antecedentes de preferencia caso 2 x,val=>x,valE
prefer_handler([[(Pref=>Val)=>>(El=>ValE),_]|T],Prop,NewProp):-
	parte_de([_=>>(Pref=>Val),_],T),
	prefer_handler(T,Prop,PropA),
	delete_repeated_properties(PropA,NPropA),
	parte_de((Pref=>Val),NPropA),
	unir_lista(Prop,[El=>ValE],NP),
	prefer_handler(T,NP,NewProp).
%caso 4.3 antecedentes de preferencia caso x => x
prefer_handler([[Pref=>>El,_]|T],Prop,NewProp):-
	parte_de([_=>>Pref,_],T),
	prefer_handler(T,Prop,PropA),
	parte_de(Pref,PropA),
	unir_lista(Prop,[El],NP),
	prefer_handler(T,NP,NewProp).
%caso 5.1 lista caso =>x,y
prefer_handler([[PrefL=>>(El=>'-'),_]|T],Prop,NewProp):-
	prefer_handlerL(PrefL,T,Prop,Val),
	unir_lista(Prop,[El=>Val],NP),
	prefer_handler(T,NP,NewProp).
%caso 5.2 lista caso =>x,val
prefer_handler([[PrefL=>>(El=>Val),_]|T],Prop,NewProp):-
	prefer_handlerL(PrefL,T,Prop,Val),
	unir_lista(Prop,[El=>Val],NP),
	prefer_handler(T,NP,NewProp).
%caso 5.3 lista caso =>x
prefer_handler([[PrefL=>>El,_]|T],Prop,NewProp):-
	prefer_handlerL(PrefL,T,Prop),
	unir_lista(Prop,[El],NP),
	prefer_handler(T,NP,NewProp).
%caso default, si no la encuentra.
prefer_handler([_|T],Prop,NewProp):-
	prefer_handler(T,Prop,NewProp).

%%manejo de lista
prefer_handlerL([],_,_).
%caso x
prefer_handlerL([Pref|T],LPref,Prop):-
	parte_de(Pref,Prop),
	prefer_handlerL(T,LPref,Prop).
%caso antecedentes x
prefer_handlerL([Pref|T],LPref,Prop):-
	parte_de([_=>>Pref,_],LPref),
	prefer_handler(LPref,Prop,PropA),
	parte_de(Pref,PropA),
	prefer_handlerL(T,LPref,Prop).
prefer_handlerL([],_,_,_).
%caso x,y
prefer_handlerL([(Pref=>'-')|T],LPref,Prop,Val):-
	delete_repeated_properties(Prop,NProp),
	parte_de((Pref=>Val),NProp),
	prefer_handlerL(T,LPref,Prop,Val),!.
%caso x,val
prefer_handlerL([(Pref=>Val)|T],LPref,Prop,Val):-
	delete_repeated_properties(Prop,NProp),
	parte_de((Pref=>Val),NProp),
	prefer_handlerL(T,LPref,Prop,Val).
%caso antecedentes x,y
prefer_handlerL([(Pref=>'-')|T],LPref,Prop,Val):-
	parte_de([_=>>(Pref=>_),_],LPref),
	prefer_handler(LPref,Prop,PropA),
	delete_repeated_properties(PropA,NPropA),
	parte_de((Pref=>Val),NPropA),
	prefer_handlerL(T,LPref,Prop,Val).
%caso antecedentes x,val
prefer_handlerL([(Pref=>Val)|T],LPref,Prop,Val):-
	parte_de([_=>>(Pref=>Val),_],LPref),
	prefer_handler(LPref,Prop,PropA),
	delete_repeated_properties(PropA,NPropA),
	parte_de((Pref=>Val),NPropA),
	prefer_handlerL(T,LPref,Prop,Val).

getObjectProperties(Object, KB, AllProperties):-
	isObject(Object, KB, si),
	getPropertiesInObject(Object, KB, ObjectProperties),
	getClassOfObject(Object, KB, Class),
	getAncestorsList(Class, KB, Ancestors),
	mergeAncestorsProperties([Class|Ancestors], KB, ClassProperties),
	append(ObjectProperties, ['desconocida'], ObjectProperties2),
	append(ObjectProperties2, ClassProperties, Temp),
	prefer(Temp, TempPref),
	filterUniqueProperties(TempPref, AllProperties),!.

getObjectProperties(_, _, desconocida).

%------------------------------
% Encontrar valor de una propiedad:
%------------------------------
searchPropertyValue(_, [], desconocida).

searchPropertyValue(Attribute, [Attribute=>Value|_], Value).

searchPropertyValue(Attribute, [not(Attribute)|_], no).

searchPropertyValue(Attribute, [Attribute|_], si).

searchPropertyValue(Attribute, [_|T], Value):-
	searchPropertyValue(Attribute, T, Value).

%------------------------------
% Obtener valor de una propiedad de un objeto:
%------------------------------
getObjectPropertyValue(Object, Property, KB, Value):-
	isObject(Object, KB, si),
	getObjectProperties(Object, KB, Properties),
	searchPropertyValue(Property, Properties, Value),!.

getObjectPropertyValue(_, _, _, desconocida).

%==== DANIEL ====

%=== IVAN ====
% Si un elemento pertenece a una lista
es_elemento(X,[X|_]).
es_elemento(X,[_|Y]):-
	es_elemento(X,Y).

%=================================================================================================
%==== PROYECTO ====
%=================================================================================================
%------------------------------
% 1(a) La extensión de una clase (el conjunto de todos los objetos que pertenecen a la misma, ya
%sea porque se declaren directamente o porque están en la cerradura de la relación de
%herencia).
%------------------------------
objetos_descendientes_todos_clase([],_,[]).

objetos_descendientes_todos_clase(Clase,Objetos):-
	open_kb('kb.txt',KB),
	objetos_solo_clase(Clase,KB,Objetos).

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

deleteNullProperty([_:desconocida|T], NewT):-
	deleteNullProperty(T, NewT),!.

deleteNullProperty([_:[desconocida]|T], NewT):-
	deleteNullProperty(T, NewT),!.

deleteNullProperty([X:Y|T], [X:Y|NewT]):-
	deleteNullProperty(T, NewT),!.

%-------------------------------------
% 1(c) Extensión de una relación y obtener valor de propiedad/relacion de objeto:  
%-------------------------------------

todos_los_objetos([],[]).
todos_los_objetos([class(_,_,_,_,Objs)|L],R):-
	append(Objs,[],R1),
	todos_los_objetos(L,R2),!,
	append(R1,R2,R).
	%save_kb('objetos.txt',R).

trae_objeto(_,[],desconocida).
trae_objeto(Objeto,[[id=>Objeto,Props,Rels]|_],[id=>Objeto,Props,Rels]).
trae_objeto(Objeto,[_|L],R):-
	trae_objeto(Objeto,L,R),!.

trae_valor_propiedad(_,desconocida,[desconocida]).
trae_valor_propiedad(_,[_,[],_],[desconocida]).
trae_valor_propiedad(Propiedad,[_,[[Propiedad=>Valor,_]|_],_],Valor).
trae_valor_propiedad(Propiedad,[Id,[_|L],Rels],R):-
	trae_valor_propiedad(Propiedad,[Id,L,Rels],R),!.

trae_valor_relacion(_,desconocida,[desconocida]).
trae_valor_relacion(_,[_,_,[]],[desconocida]).
trae_valor_relacion(Relacion,[_,_,[[Relacion=>Valor,_]|_]],Valor).
trae_valor_relacion(Relacion,[Id,P,[_|LR]],R):-
	trae_valor_relacion(Relacion,[Id,P,LR],R),!.

lista_clase(_,[],[]).
lista_clase(Clase,[class(Clase,_,Props,Rels,_)|_],[Clase,Props,Rels]).
lista_clase(Clase,[_|CL],R):-
	lista_clase(Clase,CL,R),!.

trae_prop_valor_clase(_,[],_,[]).
trae_prop_valor_clase(Propiedad,[Clase|LC],KB,R):-
	lista_clase(Clase,KB,ListaClase),
	trae_valor_propiedad(Propiedad,ListaClase,Valor),
	Valor \= [desconocida],
	trae_prop_valor_clase(Propiedad,LC,KB,H),
	append([Valor],H,R);
	trae_prop_valor_clase(Propiedad,LC,KB,R).

trae_rel_valor_clase(_,[],_,[]).
trae_rel_valor_clase(Relacion,[Clase|LC],KB,R):-
	lista_clase(Clase,KB,ListaClase),
	trae_valor_relacion(Relacion,ListaClase,Valor),
	Valor \= [desconocida],
	trae_rel_valor_clase(Relacion,LC,KB,H),
	append([Valor],H,R);
	trae_rel_valor_clase(Relacion,LC,KB,R).

verificar_valor([],[[]]).
verificar_valor([desconocida],[]).
verificar_valor([H|T],[[H|T]]).
verificar_valor([A],[[A]]).
verificar_valor(X,[X]).

formato_valor([],desconocida).
formato_valor([[V|H]|[]],[V|H]).
formato_valor([V|[]],V).
formato_valor([V|H],[V|H]).

existe_relacion([],desconocida):- !.
existe_relacion([X|Y],[X|Y]).

acomoda_objetos(_,desconocida,desconocida):- !.
acomoda_objetos(_,[],[]).
acomoda_objetos(Rel,[[id=>X,_,[[Rel=>[Y|Z],_]]]|L],[X:[Y|Z]|R]):-
	acomoda_objetos(Rel,L,R),!.
acomoda_objetos(Rel,[[id=>X,_,[[Rel=>Y,_]]]|L],[X:[Y]|R]):-
	acomoda_objetos(Rel,L,R),!.
acomoda_objetos(Rel,[[id=>X,_,[[_=>_,_]]]|L],[X:[]|R]):-
	acomoda_objetos(Rel,L,R),!.
acomoda_objetos(Rel,[[id=>X,_,[]]|L],[X:[]|R]):-
	acomoda_objetos(Rel,L,R),!.
acomoda_objetos([_|L],R):-
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

eliminar_vacios(desconocida,desconocida).
eliminar_vacios([],[]).
eliminar_vacios([_:[]|L],R):-
	eliminar_vacios(L,R).
eliminar_vacios([A|L],[A|H]):-
	eliminar_vacios(L,H).

extension_relacion(Relacion,Extension):-
	open_kb('kb.txt',KB),
	todos_los_objetos(KB,Objetos),
	acomoda_objetos(Relacion,Objetos,Resultado),
	relaciones_de_clase(Relacion,KB,Resultado,Extension1),
	eliminar_vacios(Extension1,Extension2),
	existe_relacion(Extension2,Extension),!.

valor_propiedad_objeto(Objeto,Propiedad,Valor):-
	open_kb('kb.txt',KB),
	todos_los_objetos(KB,Objetos),
	trae_objeto(Objeto,Objetos,ListaObjeto),
	trae_valor_propiedad(Propiedad,ListaObjeto,Valor1),
	verificar_valor(Valor1,Valor2),
	clase_objeto(Objeto,Clases),
	trae_prop_valor_clase(Propiedad,Clases,KB,Valores),
	append(Valor2,Valores,ValorR),
	formato_valor(ValorR,Valor),!.

valor_relacion_objeto(Objeto,Relacion,Valor):-
	open_kb('kb.txt',KB),
	todos_los_objetos(KB,Objetos),
	trae_objeto(Objeto,Objetos,ListaObjeto),
	trae_valor_relacion(Relacion,ListaObjeto,Valor1),
	verificar_valor(Valor1,Valor2),
	clase_objeto(Objeto,Clases),
	trae_rel_valor_clase(Relacion,Clases,KB,Valores),
	append(Valor2,Valores,ValorR),
	formato_valor(ValorR,Valor),!.

%------------------------------
% 1(d) Todas las clases a las que pertenece un objeto
%------------------------------

%% Clases de un objeto
clase_objeto(Obj, Clases) :-
	open_kb('kb.txt',KB),
	clase_de_objeto(Obj,KB,ClaseActual),
	clases_padre(ClaseActual,KB,KB,ClasesPadre),
	append([ClaseActual],ClasesPadre,Clases).

%------------------------------
% 1(e) Todas las propiedades de un objeto o clase
%------------------------------

prop_objeto(_,[],desconocida):-!.

prop_objeto(Objeto,[class(_,_,_,_,Objetos)|_],Prop):-
	elemento_en_arreglo([id=>Objeto,Prop,_],Objetos),!.

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

rel_objeto(Objeto,[class(_,_,_,_,Objetos)|_],Rel):-
	elemento_en_arreglo([id=>Objeto,_,Rel],Objetos),!.

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

agregar_clase(Nueva_clase,Padre):-
	open_kb('kb.txt',KB),
	append(KB,[class(Nueva_clase,Padre,[],[],[])],Nueva_KB),
    save_kb('kb.txt',Nueva_KB).

%------------------------------
% 2(a) Agregar nuevo objeto:  
%------------------------------

agregar_objeto(Nuevo_objeto,Clase):-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objectos),class(Clase,Padre,Prop,Rel,Nuevos_objectos),KB,Nueva_KB),
	append(Objectos,[[id=>Nuevo_objeto,[],[]]],Nuevos_objectos),
    save_kb('kb.txt',Nueva_KB),!.

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
		NewKB),!,
    save_kb('kb.txt', NewKB).

appendProperty(ActualProperties, Name, si, NewProperties):-
	append(ActualProperties, [[Name, 0]], NewProperties).

appendProperty(ActualProperties, Name, no, NewProperties):-
	append(ActualProperties, [[not(Name), 0]], NewProperties).

appendProperty(ActualProperties, Name, Value, NewProperties):-
	append(ActualProperties, [[Name=>Value, 0]], NewProperties).

%------------------------------
% 2(b) Agregar nueva propiedad a un objeto:
%------------------------------
agregar_propiedad_objeto(Object, Name, Value) :-
    open_kb('kb.txt', ActualKB),
	changeElement(
		class(Class, Parent, P, R, Objects),
		class(Class, Parent, P, R, NewObjects),
		ActualKB,
		NewKB),
	existsElement([id=>Object, ActualProperties, Relations], Objects),
	changeElement(
		[id=>Object, ActualProperties, Relations],
		[id=>Object, NewProperties, Relations],
		Objects,
		NewObjects),
	appendProperty(ActualProperties, Name, Value, NewProperties),
    save_kb('kb.txt', NewKB),!.

%------------------------------
% 2(c) Agregar nueva relacion de clase:  
% Cuando sea not agregar si el parentesis
% Agregar nueva propiedad a una clase:
%------------------------------
agregar_relacion_clase(Clase1,Clase2,Nueva_rel):-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase1,Padre,Prop,Rel,Objetos),class(Clase1,Padre,Prop,NRel,Objetos),KB,Nueva_KB),
	append_relacion(Rel,Nueva_rel,Clase2,NRel),
    save_kb('kb.txt',Nueva_KB),!.

agregar_preferencia_relacion_clase(Clase,Nueva_pref,Peso):-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,NRel,Objetos),KB,Nueva_KB),
	append_preferencia(Rel,Nueva_pref,Peso,NRel),
	save_kb('kb.txt',Nueva_KB).

%------------------------------
% 2(c) Agregar nueva relacion de objeto:  
%------------------------------
agregar_relacion_objeto(Objeto1,Objeto2,Nueva_rel) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,Rel,NObjetos),KB,Nueva_KB),
	verifica_elemento([id=>Objeto1,Propiedades,Relaciones],Objetos),
	cambiar_elemento([id=>Objeto1,Propiedades,Relaciones],[id=>Objeto1,Propiedades,Nuevas_rel],Objetos,NObjetos),
	append_relacion(Relaciones,Nueva_rel,Objeto2,Nuevas_rel),
	save_kb('kb.txt',Nueva_KB),!.

agregar_preferencia_relacion_objeto(Objeto,Nueva_pref,Peso) :-
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

eliminar_clase(Clase) :-
	open_kb('kb.txt',KB),
    eliminar_elemento(class(Clase,Padre,_,_,_),KB,KBAux),
	cambiar_padre(Clase,Padre,KBAux,KBAux2),
	delete_relations_with_object(Clase,KBAux2,KBFinal),%411 Esto es del repo
	save_kb('kb.txt',KBFinal),!.

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

cancel_relation(Object,[H|T],[H|NewT]):-
	cancel_relation(Object,T,NewT).

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
eliminar_objeto(Objeto) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,Rel,ObjetoNuevo),KB,KBAux),
	elemento_en_arreglo([id=>Objeto|Propiedades],Objetos),
	eliminar_elemento([id=>Objeto|Propiedades],Objetos,ObjetoNuevo),
	delete_relations_with_object(Objeto,KBAux,KBFinal),%365 codigo del repo
	save_kb('kb.txt',KBFinal),!.

%------------------------------
% 3(b) Eliminar valor de una propiedad
%------------------------------
eliminar_propiedad_clase(Class, Property):-
    open_kb('kb.txt', ActualKB),
    findProperty(ActualKB, Class, ActualProperties),
	deleteAllWithProperty(Property, ActualProperties, Aux),
	deleteElement([not(Property), _], Aux, Aux2),
	deleteElement([Property, _], Aux2, NewProperties),
	replaceAll(
		class(Class, P, ActualProperties, R, O),
		class(Class, P, NewProperties, R, O),
		ActualKB,
		NewKB),!,
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
    changeElement(
		class(Class, Parent, Props, Rels, Objects),
		class(Class, Parent, Props, Rels, NewObjects),
		ActualKB,
		NewKB),
	existsElement([id=>Object, Properties, Relations], Objects),
	changeElement([id=>Object, Properties, Relations], [id=>Object, NewProperties, Relations], Objects, NewObjects),
	deleteAllElementsWithSameProperty(Property, Properties, Aux),
	deleteElement([not(Property), _], Aux, Aux2),
	deleteElement([Property, _], Aux2, NewProperties),
    save_kb('kb.txt', NewKB),!.

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

eliminar_elementos_misma_prop_negada(X,[not(X)=>_|L],R):-
	eliminar_elementos_misma_prop_negada(X,L,R).

eliminar_elementos_misma_prop_negada(X,[[not(X)=>_,_]|L],R):-
	eliminar_elementos_misma_prop_negada(X,L,R).

eliminar_elementos_misma_prop_negada(X,[Y|L],[Y|R]):-
	eliminar_elementos_misma_prop_negada(X,L,R).



eliminar_relacion_clase(Clase,not(Relacion)):-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,NuevasRels,Objectos),KB,Nueva_KB),
	eliminar_elementos_misma_prop_negada(Relacion,Rels,NuevasRels),
	save_kb('kb.txt', Nueva_KB),!.

eliminar_relacion_clase(Clase,Relacion):-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,NuevasRels,Objectos),KB,Nueva_KB),
	eliminar_elementos_misma_prop(Relacion,Rels,NuevasRels),
	save_kb('kb.txt', Nueva_KB),!.

%-------------------------------------
% 3(c) Eliminar una relación de un objeto:  
%-------------------------------------

eliminar_relacion_objeto(Objecto,not(Relacion)):-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,Rels,NuevosObjectos),KB,Nueva_KB),
	es_elemento([id=>Objecto,Propiedades,Relaciones],Objectos),
	cambiar_elemento([id=>Objecto,Propiedades,Relaciones],[id=>Objecto,Propiedades,NuevasRelaciones],Objectos,NuevosObjectos),
	eliminar_elementos_misma_prop_negada(Relacion,Relaciones,NuevasRelaciones),
	save_kb('kb.txt',Nueva_KB).

eliminar_relacion_objeto(Objecto,Relacion):-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Props,Rels,Objectos),class(Clase,Padre,Props,Rels,NuevosObjectos),KB,Nueva_KB),
	es_elemento([id=>Objecto,Propiedades,Relaciones],Objectos),
	cambiar_elemento([id=>Objecto,Propiedades,Relaciones],[id=>Objecto,Propiedades,NuevasRelaciones],Objectos,NuevosObjectos),
	eliminar_elementos_misma_prop(Relacion,Relaciones,NuevasRelaciones),
	save_kb('kb.txt',Nueva_KB).

%------------------------------
% 4(a) Modificar el nombre de una clase:  
%------------------------------
cambiar_nombre_clase(Clase,Nuevo_nombre) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Nuevo_nombre,Padre,Prop,Rel,Objetos),KB,Tmp_KB),
	cambia_herencia(Clase,Nuevo_nombre,Tmp_KB,_),
	cambiar_relaciones_con_objeto(Clase,Nuevo_nombre,Tmp_KB,Nueva_KB),
	save_kb('kb.txt',Nueva_KB),!.

%------------------------------
% 4(a) Modificar el nombre de un objeto:  
%------------------------------
cambiar_nombre_objeto(Objeto,Nuevo_nombre) :-
	open_kb('kb.txt',KB),
	cambiar_elemento(class(Clase,Padre,Prop,Rel,Objetos),class(Clase,Padre,Prop,Rel,NObjetos),KB,Tmp_KB),
	verifica_elemento([id=>Objeto|Propiedades],Objetos),
	cambiar_elemento([id=>Objeto|Propiedades],[id=>Nuevo_nombre|Propiedades],Objetos,NObjetos),
	cambiar_relaciones_con_objeto(Objeto,Nuevo_nombre,Tmp_KB,Nueva_KB),
	save_kb('kb.txt',Nueva_KB),!.

%------------------------------				
% 4(b) Modificar el valor de una propiedad específica de un objeto 
%------------------------------
cambiar_valor_propiedad_objeto(Objeto,Propiedad,Nuevo_valor):-
	eliminar_propiedad_objeto(Objeto,Propiedad),
	agregar_propiedad_objeto(Objeto,Propiedad,Nuevo_valor).
	
%------------------------------
% 4(b) Modificar el valor de una propiedad específica de una clase 
% Hacerlo mas general
%------------------------------
cambiar_valor_propiedad_clase(Clase,Propiedad,Nuevo_valor):-
	eliminar_propiedad_clase(Clase,Propiedad),
	agregar_propiedad_clase(Clase,Propiedad,Nuevo_valor).

%------------------------------
% 4(c) Modificar con quien tiene una relacion específica una clase 
%------------------------------
cambiar_valor_relacion_objeto(Objeto,Relacion,Nuevo_objeto_relacionado):-
	open_kb('kb.txt',KB),
	%verifica_objeto_lista(Nuevo_objeto_relacionado,KB,si),
	elimina_relacion_objeto(Objeto,Relacion,KB,TemporalKB),
	agrega_relacion_objeto(Objeto,Relacion,Nuevo_objeto_relacionado,TemporalKB,Nueva_KB),
	save_kb('kb.txt',Nueva_KB),!.


%------------------------------
% 4(c) Modificar con quien tiene una relacion específica una clase 
%------------------------------
cambiar_valor_relacion_clase(Clase,Relacion,Nuevo_valor):-
	eliminar_relacion_clase(Clase,Relacion),
	agregar_relacion_clase(Clase,Nuevo_valor,Relacion),!.
