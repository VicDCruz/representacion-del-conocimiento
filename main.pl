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
% Agregar nueva propiedad a una clase:
%------------------------------
addClassProperty(Class, Name, Value):-
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
    save_kb('nueva_kb.txt', NewKB).

appendProperty(ActualProperties, Name, yes, NewProperties):-
	append(ActualProperties, [[Name, 0]], NewProperties).

appendProperty(ActualProperties, Name, no, NewProperties):-
	append(ActualProperties, [[not(Name), 0]], NewProperties).

appendProperty(ActualProperties, Name, Value, NewProperties):-
	append(ActualProperties, [[Name=>Value, 0]], NewProperties).

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

%------------------------------
% Agregar nueva propiedad a un objeto:
%------------------------------
addObjectProperty(Object, NewProperty, Value) :-
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
    save_kb('nueva_kb.txt', NewKB).

%------------------------------
% Eliminar valor de una propiedad
%------------------------------
deleteClassProperty(Class, Property):-
    open_kb('kb.txt', ActualKB),
    findProperty(ActualKB, Class, Properties),
	deleteAllWithProperty(Property, Properties, Aux),
	deleteElement([not(Property), _], Aux, Aux2),
	deleteElement([Property, _], Aux2, NewProperties),
    save_kb('nueva_kb.txt', NewKB).

deleteAllWithProperty(_, [], []).

deleteAllWithProperty(X, [[X=>_,_]|T], N):-
	deleteAllWithProperty(X, T, N).

deleteAllWithProperty(X, [H|T], [H|N]):-
	deleteAllWithProperty(X, T, N).

%------------------------------
% Eliminar valor de un objeto
%------------------------------
deleteObjectProperty(Object, Property):-
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
    save_kb('nueva_kb.txt', NewKB).

%------------------------------
% Consultar extension de propiedad
%------------------------------
getExtensionProperty(Property, Result):-
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
% Agregar nueva propiedad a una clase:
%------------------------------
addClassProperty(Class, Name, Value):-
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
    save_kb('nueva_kb.txt', NewKB).

appendProperty(ActualProperties, Name, yes, NewProperties):-
	append(ActualProperties, [[Name, 0]], NewProperties).

appendProperty(ActualProperties, Name, no, NewProperties):-
	append(ActualProperties, [[not(Name), 0]], NewProperties).

appendProperty(ActualProperties, Name, Value, NewProperties):-
	append(ActualProperties, [[Name=>Value, 0]], NewProperties).

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

%------------------------------
% Agregar nueva propiedad a un objeto:
%------------------------------
addObjectProperty(Object, NewProperty, Value) :-
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
    save_kb('nueva_kb.txt', NewKB).

%------------------------------
% Eliminar valor de una propiedad
%------------------------------
deleteClassProperty(Class, Property):-
    open_kb('kb.txt', ActualKB),
    findProperty(ActualKB, Class, Properties),
	deleteAllWithProperty(Property, Properties, Aux),
	deleteElement([not(Property), _], Aux, Aux2),
	deleteElement([Property, _], Aux2, NewProperties),
    save_kb('nueva_kb.txt', NewKB).

deleteAllWithProperty(_, [], []).

deleteAllWithProperty(X, [[X=>_,_]|T], N):-
	deleteAllWithProperty(X, T, N).

deleteAllWithProperty(X, [H|T], [H|N]):-
	deleteAllWithProperty(X, T, N).

%------------------------------
% Eliminar valor de un objeto
%------------------------------
deleteObjectProperty(Object, Property):-
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
    save_kb('nueva_kb.txt', NewKB).

%------------------------------
% Consultar extension de propiedad
%------------------------------
getExtensionProperty(Property, Result):-
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
