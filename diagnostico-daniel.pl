%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diagnostico(MissedObject,Diagnosis):-
	% Entrada:
	%	ObjetoPerdido -> Objeto perdido que activó el CICLO DE INFERENCIA cuando el robot no lo vio el estante actual para agarrarlo 
	% Salida:
	%	Diagnostico -> Lista de acciones de mover y colocar
	
	open_kb('kb.txt',KB),

	nl,nl,write('Diagnostico:'),

	% valor_propiedad_objeto(Objeto,Propiedad,Valor)
	valor_propiedad_objeto(golem,position,CurrentShelf),
	write('Estante actual : '),write(CurrentShelf),nl,

	% Objetos perdidos en el estante en relación con el contenido de la creencia
	% MissingBelieved = = Creencia - Observada
	valor_propiedad_objeto(CurrentShelf,belief,Belief),
	write('Creencia : '),write(Belief),nl,
	valor_propiedad_objeto(CurrentShelf,observed_objects,Observed),
	write('Observed : '),write(Observed),nl,
	restar_lista(Observed,Belief,MissingBelieved), % NO SE PUDO CONSEGUIR MissingBelieved por la KB
	%write('Objetos que faltan en el estante actual en relación con el contenido de la creencia: '),write(MissingBelieved),nl,

	% Obtiene la lista de estantes no observados 
	objetos_de_clase(shelf,KB,AllShelvesInKB),
	valor_propiedad_objeto(observed_shelves,list,AllPreviousObservedShelves),
	restar_lista(AllPreviousObservedShelves,AllShelvesInKB,UnobservedShelves),
	write('Estantes no vistos en este momento : '),write(UnobservedShelves),nl,

	% Cargar información sobre acciones de movimiento como predicados 
	cargar_movimiento(KB),
    cargar_toma_acciones(KB),
	cargar_acciones(KB),
	
	write('Se cargaron todas las acciones de movimiento, toma y entrega '),nl,

	% Obtiene la distancia desde el estante actual al resto de estantes 
	estante_noobservado_sercano(CurrentShelf,AllPreviousObservedShelves,OrderedUnobservedShelves),
	write('Los estantes no observados en este momento ordenados del más cercano al más lejano : '),write(OrderedUnobservedShelves),nl,

	% Asigna el objeto perdido en el estante más cercano
	perdida_cercana(KB,OrderedUnobservedShelves,MissedObject,NewKB1),

	% Eliminar el objeto perdido de la lista de MissingBelieved para obtener el resto de los objetos 
	% La reasignación se realiza de forma aleatoria en los estantes no observados 
	restar_lista([MissedObject],MissingBelieved,ObjectsToReasign),
	reasignar_creencias(NewKB1,ObjectsToReasign,OrderedUnobservedShelves,NewKB2),

	% Resta los objetos observados de las creencias de todos los estantes no observados 
	restar_observado_creencias(NewKB2,UnobservedShelves,Observed,NewKB),	

	% Genera el diagnóstico 
	recolectar_todas_observaciones(AllPreviousObservedShelves,[],ListObservations,NewKB),
	recolectar_creencias(UnobservedShelves,[],ListBeliefs,NewKB),
	append(ListObservations,ListBeliefs,Diagnosis),
	save_kb('kb.txt',NewKB),
	write(Diagnosis).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Carga la información de todas las acciones de movimiento como predicados en la forma:  move_action_dlic(Origin,Destiny,Cost,Probability)
cargar_movimiento(KB):-
	objetos_de_clase(move,KB,AllMovesKB),
	cargar_movimientos(AllMovesKB,KB).

cargar_movimientos([],_).
	
cargar_movimientos([H|T],KB):-
	% valor_relacion_objeto(Objeto,Relacion,Valor)
	valor_relacion_objeto(H,from,From),
	valor_relacion_objeto(H,to,To),
	valor_propiedad_objeto(H,cost,Cost),
	valor_propiedad_objeto(H,probability,Probability),
	% https://www.swi-prolog.org/pldoc/man?predicate=assert/1
	assert(probabilidad_movimiento(From,To,Cost,Probability)),
	cargar_movimientos(T,KB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cargar información de todas las acciones realizadas como predicados en la forma:  take_action_dlic(Object,Cost,Probability)
cargar_toma_acciones(KB):-
	objetos_de_clase(take,KB,AllTakesKB),
	cargar_toma_accion(AllTakesKB,KB).

cargar_toma_accion([],_).
	
cargar_toma_accion([H|T],KB):-
	valor_relacion_objeto(H,to,Object),
	valor_propiedad_objeto(H,cost,Cost),
	valor_propiedad_objeto(H,probability,Probability),
	assert(probabilidad_decision(Object,Cost,Probability)),
	cargar_toma_accion(T,KB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Cargar información de todas las acciones de entrega como predicados en la forma:  deliver_action_dlic(Object,Probability)
cargar_acciones(KB):-
	objetos_de_clase(deliver,KB,AllDeliversKB),
	cargar_accion(AllDeliversKB,KB).

cargar_accion([],_).
	
cargar_accion([H|T],KB):-
	valor_relacion_objeto(H,to,Object),
	valor_propiedad_objeto(H,cost,Cost),
	valor_propiedad_objeto(H,probability,Probability),
	assert(probabilidad_accion(Object,Cost,Probability)),
	cargar_accion(T,KB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Obtiene el estante no observado más cercano 
estante_noobservado_sercano(CurrentShelf,AllPreviousObservedShelves,OrderedUnobservedShelves):-
	% Obtiene la lista en orden desde el estante actual hasta el resto del estante
	% https://www.swi-prolog.org/pldoc/man?predicate=setof/3
	setof(Y=>X,move_action_dlic(CurrentShelf,X,Y,_),DistanceToShelves),
	% Eliminar distancias para obtener la lista solo con los identificadores 
	estante_lista_estante_noobservado(DistanceToShelves,[],NewList),
	% Eliminar el inicio de la lista 
	restar_lista([start],NewList,NewList2),
	% Retira todos los estantes observados anteriormente 
	restar_lista(AllPreviousObservedShelves,NewList2,OrderedUnobservedShelves).			

estante_lista_estante_noobservado([],X,X).

estante_lista_estante_noobservado([_=>Y|T],PreviousList,NewList):-
	% https://www.swi-prolog.org/pldoc/man?predicate=append/3
	append(PreviousList,[Y],AuxList),
	estante_lista_estante_noobservado(T,AuxList,NewList).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Asigne el objeto perdido en el estante no observado más cercano, en caso de que no haya más estantes, se registra como perdido 
perdida_cercana(KB,[],MissedObject,NewKB):-
	% cambiar_valor_relacion_objeto(Objeto,Relacion,Nuevo_objeto_relacionado)
	cambiar_valor_relacion_objeto(MissedObject,last_corroborated_position,lost),
	write(MissedObject),write('fue registrado como perdido '),nl.
	
perdida_cercana(KB,[H|_],MissedObject,NewKB2):-
	valor_propiedad_objeto(H,belief,Belief),
	append(Belief,[MissedObject],NewBelief),
	% cambiar_valor_propiedad_objeto(Objeto,Propiedad,Nuevo_valor)
	cambiar_valor_propiedad_objeto(H,belief,NewBelief),
	write(MissedObject),write(' ahora se cree que está en  '),write(H),nl,
	valor_propiedad_objeto(golem,position,RobotCurrentPosition),
	% agregar_relacion_objeto(Objeto1,Objeto2,Nueva_rel)
	agregar_relacion_objeto(RobotCurrentPosition,nearest_shelf,H),
	write(H),write(' se registró en KB como el estante más cercano a  '),write(RobotCurrentPosition),nl. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Modificar creencias sobre objetos perdidos pasándolos aleatoriamente en los estantes no observados 
reasignar_creencias(KB,[],_,KB).

reasignar_creencias(KB,[H|T],OrderedUnobservedShelves,NewKB):-
	reasignar_creencias2(KB,H,OrderedUnobservedShelves,KBAux),
	reasignar_creencias(KBAux,T,OrderedUnobservedShelves,NewKB).

reasignar_creencias2(KB,H,[],NewKB):-
	cambiar_valor_relacion_objeto(H,last_corroborated_position,lost),
	write(H),write(' fue registrado como perdido '),nl. 
	
reasignar_creencias2(KB,H,Shelves,NewKB):-
	% https://www.swi-prolog.org/pldoc/man?predicate=random_permutation/2
	random_permutation(Shelves,[RandomShelf|_]),
	valor_propiedad_objeto(RandomShelf,belief,Belief),
	append(Belief,[H],NewBelief),
	cambiar_valor_propiedad_objeto(RandomShelf,belief,NewBelief),
	write(H),write(' ahora se cree que está en  '),write(RandomShelf),nl. 
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Resta el objeto observado de las creencias de los estantes no observados 
restar_observado_creencias(X,[],_,X).

restar_observado_creencias(KB,[Shelf|T],Observed,NewKB):-
	valor_propiedad_objeto(Shelf,belief,BelievedObjects),
	restar_lista(Observed,BelievedObjects,NewBelief),	
	cambiar_valor_propiedad_objeto(Shelf,belief,NewBelief),
	restar_observado_creencias(AuxKB,T,Observed,NewKB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generando el diagnóstico 
recolectar_todas_observaciones([],X,X,_).
	
recolectar_todas_observaciones([Shelf|T],List,NewList,KB):-
	append(List,[move(Shelf)],AuxList),
	valor_propiedad_objeto(Shelf,observed_objects,ObservedObjects),
	append(AuxList,[place(ObservedObjects)],AuxList2),
	recolectar_todas_observaciones(T,AuxList2,NewList,KB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recolectar_creencias([],X,X,_).

recolectar_creencias([Shelf|T],List,NewList,KB):-
	append(List,[move(Shelf)],AuxList),
	valor_propiedad_objeto(Shelf,belief,BelievedObjects),
	append(AuxList,[place(BelievedObjects)],AuxList2),
	recolectar_creencias(T,AuxList2,NewList,KB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Repo 1282
objetos_de_clase(Class,KB,Objects):-
	there_is_class(Class,KB,yes),
	objects_only_in_the_class(Class,KB,ObjectsInClass),
	descendants_of_a_class(Class,KB,Sons),
	objects_of_all_descendants_classes(Sons,KB,DescendantObjects),
	append(ObjectsInClass,DescendantObjects,Objects),!.

objetos_de_clase(_,_,unknown).


there_is_class(_,[],unknown):-!.

there_is_class(Class,[class(not(Class),_,_,_,_)|_],no):-!.

there_is_class(Class,[class(Class,_,_,_,_)|_],yes):-!.

there_is_class(Class,[_|T],Answer):-
	there_is_class(Class,T,Answer).


objects_only_in_the_class(_,[],unknown):-!.

objects_only_in_the_class(Class,[class(Class,_,_,_,O)|_],Objects):-
	extract_objects_names(O,Objects),!.

objects_only_in_the_class(Class,[_|T],Objects):-
	objects_only_in_the_class(Class,T,Objects),!.
	

extract_objects_names([],[]):-!.

extract_objects_names([[id=>Name,_,_]|T],Objects):-
	extract_objects_names(T,Rest),
	append([Name],Rest,Objects).


descendants_of_a_class(Class,KB,Descendants):-
	there_is_class(Class,KB,yes),
	sons_of_a_class(Class,KB,Sons),
	all_descendants_of_a_class(Sons,KB,Descendants),!.

descendants_of_a_class(_,_,unknown).


all_descendants_of_a_class([],_,[]).

all_descendants_of_a_class(Classes,KB,Descendants):-
	sons_of_a_list_of_classes(Classes,KB,Sons),
	all_descendants_of_a_class(Sons,KB,RestOfDescendants),!,
	append(Classes,RestOfDescendants,Descendants).


objects_of_all_descendants_classes([],_,[]).

objects_of_all_descendants_classes([Class|T],KB,AllObjects):-
	objects_only_in_the_class(Class,KB,Objects),
	objects_of_all_descendants_classes(T,KB,Rest),
	append(Objects,Rest,AllObjects),!.


sons_of_a_class(_,[],[]).

sons_of_a_class(Class,[class(Son,Class,_,_,_)|T],Sons):-
	sons_of_a_class(Class,T,Brothers),!,	
	append([Son],Brothers,Sons).

sons_of_a_class(Class,[_|T],Sons):-
	sons_of_a_class(Class,T,Sons).	


sons_of_a_list_of_classes([],_,[]).

sons_of_a_list_of_classes([Son|T],KB,Grandsons):-
	sons_of_a_class(Son,KB,Sons),
	sons_of_a_list_of_classes(T,KB,Cousins),
	append(Sons,Cousins,Grandsons).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Repo 83 nueva observacion
restar_lista([],L1,L1).

restar_lista([H|T],L1,NewL1):-
	deleteElement(H,L1,LAux),
	restar_lista(T,LAux,NewL1).

% Repo 102
deleteElement(_,[],[]).

deleteElement(X,[X|T],N):-
	deleteElement(X,T,N).

deleteElement(X,[H|T],[H|N]):-
	deleteElement(X,T,N),
	X\=H.