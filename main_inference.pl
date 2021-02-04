%----------------------------
% Modulo de CICLO INFERENCIA
%----------------------------

ciclo_inferencia(ObjetoFaltante,Diagnostico,Decision,Plan):-
	realizar_diagnostico(ObjetoFaltante,Diagnostico),
	tomar_decision(Decision),
	ejecutar_plan(Diagnostico,Decision,Plan).

%----------------------
% Modulo de DIAGNOSTICO
%----------------------

realizar_diagnostico(MissedObject,Diagnosis):-
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
	resta_de_listas(Observed,Belief,MissingBelieved), % NO SE PUDO CONSEGUIR MissingBelieved por la KB
	%write('Objetos que faltan en el estante actual en relación con el contenido de la creencia: '),write(MissingBelieved),nl,

	% Obtiene la lista de estantes no observados 
	objetos_de_clase(shelf,KB,AllShelvesInKB),
	valor_propiedad_objeto(observed_shelves,list,AllPreviousObservedShelves),
	resta_de_listas(AllPreviousObservedShelves,AllShelvesInKB,UnobservedShelves),
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
	perdida_cercana(OrderedUnobservedShelves,MissedObject),

	% Eliminar el objeto perdido de la lista de MissingBelieved para obtener el resto de los objetos 
	% La reasignación se realiza de forma aleatoria en los estantes no observados 
	resta_de_listas([MissedObject],MissingBelieved,ObjectsToReasign),
	reasignar_creencias(ObjectsToReasign,OrderedUnobservedShelves),

	% Resta los objetos observados de las creencias de todos los estantes no observados 
	restar_observado_creencias(UnobservedShelves,Observed),	

	% Genera el diagnóstico 
	recolectar_todas_observaciones(AllPreviousObservedShelves,[],ListObservations,NewKB),
	recolectar_creencias(UnobservedShelves,[],ListBeliefs,NewKB),
	append(ListObservations,ListBeliefs,Diagnosis),
	write(Diagnosis).
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Carga la información de todas las acciones de movimiento como predicados en la forma:  probabilidad_movimiento(Origin,Destiny,Cost,Probability)
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

% Cargar información de todas las acciones realizadas como predicados en la forma:  probabilidad_decision(Object,Cost,Probability)
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

% Cargar información de todas las acciones de entrega como predicados en la forma:  probabilidad_accion(Object,Probability)
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
	setof(Y=>X,probabilidad_movimiento(CurrentShelf,X,Y,_),DistanceToShelves),
	% Eliminar distancias para obtener la lista solo con los identificadores 
	estante_lista_estante_noobservado(DistanceToShelves,[],NewList),
	% Eliminar el inicio de la lista 
	resta_de_listas([start],NewList,NewList2),
	% Retira todos los estantes observados anteriormente 
	resta_de_listas(AllPreviousObservedShelves,NewList2,OrderedUnobservedShelves).			

estante_lista_estante_noobservado([],X,X).

estante_lista_estante_noobservado([_=>Y|T],PreviousList,NewList):-
	% https://www.swi-prolog.org/pldoc/man?predicate=append/3
	append(PreviousList,[Y],AuxList),
	estante_lista_estante_noobservado(T,AuxList,NewList).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Asigne el objeto perdido en el estante no observado más cercano, en caso de que no haya más estantes, se registra como perdido 
perdida_cercana([],MissedObject):-
	% cambiar_valor_relacion_objeto(Objeto,Relacion,Nuevo_objeto_relacionado)
	cambiar_valor_relacion_objeto(MissedObject,last_corroborated_position,lost),
	write(MissedObject),write('fue registrado como perdido '),nl.
	
perdida_cercana([H|_],MissedObject):-
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
reasignar_creencias([],_).

reasignar_creencias([H|T],OrderedUnobservedShelves):-
	reasignar_creencias2(H,OrderedUnobservedShelves),
	reasignar_creencias(T,OrderedUnobservedShelves).

reasignar_creencias2(H,[]):-
	cambiar_valor_relacion_objeto(H,last_corroborated_position,lost),
	write(H),write(' fue registrado como perdido '),nl. 
	
reasignar_creencias2(H,Shelves):-
	% https://www.swi-prolog.org/pldoc/man?predicate=random_permutation/2
	random_permutation(Shelves,[RandomShelf|_]),
	valor_propiedad_objeto(RandomShelf,belief,Belief),
	append(Belief,[H],NewBelief),
	cambiar_valor_propiedad_objeto(RandomShelf,belief,NewBelief),
	write(H),write(' ahora se cree que está en  '),write(RandomShelf),nl. 
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Resta el objeto observado de las creencias de los estantes no observados 
restar_observado_creencias([],_).

restar_observado_creencias([Shelf|T],Observed):-
	valor_propiedad_objeto(Shelf,belief,BelievedObjects),
	resta_de_listas(Observed,BelievedObjects,NewBelief),	
	cambiar_valor_propiedad_objeto(Shelf,belief,NewBelief),
	restar_observado_creencias(T,Observed).

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



%--------------------------
% Modulo de TOMAR DECISION
%--------------------------

tomar_decision(Decision):-
	%Lista de objetos mal colocados
    valor_propiedad_objeto(pending_tasks,list,ObjetosMalColocados),
	nl,write('Objetos mal colocados: '),write(ObjetosMalColocados),nl, 

	%Lista de las ordenes de los clientes
	valor_propiedad_objeto(pending_client_orders,list,OrdenesClientes),
	write('Objetos ordenados por cliente: '),write(OrdenesClientes),nl,

	%Lista de objetos a reubicar
	resta_de_listas(OrdenesClientes,ObjetosMalColocados,ObjectosParaReubicar),
	write('Objectos para reubicar: '),write(ObjectosParaReubicar),nl,

	%Conjunto potencia de objetos a reubicar
	conjunto_potencia(ObjectosParaReubicar,_,ConjuntoPotencia),
	write('Posibles elecciones de objetos a reubicar: '),write(ConjuntoPotencia),nl,

    % Asignar un valor a cada subconjunto posible de objetos reubicados
	valuar_posibles_reubicaciones(ConjuntoPotencia,ConjuntoValuado),
	sort(ConjuntoValuado,ConjuntoOrdenado),
	reverse(ConjuntoOrdenado,ConjuntoInvertido),
	write('Posibles elecciones con valor de objetos a reubicar: '),write(ConjuntoInvertido),nl,

	% Tomar decision final
	decision_final(OrdenesClientes,ConjuntoInvertido,Decision),
	write('Decision: '),write(Decision),nl,!. 




conjunto_potencia([],X,X).

conjunto_potencia([H|T],[],NuevaLista):-
	conjunto_potencia(T,[[],[H]],NuevaLista).

conjunto_potencia([H|T],List,NuevaLista):-
	agregar_elemento_a_lista(H,List,L1),
	append(List,L1,L2),
	conjunto_potencia(T,L2,NuevaLista).

agregar_elemento_a_lista(_,[],[]).

agregar_elemento_a_lista(X,[H|T],[R1|R]):-
	append(H,[X],R1),
	agregar_elemento_a_lista(X,T,R).

valuar_posibles_reubicaciones([],[]).

valuar_posibles_reubicaciones([H|T],[Val=>H|L]):-
	write('Evaluando '),write(H),
	evaluar_opcion(H,Val),
	write(' Valor: '),write(Val),nl,
	valuar_posibles_reubicaciones(T,L).

evaluar_opcion([],0).

evaluar_opcion([H|T],NuevoValor):-
	%Probabilidad de tomar el objeto actual
	probabilidad_decision(H,_,Probabilidad),

	%Verificar si realinear el objeto es buena idea (0/1)
	verifica_accion(H,Resultado),
	
	evaluar_opcion(T,Val),
    NuevoValor is Val + ((Probabilidad/100)*Resultado).	


%Realinear el objeto es buena idea si:
%El objeto a realinear está en el estante actual
%La posicion del estante correcto del objeto es el siguiente estante en ser visitado (el mas cercano)

verifica_accion(Objecto,Valor):-
	valor_relacion_objeto(Objecto,last_corroborated_position,ObjetoEstanteActual),
	valor_relacion_objeto(Objecto,associated_shelf,ObjetoEstanteCorrecto),	
	valor_propiedad_objeto(golem,position,PosicionActual),
	valor_relacion_objeto(PosicionActual,nearest_shelf,EstanteMasCercano),
	evaluar_cambio_posicion(PosicionActual,EstanteMasCercano,ObjetoEstanteActual,ObjetoEstanteCorrecto,Valor).

evaluar_cambio_posicion(X,Y,X,Y,1).

evaluar_cambio_posicion(_,_,_,_,-1).

%Dar decision final

decision_final(OrdenesClientes,[_=>ReacomodarObjetos|_],Decision):-
	traer_lista(OrdenesClientes,L1),
	reacomodar_lista(ReacomodarObjetos,L2),
	append(L1,L2,Decision).
	
traer_lista([],[]).

traer_lista([H|T],Resultado):-
	traer_lista(T,L),
	append([bring(H)],L,Resultado).

reacomodar_lista([],[]).

reacomodar_lista([H|T],Resultado):-
	reacomodar_lista(T,L),
	append([rearrange(H)],L,Resultado).

resta_de_listas([],L,L).

resta_de_listas([H|T],L1,R):-
	eliminar_elemento(H,L1,L2),
	resta_de_listas(T,L2,R).

%------------------------
% Modulo EJECUTAR PLAN
%------------------------

% Ver si hay objeto
hay_objeto(_, [], unknown):-!.

hay_objeto(Object, [class(_, _, _, _, O)|_], no):-
	esElemento([id=>not(Object), _, _], O).

hay_objeto(Object,[class(_, _, _, _, O)|_], yes):-
	esElemento([id=>Object, _, _], O).

hay_objeto(Object, [_|T], Answer):-
	hay_objeto(Object, T, Answer),!.

hay_objeto_list(Object, KB, Ans):-
	hay_objeto(Object, KB, Ans),!.
hay_objeto_list([], _, yes):-!.
hay_objeto_list([H|_], KB, unknown):-
	hay_objeto(H, KB, unknown).
hay_objeto_list([H|_], KB, no):-
	hay_objeto(H, KB, no).
hay_objeto_list([H|T], KB, Ans):-
	hay_objeto(H, KB, yes),
	hay_objeto_list(T, KB, Ans).

% Encontrar la relacion de una lista
obtener_valor_relacion(not(Relation), Relations, Value):-
	obtener_valor_relacion_negativa(Relation, Relations, Value).

obtener_valor_relacion(Relation, Relations, Value):-
	obtener_valor_relacion_positiva(Relation, Relations, Value).


obtener_valor_relacion_negativa(_, [], unknown).

obtener_valor_relacion_negativa(Attribute, [not(Attribute=>Value)|_], Value).

obtener_valor_relacion_negativa(Attribute, [_|T], Value):-
	obtener_valor_relacion_negativa(Attribute, T, Value).


obtener_valor_relacion_positiva(_, [], unknown).

obtener_valor_relacion_positiva(Attribute, [Attribute=>Value|_], Value).

obtener_valor_relacion_positiva(Attribute, [_|T], Value):-
	obtener_valor_relacion_positiva(Attribute, T, Value).

% obtener valor de una relacion de un objeto

obtener_relacion_objeto(Object, Relation, KB, Value):-
	hay_objeto(Object, KB, yes),
	rel_objeto(Object, Relations),
	obtener_valor_relacion(Relation, Relations, Value).

obtener_relacion_objeto(_, _, _, unknown).

% Ver si elemento existe
esElemento(X,[X|_]).
esElemento(X,[_|T]):-
	esElemento(X,T).

ejecutar_plan(Diagnostico, Decisions, Plan):-
	open_kb('kb.txt', KB),
	nl,nl,write('=== Inferencia de planeación ==='),nl,

	valor_propiedad_objeto(golem, position, Posicion),
	write('El robot esta en: '),write(Posicion),nl,

	valor_propiedad_objeto(golem, right_arm, BrazoDerecho),
	write('El brazo derecho tiene: '),write(BrazoDerecho),nl,

	valor_propiedad_objeto(golem, left_arm, BrazoIzquierdo),
	write('El brazo izquierdo tiene: '),write(BrazoIzquierdo),nl,

	write('Decisiones por tomar: '),write(Decisions),nl,
	traducir_decisiones_acciones(KB, Diagnostico, Decisions, BrazoDerecho, BrazoIzquierdo, BasicActions),	
	write('Acciones por hacer: '),write(BasicActions),nl,

	%Generar todos los posibles planes
	setof(X, permutation(BasicActions, X), AllPlans),
	nl,write('Posibles planes: '),nl,
	
	%Obtener planeas validos
	filtrar_planes(AllPlans, Posicion, BrazoDerecho, BrazoIzquierdo, FilteredPlans),

	%Ordenar y obtener el mejor plan
	sort(FilteredPlans,[_=>Plan|_]),
	nl,write('Plan seleccionado: '),write(Plan),nl.

% Traducir decisiones a acciones

traducir_decisiones_acciones(_, _, [], _, _, []).

% Objeto en brazo derecho
traducir_decisiones_acciones(KB, Diagnostico, [bring(BrazoDerecho)|T], BrazoDerecho, BrazoIzquierdo, NuevaLista):-
	traducir_decisiones_acciones(KB, Diagnostico, T, BrazoDerecho, BrazoIzquierdo, Aux),
	append([deliver(BrazoDerecho, start)], Aux, NuevaLista).

% Objeto brazo izquierdo
traducir_decisiones_acciones(KB, Diagnostico, [bring(BrazoIzquierdo)|T], BrazoDerecho, BrazoIzquierdo, NuevaLista):-
	traducir_decisiones_acciones(KB, Diagnostico, T, BrazoDerecho, BrazoIzquierdo, Aux),
	append([deliver(BrazoIzquierdo, start)], Aux, NuevaLista).

% Objeto sin brazos
traducir_decisiones_acciones(KB, Diagnostico, [bring(X)|T], BrazoDerecho, BrazoIzquierdo, NuevaLista):-
	traducir_decisiones_acciones(KB, Diagnostico, T, BrazoDerecho, BrazoIzquierdo, Aux),
	obtener_posicion_desde_diagnostico(X, Diagnostico, ObjetoCurrentPosicion),
	append([take(X, ObjetoCurrentPosicion), deliver(X, start)], Aux, NuevaLista).

% Ordenar objetos de brazo derecho
traducir_decisiones_acciones(KB, Diagnostico, [rearrange(BrazoDerecho)|T],BrazoDerecho,BrazoIzquierdo, NuevaLista):-
	traducir_decisiones_acciones(KB, Diagnostico, T, BrazoDerecho, BrazoIzquierdo, Aux),
	obtener_relacion_objeto(BrazoDerecho, associated_shelf, KB, CorrectPosicion),
	append([deliver(BrazoDerecho, CorrectPosicion)], Aux, NuevaLista).

% Ordenar objetos de brazo izquierdo
traducir_decisiones_acciones(KB, Diagnostico, [rearrange(BrazoIzquierdo)|T],BrazoDerecho,BrazoIzquierdo, NuevaLista):-
	traducir_decisiones_acciones(KB, Diagnostico, T, BrazoDerecho, BrazoIzquierdo,Aux),
	obtener_relacion_objeto(BrazoIzquierdo, associated_shelf, KB, CorrectPosicion),
	append([deliver(BrazoIzquierdo, CorrectPosicion)],Aux,NuevaLista).

% Ordenar objetos de ningun brazo
traducir_decisiones_acciones(KB, Diagnostico, [rearrange(X)|T],BrazoDerecho,BrazoIzquierdo, NuevaLista):-
	traducir_decisiones_acciones(KB, Diagnostico, T, BrazoDerecho, BrazoIzquierdo, Aux),
	obtener_relacion_objeto(X, associated_shelf, KB, CorrectPosicion),
	obtener_posicion_desde_diagnostico(X, Diagnostico, ObjetoCurrentPosicion),
	append([take(X, ObjetoCurrentPosicion), deliver(X, CorrectPosicion)], Aux, NuevaLista).

% Obtener posicion del obj por el diagnostico (Posicion observada o inferida?)
obtener_posicion_desde_diagnostico(_, [], unknown).

obtener_posicion_desde_diagnostico(Objeto, [move(Shelf), place(ListObjetos)|_], Shelf):-
	esElemento(Objeto, ListObjetos).

obtener_posicion_desde_diagnostico(Objeto, [move(_), place(_)|T], Shelf):-
	obtener_posicion_desde_diagnostico(Objeto, T, Shelf).

%Filtrar planes por la cantidad de brazos disponibles

filtrar_planes([], _, _, _, []).

filtrar_planes([H|T], Posicion, BrazoDerecho, BrazoIzquierdo, NuevaLista):-
	filtrar_planes(T, Posicion, BrazoDerecho, BrazoIzquierdo, AuxList),
	cambiar_plan(H, Posicion, BrazoDerecho, BrazoIzquierdo, PlanCambiado, Value),
	write('Possible plan: '), write(H),
	write('  Transformed plan: '), write(PlanCambiado), nl,
	write('Value: '), write(Value), nl, nl,
	append([Value=>PlanCambiado], AuxList, NuevaLista).

% Cambiar plan de las acciones de agarrar o entregar a mover o buscar
%notvalid ocurrs when the robot tries to take an object but both arms are occupied, or when the robot tries to deliver an object that is not in his arms

cambiar_plan([], _, _, _, [], 0).

% Una toma en la posicion actual y ambos brazos libres
cambiar_plan([take(Objeto, Posicion)|T], Posicion, free, free, PlanCambiado, NuevoValor):-
	cambiar_plan(T, Posicion, Objeto, free, Aux, Value),
	append([grasp(Objeto, right)], Aux, PlanCambiado),
	probabilidad_decision(Objeto, CostoAccion, _),
	NuevoValor is (Value+CostoAccion).
	
% Una toma en la posicion actual y brazo derecho libre
cambiar_plan([take(Objeto, Posicion)|T], Posicion, free, BrazoIzquierdo, PlanCambiado, NuevoValor):-
	BrazoIzquierdo \= free,
	cambiar_plan(T, Posicion, Objeto, BrazoIzquierdo, Aux, Value),
	append([grasp(Objeto, right)], Aux, PlanCambiado),
	probabilidad_decision(Objeto, CostoAccion, _),
	NuevoValor is (Value+CostoAccion).
	
% Una toma en la posicion actual y brazo izquierdo libre
cambiar_plan([take(Objeto, Posicion)|T], Posicion, BrazoDerecho, free, PlanCambiado, NuevoValor):-
	BrazoDerecho \= free,
	cambiar_plan(T, Posicion, BrazoDerecho, Objeto, Aux, Value),
	append([grasp(Objeto, left)], Aux, PlanCambiado),
	probabilidad_decision(Objeto, CostoAccion, _),
	NuevoValor is (Value+CostoAccion).	

% Una toma en la posicion actual y ningun brazo libre
cambiar_plan([take(_, Posicion)|_], Posicion, BrazoDerecho, BrazoIzquierdo, [notvalid], 100000):-
	BrazoIzquierdo \= free,
	BrazoDerecho \= free.

% Una toma en otro lugar
cambiar_plan([take(Objeto, Shelf)|T], Posicion, BrazoDerecho, BrazoIzquierdo, PlanCambiado, NuevoValor):-
	Shelf \= Posicion,
	cambiar_plan([take(Objeto, Shelf)|T], Shelf, BrazoDerecho, BrazoIzquierdo, Aux, Value),
	append([move(Shelf)], Aux, PlanCambiado),
	probabilidad_movimiento(Posicion, Shelf, CostoAccion, _),
	NuevoValor is (Value+CostoAccion).
	
% Una entrega en la posicion actual con el brazo derecho
cambiar_plan([deliver(Objeto, Posicion)|T], Posicion, Objeto, BrazoIzquierdo, PlanCambiado, NuevoValor):-
	cambiar_plan(T, Posicion, free, BrazoIzquierdo, Aux, Value),
	append([deliver(Objeto, right)], Aux, PlanCambiado),
	probabilidad_accion(Objeto, CostoAccion,_),
	NuevoValor is (Value+CostoAccion).	

% Una entrega en la posicion actual con el brazo izquierdo
cambiar_plan([deliver(Objeto, Posicion)|T], Posicion, BrazoDerecho, Objeto, PlanCambiado, NuevoValor):-
	cambiar_plan(T, Posicion, BrazoDerecho, free, Aux, Value),
	append([deliver(Objeto, left)], Aux, PlanCambiado),
	probabilidad_accion(Objeto, CostoAccion,_),
	NuevoValor is (Value+CostoAccion).	

% Una entrega en la posicion actual sin ningun brazo
cambiar_plan([deliver(Objeto, Posicion)|_], Posicion, BrazoDerecho, BrazoIzquierdo, [notvalid], 100000):-
	BrazoIzquierdo \= Objeto,
	BrazoDerecho \= Objeto.

% Una entrega en otro lugar
cambiar_plan([deliver(Objeto, Shelf)|T], Posicion, BrazoDerecho, BrazoIzquierdo, PlanCambiado, NuevoValor):-
	Shelf \= Posicion,
	cambiar_plan([deliver(Objeto, Shelf)|T], Shelf, BrazoDerecho, BrazoIzquierdo, Aux, Value),
	append([move(Shelf)], Aux, PlanCambiado),
	probabilidad_movimiento(Posicion, Shelf, CostoAccion, _),
	NuevoValor is (Value+CostoAccion).