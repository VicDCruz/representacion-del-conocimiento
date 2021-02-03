load_take_actions(KB):-
    objetos_solo_clase(take,KB,Objetos),
	load_take_action(Objetos).

load_take_action([],_).
	
load_take_action([H|T],KB):-
	valor_relacion_objeto(H,to,Object),
	getObjectPropertyValue(H,cost,KB,Cost),
	getObjectPropertyValue(H,probability,KB,Probability),
	assert(take_action_dlic(Object,Cost,Probability)),
	load_take_action(T).


%----------------------
% Modulo de TOMAR DECISION
%----------------------

tomar_decision(Decision):-
    open_kb('kb.txt',KB),
    load_take_actions(KB),
	%Lista de objetos mal colocados
    getObjectPropertyValue(pending_tasks,list,KB,ObjetosMalColocados),
	write('Objetos mal colocados: '),write(ObjetosMalColocados),nl, 

	%Lista de las ordenes de los clientes
	getObjectPropertyValue(pending_client_orders,list,KB,OrdenesClientes),
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
	write('Decision: '),write(Decision),nl. 




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

evaluar_opcion([],0,_).

evaluar_opcion([H|T],NuevoValor,KB):-
	%Probabilidad de tomar el objeto actual
	take_action_dlic(H,_,Probabilidad),

	%Verificar si realinear el objeto es buena idea (0/1)
	verifica_accion(H,Resultado,KB),
	
	evaluar_opcion(T,Val),
    NuevoValor is Val + ((Probabilidad/100)*Resultado).	


%Realinear el objeto es buena idea si:
%El objeto a realinear está en el estante actual
%La posicion del estante correcto del objeto es el siguiente estante en ser visitado (el mas cercano)

verifica_accion(Objecto,Valor,KB):-
	valor_relacion_objeto(Objecto,last_corroborated_position,ObjetoEstanteActual),
	valor_relacion_objeto(Objecto,associated_shelf,ObjetoEstanteCorrecto),	
	getObjectPropertyValue(golem,position,KB,PosicionActual),
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