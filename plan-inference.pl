%take_action_dlic

:-op(800,xfx,'=>').
:-op(800,xfx,'==>').
:-op(800,xfx,'=>>').

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

planner_dlic(Diagnostico, Decisions, Plan):-
	open_kb('kb.txt', KB),
	nl,nl,write('=== Inferencia de planeación ==='),nl,

	object_property_value(golem, position, KB, Posicion),
	write('El robot esta en: '),write(Posicion),nl,

	object_property_value(golem, right_arm, KB, BrazoDerecho),
	write('El brazo derecho tiene: '),write(BrazoDerecho),nl,

	object_property_value(golem, left_arm, KB, BrazoIzquierdo),
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
	take_action_dlic(Objeto, CostoAccion, _),
	NuevoValor is (Value+CostoAccion).
	
% Una toma en la posicion actual y brazo derecho libre
cambiar_plan([take(Objeto, Posicion)|T], Posicion, free, BrazoIzquierdo, PlanCambiado, NuevoValor):-
	BrazoIzquierdo \= free,
	cambiar_plan(T, Posicion, Objeto, BrazoIzquierdo, Aux, Value),
	append([grasp(Objeto, right)], Aux, PlanCambiado),
	take_action_dlic(Objeto, CostoAccion, _),
	NuevoValor is (Value+CostoAccion).
	
% Una toma en la posicion actual y brazo izquierdo libre
cambiar_plan([take(Objeto, Posicion)|T], Posicion, BrazoDerecho, free, PlanCambiado, NuevoValor):-
	BrazoDerecho \= free,
	cambiar_plan(T, Posicion, BrazoDerecho, Objeto, Aux, Value),
	append([grasp(Objeto, left)], Aux, PlanCambiado),
	take_action_dlic(Objeto, CostoAccion, _),
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
	move_action_dlic(Posicion, Shelf, CostoAccion, _),
	NuevoValor is (Value+CostoAccion).
	
% Una entrega en la posicion actual con el brazo derecho
cambiar_plan([deliver(Objeto, Posicion)|T], Posicion, Objeto, BrazoIzquierdo, PlanCambiado, NuevoValor):-
	cambiar_plan(T, Posicion, free, BrazoIzquierdo, Aux, Value),
	append([deliver(Objeto, right)], Aux, PlanCambiado),
	deliver_action_dlic(Objeto, CostoAccion, _),
	NuevoValor is (Value+CostoAccion).	

% Una entrega en la posicion actual con el brazo izquierdo
cambiar_plan([deliver(Objeto, Posicion)|T], Posicion, BrazoDerecho, Objeto, PlanCambiado, NuevoValor):-
	cambiar_plan(T, Posicion, BrazoDerecho, free, Aux, Value),
	append([deliver(Objeto, left)], Aux, PlanCambiado),
	deliver_action_dlic(Objeto, CostoAccion, _),
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
	move_action_dlic(Posicion, Shelf, CostoAccion, _),
	NuevoValor is (Value+CostoAccion).	
