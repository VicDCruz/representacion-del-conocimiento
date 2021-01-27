planner_dlic(Diagnosis, Decisions, Plan):-
	open_kb('kb.txt', KB),
	nl,nl,write('=== Inferencia de planeación ==='),nl,

	object_property_value(golem, position, KB, Position),
	write('El robot esta en: '),write(Position),nl,

	object_property_value(golem, right_arm, KB, RightArm),
	write('El brazo derecho tiene: '),write(RightArm),nl,

	object_property_value(golem, left_arm, KB, LeftArm),
	write('El brazo izquierdo tiene: '),write(LeftArm),nl,

	write('Decisiones por tomar: '),write(Decisions),nl,
	convert_decisions_to_basic_actions_dlic(KB, Diagnosis, Decisions, RightArm, LeftArm, BasicActions),	
	write('Acciones por hacer: '),write(BasicActions),nl,

	%Generar todos los posibles planes
	setof(X, permutation(BasicActions, X), AllPlans),
	nl,write('Posibles planes: '),nl,
	
	%Obtener planeas validos
	filter_plans_dlic(AllPlans, Position, RightArm, LeftArm, FilteredPlans),

	%Ordenar y obtener el mejor plan
	sort(FilteredPlans,[_=>Plan|_]),
	nl,write('Plan seleccionado: '),write(Plan),nl.
