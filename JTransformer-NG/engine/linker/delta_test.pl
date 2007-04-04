:- use_module(delta).

setUp(delta_module):-
	activate.
test(delta_module):-
	exec_with_delta(assert,deltaterm),
	query(assert,deltaterm).
	
tearDown(delta_module):-
    deactivate,
    clear.
