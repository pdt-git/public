:- module(multiplicity,[
	multiplicity_check/3,	
	multiplicity_count/3,
	count/2
	]
).

:- use_module(model).
% multiplicity_check(+Defined,+Current,?State)
multiplicity_check(0-n,_,ok).
multiplicity_check(N-M,C,ok):-
    (compare(=,N,C); compare(<,N,C)),
    (compare(=,C,M); compare(<,C,M)).
multiplicity_check(_-n,_,can_create).
multiplicity_check(_-N,C,can_create):-
    compare(<,C, N).
multiplicity_check(N-_,C,can_delete):-
    compare(<,N,C).
    
count(Goal,Count):-
    nb_setval(my_counter,0),
    (	(	Goal,
    		nb_getval(my_counter,I),
    		succ(I,II),
    		nb_setval(my_counter,II),
    		fail
    	)
    ; 	nb_getval(my_counter,Count),
    	nb_delete(my_counter)
    ).
