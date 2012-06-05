:- use_module(builder).
:- dynamic target_ts/2,current_ts/1.
%:- tspy(pdt_builder:spyme).
:- debug(builder(transition(_))).

% t1 --> t2 --> t3
pdt_builder:target_mutable(t1,true).
pdt_builder:target_mutable(t2,true).
pdt_builder:target_mutable(t3,true).
pdt_builder:target_mutable(t4,true).
pdt_builder:build_hook(t1):-
    pdt_with_targets([t2],
    	(	current_ts(Ts),
    		retractall(target_ts(t1,_)),    		
    		assert(target_ts(t1,Ts))
    	)
    ).
    
pdt_builder:build_hook(t2):-
    pdt_with_targets([t3],
    	(	current_ts(Ts),
    		retractall(target_ts(t2,_)),
    		assert(target_ts(t2,Ts))
    	)
    ).    

pdt_builder:build_hook(t3):-
    pdt_with_targets([],
    	(	current_ts(Ts),
    		(	catch(pdt_request_target(t1),_,fail)
    		->	throw(not_what_I_expected)
    		;	true
    		),
    		retractall(target_ts(t3,_)),
    		assert(target_ts(t3,Ts))
    	)
    ).    

do_test2(S):-
    thread_create(do_test(S),_,[]).
    
do_test(S):-
    pdt_invalidate_target(t3),
    retractall(current_ts(_)),
    assert(current_ts(S)),
    pdt_with_targets([t1],true),
    forall(target_ts(A,B),writeln(target_ts(A,B))).