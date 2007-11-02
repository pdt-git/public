
/*p:-
    forall(a,b).
*/
/*
zeug(G,A):-
    forall(G,writeln(A)).

   
test_zeug:-
	writeln(running_test),
	zeug(mich_gibts_gar_nicht,mir_doch_egal).
*/
	
call_all([]).
call_all([A|As]):-
    call(A),
    call_all(As).
    
test_call_all:-
	call_all([c,b,d,x,y]).
  