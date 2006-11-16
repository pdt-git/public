:- multifile test/1.
:- multifile tearDown/1.

:- [magicmap].
:- [sync].
:- [localisation].

test(separate_functor_arity_module_safe):-
    separate_functor_arity_module_safe(m:f/1, m:f, 1),
    separate_functor_arity_module_safe('m:f/1', m:f, 1),
    separate_functor_arity_module_safe(f/2, f, 2).
    
test(evaluate_and_store_idb):-
    evaluate_and_store_idb(mymodule:non_existing(_t)),
    clause(idb(mymodule:non_existing(_A)),fail).

tearDown(evaluate_and_store_idb):-
    retractall(idb(_)).


test(update_idb_and_changed):-
    Term = testmodule:newpred(_A,b),
    term_to_atom(Term, Atom),
    init_idb(Atom),
    assert(testmodule:newpred(a,b)),
    term_ref(Term,Ref),
	update_idb(Term, Ref),
	testmodule:newpred(a,b),
    clause(idb_copy(Ref,testmodule:newpred(_,_)),fail),
    changed(Ref).

tearDown(update_idb_and_changed):-
	term_ref(testmodule:newpred(_A,b), Ref),
	remove_term_idb(Ref),
	retractall(testmodule:newpred(_,_)).
    
tearDown('update_idb and changed'):-
    retractall(idb(_)),
    retractall(idb_copy(_)),
    retractall(testmodule:newpred(_,_)).

setUp(term_ref) :-
    init_term_ref(term(a,_B),_Ref).
    	
test(term_ref):-
    assert_true(term_ref(term(a,_C),_Ref)).
    
tearDown(term_ref):-
	term_ref(term(a,_C),Ref),
	remove_term_idb(Ref).

setUp(init_idb):-
    assert(location('00-09-2D-53-27-3A', 50.73, 7.12, 0)),
    retractall(idb(_,_)).
    
test(init_idb) :-
  init_idb('company_nearby(_,_,_,1000)'),
  term_ref(company_nearby(_,_,_,1000), Ref),


  retract(location('00-09-2D-53-27-3A', 50.73, 7.12, 0)),
  assert(location('00-09-2D-53-27-3A', 50.73, 7.123, 0)),

  update_idb(company_nearby(_,_,_,1000), Ref),

  changed(Ref).
  
tearDown(init_idb):-
    retractall(location(_,_,_,_)),
    term_ref(localisation:company_nearby(_,_,_,1000), Ref),
    remove_term_idb(Ref).
 