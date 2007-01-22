:- use_module(library('org/cs3/pdt/util/pdt_util_rbtree')).

random_len(10000000000000).

random(I):-
    random_len(Len),
	I is random(Len).

:-dynamic bm_fact/1, bm_fact_2/1.
bm_assert:-
	bm_assert(1).
		
bm_assert(100000):-!.
bm_assert(I):-
    random(Key),
	assert(bm_fact(Key)),
	J is I + 1,
	bm_assert(J).

%----------------------------	


bm_assert_2:-
    repeat,
	    random(Key),
		assert(bm_fact_2(Key)),
		flag(counter,J,J+1),
		J=100000,
	!.

%----------------------------	
	
bm_rbtree:-
    pdt_rbtree_empty(T0),
    bm_rbtree(1,T0,_T).
    
bm_rbtree(100000,_,_):-!.
bm_rbtree(I,Tin,Tout):-
	random(Key),
	pdt_rbtree_insert(Tin,Key,Key,Tnext),
	J is I + 1,
	bm_rbtree(J,Tnext,Tout).
%----------------------	
bm_assoc:-
    empty_assoc(T0),
    bm_assoc(1,T0,_T).
    
bm_assoc(100000,_,_):-!.
bm_assoc(I,Tin,Tout):-
	random(Key),
	put_assoc(Key, Tin, Key, Tnext),
	J is I + 1,
	bm_assoc(J,Tnext,Tout).
%------------------------------


bm_reset:-
	retractall(bm_fact(_)),
	retractall(bm_fact_2(_)),	
	flag(counter,_,0).	
	
bm:-
    writeln(assert),
    time(bm_assert),
	bm_reset,
    writeln(assert_2),
    time(bm_assert_2),
	bm_reset,
    writeln(rbtree),
    time(bm_rbtree),
    bm_reset,
    writeln(assoc),
    time(bm_assoc),
    bm_reset.