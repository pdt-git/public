:-module(collector_test,[
	knarz/4, rumpel/1
]).

:- dynamic pumpel/4.

rumpel(A):-
    forall(
    	pumpel(C),
    	(	A=C
    	->	writeln('ja')
    	;	writeln('nein')
    	)
    ),
    true.
    
knarz(A,B,C,D):-
    assert(pumpel(A)),
    assert(pumpel(B)),
    assert(pumpel(C)),
    assert(pumpel(D)).


aua:(autsch(Lala):-
	rumpel(Lala)
	).

