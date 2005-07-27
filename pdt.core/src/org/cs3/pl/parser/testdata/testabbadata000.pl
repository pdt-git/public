rumpel(A):-
    forall(
    	pumpel(C),
    	(	A=C
    	->	writeln('ja')
    	;	writeln('nein')
    	)
    ),
    true.

