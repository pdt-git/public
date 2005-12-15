la(1).
la(2).
la(3).

testla:-
    repeat,
    la(A),
    writeln(A),
    fail.

hu(u(A),u(B)):-
    ra(A),
    ra(B).
ra(lu(_)).

do(In,Out):-
    unify([],In,hu(u(A),u(B)),S0),  % here we need the righthand bindings, 
    								% but they are in the original clause head.
    apply_subst(S0,ra(A),H1),
    do(H1,H1out),
    unify(S0,H1,H1out,S1), 	% what are righthand bindings used for? -
    						% The out head needs to have unbound variables at places
    						% where the in head is bound. How could this happen?
    apply_subst(S1,ra(B),H2),
    do(H2,H2out),
    unify(S1,H2,H2out,S2),
    apply_subst(S2,In,Out).
    
:- do(hu(A,u(B)),Out).

weird(c(a,A),B):-
    weird(A,B).


:- weird(a,A).
:- do(weird(a,A),Out),
	unify([],weird(a,A),Out,S0).
	
eq(A,A).