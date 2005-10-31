:-module(wrapper,[wrap/2,unwrap/2]).

unwrap(f(A),A):-
	var(A),
	!.    
unwrap(f(A),B):-
	is_list(A),
	!,
	unwrap_elms(A,B).
unwrap(f({A}),{B}):-
    unwrap(A,B).
unwrap(f(Term),Out):-
    (	compound(Term)
    ->	Term=..[Name|Args],
    	unwrap_elms(Args,OutArgs),
	    Out=..[Name|OutArgs]
	 ;	Out=Term
	 ).	    

wrap(A,f(A)):-
	var(A),
	!.    
wrap(A,f(B)):-
	is_list(A),
	!,
	wrap_elms(A,B).
wrap({A},f({B})):-
    wrap(A,B).
wrap(Term,f(Out)):-
    (	compound(Term)
    ->	Term=..[Name|Args],
    	wrap_elms(Args,OutArgs),
	    Out=..[Name|OutArgs]
	 ;	Out=Term
	 ).	    

        
wrap_elms([],[]).
wrap_elms([H|T],[OH|OT]):-
    wrap(H,OH),
    wrap_elms(T,OT).

unwrap_elms([],[]).
unwrap_elms([H|T],[OH|OT]):-
    unwrap(H,OH),
    unwrap_elms(T,OT).
