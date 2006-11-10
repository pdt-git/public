:- module(testmodule,[]).

cool(me).
be_cool:-cool(A),A.


umf([]).
umf([A|As]):-
    A,
    umf(As).
    
