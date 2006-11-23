%:- module(testmodule,[]).

mf:-
    new_memory_file(MF),
    open_memory_file(MF,write,S),
    stream_property(S,encoding(E)),
    writeln(E),
    close(S),
    free_memory_file(MF).
    
cool(me).
be_cool:-cool(A),A.


umf([]).
umf([A|As]):-
    A,
    umf(As).
    
