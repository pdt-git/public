:- module(b,[]).
:- use_module(c).
c:hook(M):-
    b:local(M).
    
local(M):-
    forall((context_module(LM),member(X,[1,2,3])),writeln(LM)),
	context_module(M).   