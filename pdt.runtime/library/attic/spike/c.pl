:- module(c,[call_hooks/1]).

:- dynamic hook/1.
:- multifile hook/1.

call_hooks(M):-
    forall(hook(M),writeln(M)).