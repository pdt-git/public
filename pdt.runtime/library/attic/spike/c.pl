:- module(c,[call_hooks/1]).
:- ensure_loaded(a).
:- dynamic hook/1.
:- multifile hook/1.

call_hooks(M):-
    forall(hook(M),writeln(M)).