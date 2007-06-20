:- dynamic umf:bla/0.

p(X):-
    q(X).
p(X):-
	fallback(X).