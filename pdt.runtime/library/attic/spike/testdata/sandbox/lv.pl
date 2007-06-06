:- dynamic t/1.

t(a(1)).
t(a(2)).
t(a(3)).
t(b(1)).
t(b(2)).

first_t(A):-
    t(A),!.
    
my_test:-
	repeat,
		first_t(A),
		effect(A),
		functor(A,Name,Arity),
		functor(B,Name,Arity),
		retractall(t(B)),
		\+ first_t(_),
	!.  
	
effect(A):-
	writeln(A).	  