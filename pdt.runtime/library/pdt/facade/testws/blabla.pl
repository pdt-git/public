:- use_module(aha).
aha:ja(a).
aha:ja(b).


referer(A):-
	umf(aha:ja(A)). 
	
umf(X):-call(X).	 