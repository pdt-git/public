:- use_module('oho.pl').
aha:ja(a).
aha:ja(b).

:-['oho.pl',
	'oho.pl',
		blabla,'oho.pl'
	].
referer(A):-
	umf(aha:ja(A)). 
	
umf(X):-call(X).	 