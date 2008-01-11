:- use_module('aha.pl').
aha:ja(a).
aha:ja(b).

:- ['aha.pl','aha.pl',blabla,'aha.pl'].
referer(A):-
	umf(aha:ja(A)). 
	
umf(X):-call(X).	 