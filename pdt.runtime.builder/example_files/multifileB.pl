:- module(multifileB, [a/1]).

:- use_module(multifileA). 


a(A):- 
	multifileA:multi(A).
	
multifileA:multi(a).