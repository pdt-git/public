:- module(useUseBla,[]).

:- use_module(usebla).


y(X):- 
	(	x(X) > 3
	;	X=c
	).