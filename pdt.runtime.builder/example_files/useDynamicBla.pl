:- module(useDynamicBla,[]).

:- use_module(dynamicbla).

y(X):- 
	(	x(X) > 3
	;	X=c
	).