:-module(testmoda,[foo/1,bar/1,baz/1,foo/0,bar/0,baz/0,wahr/1,gut/1]).
foo(A):-
	(	wahr(A),!,
		format("wahr: ~w~n",[A])
	;	format("u n w a h r: ~w~n",[A])
    ).


bar(A):-
	(	wahr(A)
	;	format("u n w a h r: ~w~n",[A])
	),
	!,
	format("wahr: ~w~n",[A]).

baz(A):-
	(	wahr(A),
		!
	;	format("u n w a h r: ~w~n",[A])
	),
	format("wahr: ~w~n",[A]).

foo:-
    gut(A),
	(	true
	->	format("wahr: ~w~n",[A])
	;	format("u n w a h r: ~w~n",[A])
    ).




bar:-
    gut(A),
	(	wahr(A)
	;	format("u n w a h r: ~w~n",[A])
	),
	!,
	format("wahr: ~w~n",[A]).

baz:-
    gut(A),    
	(	wahr(A),
		!
	;	format("u n w a h r: ~w~n",[A])
	),
	format("wahr: ~w~n",[A]).
    
wahr(wahrheit).    
wahr(schein).
gut(wahrheit).    
gut(schein).