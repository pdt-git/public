:- module(compatibility,[
	pdt_source_file/2
]).

:- if((current_prolog_flag(version_data, Version), Version @>= swi(7, 1, 15, []))).

:- meta_predicate(pdt_source_file(:, ?)).
pdt_source_file(PI, File) :-
	source_file(PI, File).

:- else.

pdt_source_file(PI, File) :-
	nonvar(PI),
	!,
	(	PI = user:Head
	->	source_file(Head, File)
	;	source_file(PI, File)
	).

pdt_source_file(PI, File) :-
	source_file(Head, File),
	(	Head = _:_
	->	PI = Head
	;	PI = user:Head
	).

:- endif.