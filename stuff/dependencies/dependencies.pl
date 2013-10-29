:- module(dependencies, [
	dep/2,
	deps/2
]).

:- use_module(library(lists)).
:- use_module(pdt_common_pl('callgraph/pdt_call_graph')).

starting_point(pdt_search, loaded_by, 4).
%starting_point(pdt_search, find_completion, 12).

pdt_dir('c:/users/user/documents/neuer ordner/git-repos/pdt/').

dep(M:N/A, M2:N2/A2) :-
	deps(M:N/A, Deps),
	member(M2:N2/A2, Deps).

deps(P, Deps) :-
	(	var(P)
	->	starting_point(M, N, A)
	;	P = M:N/A
	),
	setof(MX:NX/AX, dep(M, N, A, MX, NX, AX, []), Deps).

dep(M, N, A, _M2, _N2, _A2, Visited) :-
	member(M-N-A, Visited),
	!,
	fail.
dep(M, N, A, M2, N2, A2, Visited) :-
	calls(MX, NX, AX, M, N, A, _),
	(	is_pdt_predicate(MX, NX, AX)
	->	dep(MX, NX, AX, M2, N2, A2, [M-N-A|Visited])
	;	\+ is_iso_predicate(MX, NX, AX),
		M2 = MX,
		N2 = NX,
		A2 = AX
	).
	
is_pdt_predicate(M, N, A) :-
	functor(H, N, A),
	(	predicate_property(M:H, file(F))
	->	true
	;	module_property(M, file(F))
	),
	pdt_dir(P),
	atom_concat(P, _, F),
	!.
is_iso_predicate(M, N, A) :-
	functor(H, N, A),
	predicate_property(M:H, iso),
	!.