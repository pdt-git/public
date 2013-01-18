:- module(pdt_call_analysis, [find_undefined_call/7, ensure_call_graph_generated/0, ensure_dead_predicates_up_to_date/0, dead_predicate/5]).

:- use_module(pdt_prolog_codewalk).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(pdt_prolog_library(utils4modules_visibility)).

:- dynamic(result/4).

assert_result(QGoal, _, clause_term_position(Ref, TermPosition), Kind) :-
	QGoal = _:Goal,
    (	result(Goal, Ref, TermPosition, Kind)
    ->	true
    ;	assertz(result(Goal, Ref, TermPosition, Kind))
    ),
    !.
assert_result(_,_,_,_).

find_undefined_call(Module, Name, Arity, File, Start, End, [clause_line(Line),goal(GoalAsAtom)|PropertyList]) :-
	retractall(result(_, _, _, _)),
	pdt_prolog_walk_code([undefined(trace), on_trace(pdt_call_analysis:assert_result)]),
	!,
	retract(result(Goal, Ref, TermPosition, _Kind)),
	(	TermPosition = term_position(Start, End, _, _, _)
	->	true
	;	TermPosition = Start-End
	),
	clause_property(Ref, file(File)),
	clause_property(Ref, predicate(Module:Name/Arity)),
	clause_property(Ref, line_count(Line)),
	properties_for_predicate(Module,Name,Arity,PropertyList),
	format(atom(GoalAsAtom), '~w', [Goal]).

:- dynamic(first_run/0).
first_run.

ensure_call_graph_generated :-
	first_run,
	!,
	generate_call_graph,
	retractall(first_run).
ensure_call_graph_generated.

:- dynamic(calls/7).

clear :-
	retractall(calls(_,_,_,_,_,_,_)).

clear(Module) :-
	retractall(calls(_,_,_,Module,_,_,_)).

generate_call_graph :-
	pdt_prolog_walk_code([ trace_reference(_),
			on_trace(pdt_call_analysis:assert_edge),
			on_reiterate(pdt_call_analysis:clear),
			source(false)
			]).

generate_call_graph(Module) :-
	pdt_prolog_walk_code([ trace_reference(_),
			on_trace(pdt_call_analysis:assert_edge),
			on_reiterate(pdt_call_analysis:clear(Module)),
			source(false),
			module(Module)
			]).

assert_edge(M1:Callee, M2:Caller, _, _) :-
	(	predicate_property(M1:Callee,built_in)
	->	true
	;	functor(Callee,F1,N1),
		(	predicate_property(M1:Callee, imported_from(M0))
		->	M = M0
		;	M = M1
		),
		functor(Caller,F2,N2), 
		assert_edge_(M,F1,N1, M2,F2,N2)
	). 
assert_edge(_, '<initialization>', _, _) :- !.

assert_edge_(M1,F1,N1, M2,F2,N2) :-
	retract( calls(M1,F1,N1, M2,F2,N2, Counter) ), 
	!,
	Cnt_plus_1 is Counter + 1,
	assertz(calls(M1,F1,N1, M2,F2,N2, Cnt_plus_1)).
assert_edge_(M1,F1,N1, M2,F2,N2) :-
	assertz(calls(M1,F1,N1, M2,F2,N2, 1)).

:- multifile(pdt_reload:pdt_reload_listener/1).

pdt_reload:pdt_reload_listener(_Files) :-
	(	first_run
	->	true
	;	setof(Module, (
			pdt_reload:reloaded_file(File),
			(	module_property(Module, file(File))
			*->	true
			;	Module = user
			)
		),Modules),
		maplist(clear, Modules),
		maplist(generate_call_graph, Modules)
	),
	retractall(dead_predicate_search_up_to_date).

:- dynamic(dead_predicate_search_up_to_date/0).

dead_predicate(Module, Functor, Arity, File, Line) :-
	is_dead(Module, Functor, Arity),
	once(accept_possible_dead_predicate(Module:Functor/Arity)),
	functor(Head, Functor, Arity),
	(	predicate_property(Module:Head, multifile)
	->	defined_in_files(Module, Functor, Arity, Locations),
		member(File-LineAndClauseRefs, Locations),
		setof(Line, Ref^(member(location(Line, Ref), LineAndClauseRefs)), SortedLines),
		SortedLines = [Line|_]
	;	predicate_property(Module:Head, file(File)),
		predicate_property(Module:Head, line_count(Line))
	).

:- multifile(entry_point/1).
%:- dynamic(entry_point/1).

:- multifile(accept_possible_dead_predicate/1).
%:- dynamic(accept_possible_dead_predicate/1).

:- dynamic(is_called/3).
:- dynamic(is_dead/3).

ensure_dead_predicates_up_to_date :-
	dead_predicate_search_up_to_date,
	!.
ensure_dead_predicates_up_to_date :-
	find_dead_predicates.

find_dead_predicates :-
	ensure_call_graph_generated,
	retractall(is_called(_, _, _)),
	retractall(is_dead(_, _, _)),
	forall((
		entry_point(E),
		(	atomic(E)
		->	% module
			(	E == user
			->	current_predicate(_, user:H),
				\+ predicate_property(user:H, imported_from(_)),
				functor(H, F, A)
			;	module_property(E, exports(ExportList)),
				member(F/A, ExportList)
			)
		;	% predicate
			E = M:F/A
		)
	),(
		assertz(is_called(M, F, A)),
		follow_call_edge(M, F, A)
	)),
	forall((
		declared_in_module(M, F, A, M),
		\+ is_called(M, F, A)
	),(
		(	is_dead(M, F, A)
		->	true
		;	assertz(is_dead(M, F, A))
		)
	)).

follow_call_edge(M, F, A) :-
	calls(M2, F2, A2, M, F, A, _),
	\+ is_called(M2, F2, A2),
	assertz(is_called(M2, F2, A2)),
	follow_call_edge(M2, F2, A2),
	fail.
follow_call_edge(_, _, _).