:- module(pdt_call_analysis, [find_undefined_call/7, ensure_call_graph_generated/0, find_dead_predicate/6, find_undeclared_meta_predicate/7]).

:- use_module(pdt_prolog_codewalk).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(pdt_prolog_library(utils4modules_visibility)).
:- use_module('metainference/pmi').
:- use_module(pdt_entry_points).

:- dynamic(result/4).

assert_result(QGoal, _, clause_term_position(Ref, TermPosition), Kind) :-
	QGoal = _:Goal,
    (	result(Goal, Ref, TermPosition, Kind)
    ->	true
    ;	assertz(result(Goal, Ref, TermPosition, Kind))
    ),
    !.
assert_result(_,_,_,_).

%% find_undefined_call(Module, Name, Arity, File, Start, End, PropertyList) 
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
	;	setof(Module, Head^(
			(	pdt_reload:reloaded_file(File),
				source_file(Head, File),
				module_of_head(Head, Module)
			;	retract(module_to_clear(Module))
			)
		), Modules),
		maplist(clear, Modules),
		maplist(generate_call_graph, Modules)
	).

:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).
user:message_hook(load_file(start(_, file(_, File))),_,_) :-
	\+ first_run,
	source_file(Head, File),
	module_of_head(Head, Module),
	\+ module_to_clear(Module),
	assertz(module_to_clear(Module)),
	fail.

module_of_head(Module:_Head, Module) :- !.
module_of_head(_Head, user).

:- dynamic(module_to_clear/1).

%% find_dead_predicate(Module, Functor, Arity, File, Location, PropertyList) 
%
find_dead_predicate(Module, Functor, Arity, File, Location, PropertyList) :-
	find_dead_predicates,
	!,
	is_dead(Module, Functor, Arity),
	once(accept_dead_predicate(Module:Functor/Arity)),
	defined_in_files(Module, Functor, Arity, Locations),
	member(File-LineAndClauseRefs, Locations),
    member(location(Line, Ref), LineAndClauseRefs),
    properties_for_predicate(Module, Functor, Arity, PropertyList0),
    (	pdt_search:head_position_of_clause(Ref, Position)
    ->	Location = Position,
    	PropertyList = [line(Line)|PropertyList0]
    ;	Location = Line,
    	PropertyList = PropertyList0
    ).

:- multifile(entry_point/1).

:- multifile(accept_dead_predicate/1).

:- dynamic(is_called/3).
:- dynamic(is_dead/3).

find_dead_predicates :-
	ensure_call_graph_generated,
	retractall(is_called(_, _, _)),
	retractall(is_dead(_, _, _)),
	forall((
		entry_point_predicate(M, F, A)
	),(
		(	is_called(M, F, A)
		->	true
		;	assertz(is_called(M, F, A)),
			follow_call_edge(M, F, A)
		)
	)),
	(	is_called(_, _, _)
	->	forall((
			declared_in_module(M, F, A, M),
			\+ is_called(M, F, A)
		),(
			(	is_dead(M, F, A)
			->	true
			;	assertz(is_dead(M, F, A))
			)
		))
	;	true
	).

entry_point_predicate(M, F, A) :-
	entry_point(M), % module
	atomic(M),
	(	M == user
	->	declared_in_module(user, F, A, user)
	;	module_property(M, exports(ExportList)),
		member(F/A, ExportList)
	).
entry_point_predicate(M, F, A) :-
	entry_point(M:F/A). % predicate
entry_point_predicate(M, F, A) :-
	pdt_entry_point(File),
	(	module_property(M, file(File))
	*->	module_property(M, exports(ExportList)),
		member(F/A, ExportList)
	;	source_file(Head, File),
		functor(Head, F, A)
	).

follow_call_edge(M, F, A) :-
	calls(M2, F2, A2, M, F, A, _),
	\+ is_called(M2, F2, A2),
	assertz(is_called(M2, F2, A2)),
	follow_call_edge(M2, F2, A2),
	fail.
follow_call_edge(_, _, _).

%% find_undeclared_meta_predicate(Module, Name, Arity, MetaSpec, File, Line, PropertyList)
find_undeclared_meta_predicate(Module, Name, Arity, MetaSpec, File, Line, [label(MetaSpec)|PropertyList]) :-
	pdt_prolog_walk_code([]),
	!,
	declared_in_module(Module, Name, Arity, Module),
	functor(Head, Name, Arity),
	\+ predicate_property(Module:Head, built_in),
%	\+ predicate_property(Module:Head, multifile),
	inferred_meta(Module:Head, MetaSpec),
	predicate_property(Module:Head, line_count(Line)),
	(	predicate_property(Module:Head, meta_predicate(DeclaredMetaSpec))
	->	DeclaredMetaSpec \== MetaSpec
	;	true
	),
	properties_for_predicate(Module, Name, Arity, PropertyList),
	member(file(File), PropertyList).


