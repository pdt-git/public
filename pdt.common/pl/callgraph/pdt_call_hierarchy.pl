/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2014, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(pdt_call_hierarchy, [
	find_predicate_declaration_and_visibility/5,
	find_caller/9,
	find_callee/9,
	find_call_location/9
]).

:- use_module(pdt_prolog_library(utils4modules_visibility), [
	declared_in_module/4,
	module_of_file/2
]).

:- use_module(pdt_call_graph, [
	ensure_call_graph_generated/0,
	calls/7,
	pdt_walk_code/1
]).

%% find_predicate_declaration_and_visibility(ModuleOrFile, Name, Arity, DeclaringModule, Visibility)
find_predicate_declaration_and_visibility(ModuleOrFile, Name, Arity, DeclaringModule, Visibility) :-
	(	ModuleOrFile = module(Module)
	->	true
	;	ModuleOrFile = file(File),
		source_file(File),
		once(module_of_file(File, Module))
	),
	(	declared_in_module(Module, Name, Arity, DeclaringModule)
	->	true
	;	DeclaringModule = Module
	),
	predicate_visibility(DeclaringModule, Name, Arity, Visibility).

%% predicate_visibility(Module, Name, Arity, Visibility)
predicate_visibility(Module, Name, Arity, Visibility) :-
	(	visible_in_module(Module, Name, Arity)
	->	(	(	Module == user
			;	functor(Head, Name, Arity),
				predicate_property(Module:Head, exported)
			)
		->	Visibility = exported
		;	Visibility = non_exported
		)
	;	Visibility = undefined
	).

%% find_caller(Module, Name, Arity, Root, CallerModule, CallerName, CallerArity, Count, Visibility)
find_caller(Module, Name, Arity, _Root, CallerModule, CallerName, CallerArity, Count, Visibility) :-
	ensure_call_graph_generated,
	calls(Module, Name, Arity, CallerModule, CallerName, CallerArity, Count),
	predicate_visibility(CallerModule, CallerName, CallerArity, Visibility).
	
%% find_callee(Module, Name, Arity, Root, CalleeModule, CalleeName, CalleeArity, Count, Visibility)
find_callee(Module, Name, Arity, _Root, CalleeModule, CalleeName, CalleeArity, Count, Visibility) :-
	ensure_call_graph_generated,
	calls(CalleeModule, CalleeName, CalleeArity, Module, Name, Arity, Count),
	predicate_visibility(CalleeModule, CalleeName, CalleeArity, Visibility).
 
%% find_call_location(CallerModule, CallerName, CallerArity, CalleeModule, CalleeName, CalleeArity, Root, File, Location)
find_call_location(CallerModule, CallerName, CallerArity, CalleeModule, CalleeName, CalleeArity, Root, File, Location) :-
	retractall(result(_, _, _, _)),
	functor(CalleeHead, CalleeName, CalleeArity),
	pdt_walk_code([trace_reference(CalleeModule:CalleeHead), predicates([CallerModule:CallerName/CallerArity]), on_trace(pdt_call_hierarchy:assert_result)]),
	!,
	retract(result(_Goal, Ref, TermPosition, _Kind)),
	(	(	TermPosition = term_position(Start, End, _, _, _)
		;	TermPosition = Start-End
		)
	->	format(atom(Location), '~w-~w', [Start, End])
	;	clause_property(Ref, line_count(Location))
	),
	clause_property(Ref, file(File)),
	(	nonvar(Root)
	->	sub_atom(File, 0, _, _, Root)
	;	true
	).

:- dynamic(result/4).

assert_result(Goal, _, clause(Ref), Kind) :-
    (	result(Goal, Ref, [], Kind)
    ->	true
    ;	assertz(result(Goal, Ref, [], Kind))
    ),
    !.
assert_result(Goal, _, clause_term_position(Ref, TermPosition), Kind) :-
    (	result(Goal, Ref, TermPosition, Kind)
    ->	true
    ;	assertz(result(Goal, Ref, TermPosition, Kind))
    ),
    !.
assert_result(_,_,_,_).

