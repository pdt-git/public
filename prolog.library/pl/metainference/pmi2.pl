/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 *
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 *
 * Authors: Eva Stoewe, Guenter Kniesel and Jan Wielemaker
 *
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 ****************************************************************************/

:- module(pmi,
	  [ infer_meta/2,		% :Head, -MetaSpec
	    inferred_meta/2		% :Head, ?MetaSpec
	  ]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(pdt_prolog_library(utils4modules_visibility)).
:- use_module(pdt_prolog_library(lists)).

reset :-
	retractall(inferred_meta_pred2(_, _, _)).

run :-
	run(1).

run(Run) :-
	statistics(cputime, CPU0),
	format('Starting meta inference iteration ~w~n', [Run]),
	findall(MetaSpec, (
		declared_in_module(Module, Name, Arity, Module),
		functor(Head, Name, Arity),
		\+ predicate_property(Module:Head, built_in),
		\+ predicate_property(Module:Head, foreign),
		\+ predicate_property(Module:Head, number_of_clauses(0)),
		do_infer_meta(Module:Head, MetaSpec),
		(	inferred_meta_pred2(Head, Module, ExistingMetaSpec)
		->	(	MetaSpec == ExistingMetaSpec
			->	fail
			;	retract(inferred_meta_pred2(Head, Module, ExistingMetaSpec)),
				assertz(inferred_meta_pred2(Head, Module, MetaSpec))
			)
		;	assertz(inferred_meta_pred2(Head, Module, MetaSpec))
		)
	), NewOrModifiedMetaSpecs),
	statistics(cputime, CPU1),
	CPU is CPU1 - CPU0,
	length(NewOrModifiedMetaSpecs, NewOrModified),
	format('Finished meta inference iteration ~w: ~w meta predicates found or modified in ~f sec.~n', [Run, NewOrModified, CPU]),
	(	NewOrModified > 0
	->	NextRun is Run + 1,
		run(NextRun)
	;	true
	).

:- meta_predicate
	inferred_meta(:, ?),
	infer_meta(:, -).

:- dynamic
	inferred_meta_pred2/3.			% Head, Module, Meta

/** <module> Infer meta-predicate properties

This module infers meta-predicate properties   by inspecting the clauses
of predicates that call other predicates.   This is extremely useful for
program analysis and refactoring because  many   programs  `in the wild'
have incomplete or incorrect meta-predicate information.

@see	This library is used by prolog_walk_code/1 to improve the
	accuracy of this analysis.
@tbd	Re-introduce some alias-analysis
@tbd	Not all missing meta-declarations are interesting.  Notably,
	meta-predicates that are private and only pass meta-arguments
	on behalve of a public meta-predicates do not need a declaration.
*/


%%	inferred_meta_predicate(:Head, ?MetaSpec) is nodet.
%
%	True when MetaSpec is an   inferred meta-predicate specification
%	for Head.

inferred_meta(M:Head, MetaSpec) :-
	inferred_meta_pred2(Head, M, MetaSpec).
inferred_meta(M:Head, MetaSpec) :-
	predicate_property(M:Head, imported_from(From)),
	inferred_meta_pred2(Head, From, MetaSpec).


%%	infer_meta_predicate(:Head, -MetaSpec) is semidet
%
%	True  when  MetaSpec  is  a  meta-predicate  specifier  for  the
%	predicate Head. Derived meta-predicates are   collected and made
%	available through inferred_meta_predicate/2.

infer_meta(Head, MetaSpec) :-
	inferred_meta(Head, MetaSpec), !.
infer_meta(M:Head, MetaSpec) :-
	predicate_property(M:Head, imported_from(From)), !,
	do_infer_meta(From:Head, MetaSpec),
	assertz(inferred_meta_pred2(Head, From, MetaSpec)).
infer_meta(M:Head, MetaSpec) :-
	do_infer_meta(M:Head, MetaSpec),
	assertz(inferred_meta_pred2(Head, M, MetaSpec)).

:- meta_predicate
	do_infer_meta(:, -).

do_infer_meta(Module:AHead, MetaSpec):-
	functor(AHead, Functor, Arity),
	functor(Head, Functor, Arity),	% Generalise the head
	findall(MetaSpec,
		meta_pred_args_in_clause(Module, Head, MetaSpec),
		MetaSpecs),
	MetaSpecs \== [],
	combine_meta_args(MetaSpecs, MetaSpec).


%%	meta_pred_args_in_clause(+Module, +Head, -MetaSpec) is nondet.

meta_pred_args_in_clause(Module, Head, MetaArgs) :-
	catch(clause(Module:Head, Body), _, fail),
	reset_call_count,
	annotate_meta_vars_in_body(Body, Module),
	meta_annotation(Head, MetaArgs).

reset_call_count :-
	flag(call_count, _, 1).

next_call_count(CallCount) :-
	flag(call_count, CallCount, CallCount + 1).

%%	annotate_meta_vars_in_body(+Term, +Module) is det
%
%	Annotate variables in Term if they appear as meta-arguments.
%
%	@tbd	Aliasing.  Previous code detected aliasing for
%		- =/2
%		- functor/3
%		- atom_concat/3
%		- =../2
%		- arg/3
%	@tbd	We can make this nondet, exploring multiple aliasing
%		paths in disjunctions.

:- discontiguous(annotate_meta_vars_in_body/2).
annotate_meta_vars_in_body(A, _) :-
	atomic(A), !.
annotate_meta_vars_in_body(Var, _) :-
	var(Var), !,
	next_call_count(CallCount),
	annotate(0, CallCount, Var).
annotate_meta_vars_in_body(Module:Term, _) :- !,
	(   atom(Module)
	->  annotate_meta_vars_in_body(Term, Module)
	;   var(Module)
	->  next_call_count(CallCount),
		annotate(m, CallCount, Module)
	;   true			% may continue if Term is a system
	).				% predicate?
annotate_meta_vars_in_body((TermA, TermB), Module) :- !,
	annotate_meta_vars_in_body(TermB, Module),
	annotate_meta_vars_in_body(TermA, Module).
annotate_meta_vars_in_body((TermA; TermB), Module) :- !,
	(	annotate_meta_vars_in_body(TermB, Module)
	;	annotate_meta_vars_in_body(TermA, Module)
	).
annotate_meta_vars_in_body((TermA->TermB), Module) :- !,
	annotate_meta_vars_in_body(TermB, Module),
	annotate_meta_vars_in_body(TermA, Module).
annotate_meta_vars_in_body((TermA*->TermB), Module) :- !,
	annotate_meta_vars_in_body(TermB, Module),
	annotate_meta_vars_in_body(TermA, Module).
annotate_meta_vars_in_body(A=B, _) :-
	!,
	next_call_count(CallCount),
	unifiable(A, B, Unifiers),
	new_aliases(Unifiers, CallCount).

new_aliases([], _).
new_aliases([A = B|Unifiers], CallCount) :-
	(	var(B)
	->	merge_annotations(A, B, CallCount),
		new_alias(A, B, CallCount),
		new_alias(B, A, CallCount)
	;	new_alias(A, B, CallCount)
	),
	new_aliases(Unifiers, CallCount).

merge_annotations(A, B, CallCount) :-
	(	get_meta_annotations_filtered(A, CallCount, MetaAnnotationsA)
	->	(	get_meta_annotations_filtered(B, CallCount, MetaAnnotationsB)
		->	get_meta_annotations(A, AllMetaAnnotationsA),
			get_meta_annotations(B, AllMetaAnnotationsB),
			merge_lists(AllMetaAnnotationsA, MetaAnnotationsB, MetaAnnotationsMergedA),
			merge_lists(MetaAnnotationsA, AllMetaAnnotationsB, MetaAnnotationsMergedB),
			set_meta_annotations(A, MetaAnnotationsMergedA),
			set_meta_annotations(B, MetaAnnotationsMergedB)
		;	set_meta_annotations(B, MetaAnnotationsA)
		)
	;	(	get_meta_annotations_filtered(B, CallCount, MetaAnnotationsB)
		->	set_meta_annotations(A, MetaAnnotationsB)
		;	true
		)
	).

merge_lists([], L2, L2) :- !.
merge_lists(L1, [], L1) :- !.
merge_lists(L1, L2, L3) :-
	append(L1, L2, L12),
	sort(L12, L3).

new_alias(A, B, CallCount) :-
	(	get_aliases(A, Aliases)
	->	true
	;	Aliases = []
	),
	(	alias_member(Aliases, CallCount, B)
	->	true
	;	annotate(aliased(B), CallCount, A),
		check_for_unifiers(Aliases, B, CallCount)
	).

alias_member([Alias-AliasCallCount|Aliases], CallCount, Var) :-
	(	AliasCallCount == CallCount,
		Alias == Var
	->	true
	;	alias_member(Aliases, CallCount, Var)
	).

check_for_unifiers([], _, _).
check_for_unifiers([Alias-AliasCallCount|Aliases], B, CallCount) :-
	unifiable(Alias, B, Unifiers),
	NewCallCount is min(AliasCallCount, CallCount),
	new_aliases(Unifiers, NewCallCount),
	check_for_unifiers(Aliases, B, CallCount).

annotate_meta_vars_in_body(functor(Term, Functor, Arity), _Module) :-
	!,
	(	var(Term),
		var_is_meta_called(Term)
	->	next_call_count(CallCount),
		(	var(Functor)
		->	annotate(functor, CallCount, Functor)
		;	true
		),
		(	var(Arity)
		->	annotate(arity, CallCount, Arity)
		;	true
		)
	;	true
	).

var_is_meta_called(Var) :-
	get_meta_annotations(Var, Annotations),
	member(Annotation-_, Annotations),
	is_meta(Annotation),
	!.

annotate_meta_vars_in_body(atom_concat(A, B, C), _Module) :-
	!,
	(	var(C)
	->	(	var_is_functor_or_meta_called(C)
		->	next_call_count(CallCount),
			annotate_atom_concat(A, B, CallCount)
		;	true
		)
	;	true
	).

var_is_functor_or_meta_called(Var) :-
	get_meta_annotations(Var, Annotations),
	member(Annotation-_, Annotations),
	(	Annotation == functor
	;	is_meta(Annotation)
	),
	!.

annotate_atom_concat(A, B, CallCount) :-
	var(A), var(B), !,
	annotate(is_prefix, CallCount, A),
	annotate(is_suffix, CallCount, B).
annotate_atom_concat(A, B, CallCount) :-
	atomic(A), var(B), !,
	annotate(add_prefix(A), CallCount, B).
annotate_atom_concat(A, B, CallCount) :-
	var(A), atomic(B), !,
	annotate(add_suffix(B), CallCount, A).

annotate_meta_vars_in_body(Term =.. List, _Module) :-
	!,
	(	var(Term),
		var_is_meta_called(Term)
	->	(	var(List)
		->	next_call_count(CallCount),
			annotate(univ_list, CallCount, List)
		;	(	List = [Functor|Args],
				var(Functor)
			->	next_call_count(CallCount),
				(	finite_length(Args, ArgsLength)
				->	annotate(0 -> ArgsLength, CallCount, Functor)
				;	annotate(functor, CallCount, Functor)
				)
			;	true
			)
		)
	;	true
	).

annotate_meta_vars_in_body(asserta(Clause), _Module) :-
	!,
	annotate_database_argument(Clause).
annotate_meta_vars_in_body(assert(Clause), _Module) :-
	!,
	annotate_database_argument(Clause).
annotate_meta_vars_in_body(assertz(Clause), _Module) :-
	!,
	annotate_database_argument(Clause).
annotate_meta_vars_in_body(retract(Clause), _Module) :-
	!,
	annotate_database_argument(Clause).
annotate_meta_vars_in_body(retractall(Clause), _Module) :-
	!,
	annotate_database_argument(Clause).

annotate_database_argument(Clause) :-
	var(Clause),
	!,
	next_call_count(CallCount),
	annotate(database, CallCount, Clause).
annotate_database_argument(Module:Clause) :-
	!,
	next_call_count(CallCount),
	(	var(Module)
	->	annotate(m, CallCount, Module)
	;	true
	),
	(	var(Clause)
	->	annotate(database, CallCount, Clause)
	;	true
	).
annotate_database_argument(_Clause).

annotate_meta_vars_in_body(Goal, Module) :- % TBD: do we trust this?
	predicate_property(Module:Goal, meta_predicate(Head)),
	!,
	functor(Goal, _, Arity),
	next_call_count(CallCount),
	annotate_meta_args(1, Arity, Goal, Head, Module, CallCount).
annotate_meta_vars_in_body(Goal, Module) :-
	inferred_meta(Module:Goal, Head), !,
	functor(Goal, _, Arity),
	next_call_count(CallCount),
	annotate_meta_args(1, Arity, Goal, Head, Module, CallCount).
annotate_meta_vars_in_body(_, _).


%%	annotate_meta_args(+Arg, +Arity, +Goal, +MetaSpec, +Module)

annotate_meta_args(I, Arity, Goal, MetaSpec, Module, CallCount) :-
	I =< Arity, !,
	arg(I, MetaSpec, MetaArg),
	arg(I, Goal, Arg),
	annotate_meta_arg(MetaArg, Arg, Module, CallCount),
	I2 is I + 1,
	annotate_meta_args(I2, Arity, Goal, MetaSpec, Module, CallCount).
annotate_meta_args(_, _, _, _, _, _).

annotate_meta_arg(Spec, Arg, _, CallCount) :-
	var(Arg), !,
	annotate(Spec, CallCount, Arg).
annotate_meta_arg(0, Arg, Module, _CallCount) :- !,
	annotate_meta_vars_in_body(Arg, Module).
annotate_meta_arg(N, Arg, Module, _CallCount) :-
	integer(N),
	callable(Arg), !,
	Arg =.. List,
	length(Extra, N),
	append(List, Extra, ListX),
	ArgX =.. ListX,
	annotate_meta_vars_in_body(ArgX, Module).
annotate_meta_arg(Spec, Arg, _, CallCount) :-
	is_meta(Spec),
	compound(Arg),
	Arg = Module:_,
	var(Module), !,
	annotate(m, CallCount, Module).
annotate_meta_arg(_,_,_,_).

annotate([], _, _) :- !.
annotate([Annotation|Annotations], CallCount, Var) :-
	!,
	annotate(Annotation, CallCount, Var),
	annotate(Annotations, CallCount, Var).
annotate(aliased(Alias), CallCount, Var) :-
	!,
	(	get_attr(Var, pmi, meta(Aliases, Metas))
	->	put_attr(Var, pmi, meta([Alias-CallCount|Aliases], Metas))
	;	put_attr(Var, pmi, meta([Alias-CallCount], []))
	).
annotate(Annotation, CallCount, Var) :-
	(	get_attr(Var, pmi, meta(Aliases, Metas))
	->	put_attr(Var, pmi, meta(Aliases, [Annotation-CallCount|Metas]))
	;	put_attr(Var, pmi, meta([], [Annotation-CallCount]))
	).

get_aliases(Var, Aliases) :-
	get_attr(Var, pmi, meta(Aliases, _)).

get_meta_annotations(Var, Annotations) :-
	get_attr(Var, pmi, meta(_, Annotations)).

get_meta_annotations_filtered(Var, CallCount, FilteredAnnotations) :-
	get_meta_annotations(Var, AllAnnotations),
	filter_annotations(AllAnnotations, CallCount, FilteredAnnotations).

filter_annotations([], _, []).
filter_annotations([Annotation-AnnotationCallCount|AllAnnotations], CallCount, FilteredAnnotations) :-
	(	AnnotationCallCount < CallCount
	->	FilteredAnnotations = [Annotation-AnnotationCallCount|OtherFilteredAnnotations]
	;	FilteredAnnotations = OtherFilteredAnnotations
	),
	filter_annotations(AllAnnotations, CallCount, OtherFilteredAnnotations).

set_meta_annotations(Var, Annotations) :-
	get_attr(Var, pmi, meta(Aliases, _)),
	!,
	put_attr(Var, pmi, meta(Aliases, Annotations)).
set_meta_annotations(Var, Annotations) :-
	put_attr(Var, pmi, meta([], Annotations)).

attr_unify_hook(A0, Other) :-
	writeln(attr_unify_hook(A0, Other)).

join_annotation(A, A, A) :- !.
join_annotation([A|As], [B|Bs], C) :- !,
	append([A|As], [B|Bs], C0),
	sort(C0, C).
join_annotation([A|As], B, C) :- !,
	(	member(B, [A|As])
	->	C = [A|As]
	;	C = [B, A|As]
	).
join_annotation(A, [B|Bs], C) :- !,
	(	member(A, [B|Bs])
	->	C = [B|Bs]
	;	C = [A, B|Bs]
	).
join_annotation(A, B, [A, B]).
%%	meta_annotation(+Head, -Annotation) is semidet.
%
%	True when Annotation is an   appropriate  meta-specification for
%	Head.

meta_annotation(Head, Meta) :-
	functor(Head, Name, Arity),
	functor(Meta, Name, Arity),
	meta_args(1, Arity, Head, Meta, HasMeta),
	HasMeta == true.

meta_args(I, Arity, Head, Meta, HasMeta) :-
	I =< Arity, !,
	arg(I, Head, HeadArg),
	arg(I, Meta, MetaArg),
	meta_arg(HeadArg, MetaArg),
	(	(	is_list(MetaArg),
			member(E, MetaArg),
			is_meta2(E)
		;	is_meta2(MetaArg)
		)
	->  HasMeta = true
	;   true
	),
	I2 is I + 1,
	meta_args(I2, Arity, Head, Meta, HasMeta).
meta_args(_, _, _, _, _).

is_meta(I) :- integer(I), !.
is_meta(_ -> _).
is_meta(:).
is_meta(database).
is_meta(^).
is_meta(//).

is_meta2(S) :- is_meta(S), !.
is_meta2(functor).
is_meta2(arity).
is_meta2(add_prefix(_)).
is_meta2(add_suffix(_)).
is_meta2(is_prefix).
is_meta2(is_suffix).
is_meta2(univ_list).

%%	meta_arg(+AnnotatedArg, -MetaSpec) is det.
%
%	True when MetaSpec is  a  proper   annotation  for  the argument
%	AnnotatedArg. This is simple if the argument is a plain argument
%	in the head (first clause). If it   is  a compound term, it must
%	unify to _:_, otherwise there is no point turning it into a meta
%	argument. If the  module  part  is   then  passed  to  a  module
%	sensitive predicate, we assume it is a meta-predicate.

meta_arg(HeadArg, MetaArg) :-
	get_meta_annotations(HeadArg, Annotations),
	(	setof(A, Pos^(member(A-Pos, Annotations), is_meta2(A)), MetaAnnotations)
	->	Es = MetaAnnotations
	;	setof(E, Pos^member(E-Pos, Annotations), Es)
	),
	!,
	(	Es = [MetaArg]
	;	Es = [_, _|_],
		MetaArg = Es
	),
	!.
meta_arg(_, *).

%%	combine_meta_args(+Heads, -Head) is det.
%
%	Combine multiple meta-specifications.

combine_meta_args([], []) :- !.
combine_meta_args([List], List) :- !.
combine_meta_args([Spec,Spec|Specs], CombinedArgs) :- !,
	combine_meta_args([Spec|Specs], CombinedArgs).
combine_meta_args([Spec1,Spec2|Specs], CombinedArgs) :-
	Spec1 =.. [Name|Args1],
	Spec2 =.. [Name|Args2],
	maplist(join_annotation, Args1, Args2, Args),
	Spec =.. [Name|Args],
	combine_meta_args([Spec|Specs], CombinedArgs).