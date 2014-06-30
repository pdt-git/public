:- module(prolog_refactoring_demo_renaming, [
	rename_predicate/6   % (+M:N/A, +NewName, ?File, ?Start, ?End, ?NewText)
]).

:- use_module(library(prolog_clause)).
:- use_module(pdt_common_pl('callgraph/pdt_call_graph')).

:- dynamic(result/4).

assert_result(Goal, Caller, From, CallKind) :-
	assertz(result(Goal, Caller, From, CallKind)).

%% rename_predicate(+PI, +NewName, File, Start, End, NewText) 
%
% Rename M:N/A replacing N by NewName.
% File, Start, End and NewText form a Text Change.
% In the case of a predicate renaming Length is the Length of N 
% and NewText = NewName in all TextChange values.

rename_predicate(M:N/A, NewName, File, Start, End, NewText) :-
	NewText = NewName,
	position_of(M:N/A, File, Start, End). 

% We need all declarations (predicate heads) and calls of M:N/A.
% F is the file and S is the start position of N in the file: 
position_of(M:N/A, File, Start, End) :- declaration_positon(M:N/A, File, Start, End).
position_of(M:N/A, File, Start, End) :- call_positon(       M:N/A, File, Start, End).


declaration_positon(M:N/A, File, Start, End) :-
	functor(Head, N, A),
	clause(M:Head, _, Ref),
	clause_info(Ref, File, TermPosition, _),
	(	clause_property(Ref, fact)
	->	% fact
		TermPosition = HeadPosition
	;	% clause with body
		TermPosition = term_position(_, _, _, _, [HeadPosition|_])
	),
	(	HeadPosition = Start-End
	->	% no arguments
		true
	;	% at least one argument
		HeadPosition = term_position(_, _, Start, End, _)
	). 

call_positon(M:N/A, File, Start, End) :-
	retractall(result(_, _, _, _)),
	functor(Head, N, A),
	pdt_walk_code([trace_reference(M:Head), on_trace(prolog_refactoring_demo_renaming:assert_result)]),
	!,
	retract(result(_Goal, _Caller, From, _CallKind)),
	From = clause_term_position(Ref, TermPosition),
	clause_property(Ref, file(File)),
	(	TermPosition = Start-End
	->	true
	;	TermPosition = term_position(_, _, Start, End, _)
	).
