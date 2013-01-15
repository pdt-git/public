:- module(pdt_call_analysis, [find_undefined_call/4]).

:- use_module(pdt_prolog_codewalk).

:- dynamic(result/4).

assert_result(QGoal, _, clause_term_position(Ref, TermPosition), Kind) :-
	QGoal = _:Goal,
    (	result(Goal, Ref, TermPosition, Kind)
    ->	true
    ;	assertz(result(Goal, Ref, TermPosition, Kind))
    ),
    !.
assert_result(_,_,_,_).

find_undefined_call(Goal, File, Start, End) :-
	retractall(result(_, _, _, _)),
	pdt_prolog_walk_code([undefined(trace), on_trace(pdt_call_analysis:assert_result)]),
	!,
	retract(result(Goal, Ref, TermPosition, _Kind)),
	(	TermPosition = term_position(Start, End, _, _, _)
	->	true
	;	TermPosition = Start-End
	),
	clause_property(Ref, file(File)).
