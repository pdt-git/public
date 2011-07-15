:- module(edge_counter,[count_call_edges_between_predicates/0,
						call_edges_for_predicates/3]).

:- ensure_loaded('../parse_util').

:- dynamic call_edges_for_predicates/3.
count_call_edges_between_predicates:-
    retractall(call_edges_for_predicates(_,_,_)),
	forall(	call_edge(TargetId, SourceLiteralId),
			(	(	literalT(SourceLiteralId,_,SourceRule,_,_,_),
    				pred_edge(SourceRule,SourceId)
   				)
			->	inc_call_edges_for_predicates(SourceId,TargetId)
			;	format('Problem with call-edge: ~w -> ~w~n',[SourceLiteralId, TargetId])
			)
		).	

inc_call_edges_for_predicates(SourceID,TargetID):-
    call_edges_for_predicates(SourceID,TargetID,Counter),
	retract(call_edges_for_predicates(SourceID,TargetID,Counter)),
 	New_Counter is (Counter + 1),
    assert(call_edges_for_predicates(SourceID,TargetID,New_Counter)).
inc_call_edges_for_predicates(SourceID,TargetID):-  
    assert(call_edges_for_predicates(SourceID,TargetID,1)).
