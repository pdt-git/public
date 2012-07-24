/* $LICENSE_MSG$ */

:- module(dead_predicate_finder,[	uncalled_predicate/1,
									uncalled_local_predicate/1,
									locally_dead_predicate/1]).

:- ensure_loaded('../pdt_factbase').
:- use_module('../modules_and_visibility').
:- use_module('edge_counter').


locally_dead_predicate(Dead) :-
    locally_dead_predicate(Dead, [Dead]).

locally_dead_predicate(Dead, _Visited):-
    uncalled_local_predicate(Dead).
locally_dead_predicate(Dead, Visited):-
    forall(	
    	(call_edges_for_predicates(Caller,Dead,_), Caller \== Dead, not(member(Caller, Visited))),
    	locally_dead_predicate(Caller, [Caller | Visited])
    ),
    \+(exporting(_,Dead,_)).
    		
    
    


uncalled_local_predicate(Uncalled):-
    uncalled_predicate(Uncalled),
    \+(exporting(_,Uncalled,_)).


uncalled_predicate(Uncalled):-
    predicateT(Uncalled,_,_,_,_),
    \+(call_edge(Uncalled,_)).

