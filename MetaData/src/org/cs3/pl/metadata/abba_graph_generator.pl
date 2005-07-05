:- module(abba_graph_generator,[abba_assert_global/1]).

:- dynamic last_id/1,global_id/2, local_symbol/2, local_id/2.
:- thread_local local_symbol/2, local_id/2.

last_id(0).
map_id(Local,Global):-
    local_symbol(Local,Symbol),
    !,
    (	global_id(Symbol,Global)
    ->	true
    ;	unused_id(Global),
	    assert(global_id(Symbol,Global)),
	    retractall(last_id(_)),
	    assert(last_id(Global))
    ).
map_id(Local,Global):-
    (	local_id(Local,Global)
    ->	true
    ;	unused_id(Global),
    	assert(local_id(Local,Global)),
	    retractall(last_id(_)),
	    assert(last_id(Global))
    ).
    
abba_assert_global(Term):-
	replace_local_ids(Term,GlobalTerm),
	assert(GlobalTerm).
	
	
replace_lcoal_ids([],[]).
replace_local_ids([edge_id(LId)|LT],	[edge_id(GId)|GT]):-
    !,
    map_id(LId,GId),
    replace_local_ids(LT,GT).
replace_local_ids([node_id(LId)|LT],	[node_id(GId)|GT]):-
    !,
    map_id(LId,GId),
    replace_local_ids(LT,GT).
replace_local_ids(Term,Global):-
	Term=.. TermList,
	replace_local_ids(TermList,GlobalList),
	Global=.. GlobalList.
	
unused_id(Id):-
	last_id(Last),
	Id is Last + 1.
