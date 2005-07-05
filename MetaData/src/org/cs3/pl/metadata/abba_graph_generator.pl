:- module(abba_graph_generator,[
	abba_assert_data/1, 
	abba_put_local_symbol/2, 
	abba_clear_local_symbols/0
]).

:- dynamic last_id/1,global_id/2, local_symbol/2, local_id/2.
:- thread_local local_symbol/2, local_id/2.
:- module_transparent abba_assert_data/1.

abba_assert_data(Term):-
	replace_local_ids_in_args(Term,GlobalTerm),
	assert(GlobalTerm).
abba_put_local_symbol(local_id(Local),Symbol):-
	(	local_symbol(Local,Symbol)
	->	true
	;	assert(local_symbol(Local,Symbol))
	).	
abba_clear_local_symbols:-
	retractall(local_symbol(_,_)),
	retractall(local_id(_,_)).



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
    
		
replace_local_ids_in_list([],[]).
replace_local_ids_in_list([local_id(LId)|LT],	[GId|GT]):-
    !,
    map_id(LId,GId),
    replace_local_ids_in_list(LT,GT).
replace_local_ids_in_list([X|LT],	[X|GT]):-
    !,
    replace_local_ids_in_list(LT,GT).

replace_local_ids_in_args(Term,Global):-
	Term=.. TermList,
	replace_local_ids_in_list(TermList,GlobalList),
	Global=.. GlobalList.
	
unused_id(Id):-
	last_id(Last),
	Id is Last + 1.
