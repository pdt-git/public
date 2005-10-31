:- module(abba_graph_generator,[
	abba_begin_cu/1,
	abba_assert_data/1, 
	abba_put_local_symbol/2, 
	abba_clear_local_symbols/0
]).

:- dynamic global_id/2, local_symbol/2.
:- thread_local local_symbol/2.
:- module_transparent abba_assert_data/1.

	    
abba_begin_cu(File):-
	retract_cu(File),		
	set_current_cu(File).	

abba_assert_data(Term):-
	replace_local_ids_in_args(Term,GlobalTerm),
	context_module(M),
	register_node_with_cu(M,GlobalTerm),
	assert(GlobalTerm).
	
abba_put_local_symbol(local_id(Local),Symbol):-
	(	local_symbol(Local,Symbol)
	->	true
	;	put_local_symbol(Local,Symbol)
	).	

abba_clear_local_symbols:-
    clear_local_symbols.
%--------------------------------------------------
% db manipulating preds

set_current_cu(Cu):-
	nb_setval(current_cu,Cu).
current_cu(Cu):-
    nb_getval(current_cu,Cu).
    
add_cu_member(Cu,M,Member):-
	recordz(Cu,M:Member).

cu_member(Cu,M,Member):-
	recorded(Cu,M:Member).	
	
clear_cu_members(Cu):-
	forall(recorded(Cu,_,Ref),erase(Ref)).	

put_local_symbol(Local,Symbol):-
    assert(local_symbol(Local,Symbol)).
clear_local_symbols:-
	retractall(local_symbol(_,_)).

%local_symbol(Local,Symbol):-
%    local_symbol(Local,Symbol)

put_local_id(Local,Global):-
    thread_self(Me),
    recordz(Me,Local),
    concat_atom([local_,Local],Atom),
    nb_setval(Atom,Global).
local_id(Local,Global):-
    concat_atom([local_,Local],Atom),
	catch(nb_getval(Atom,Global),_,fail).
	    
clear_local_ids:-
    thread_self(Me),
    forall(
    	recorded(Me,Local,Ref),
    	(	nb_delete(Local),
    		erase(Ref)
    	)
    ).
    
put_global_id(Symbol,Global):-
    assert(global_id(Symbol,Global)).
%global_id(Symbol,Global).   

retract_cu(File):-
	forall(cu_member(File,M,Node),retract_node(M,Node)),
	clear_cu_members(File).
	
retract_node(M,Id):-
	M:retractall(edge(_,_,_,Id,_)),
	M:retractall(edge(_,_,_,_,Id)),
	M:retractall(property(Id,_)),
	M:retractall(node(Id,_,_)).

%-------------------------------------------------------------

register_node_with_cu(M,node(Id,_,_)):-
    (	current_cu(Cu)
    ->	add_cu_member(Cu,M,Id)
    ;	throw(error(no_current_cu))
    ).

register_node_with_cu(_,_).    


map_id(Local,Global):-
    local_symbol(Local,Symbol),
    !,
    (	global_id(Symbol,Global)
    ->	true
    ;	unused_id(Global),
	    put_global_id(Symbol,Global)
    ).
map_id(Local,Global):-
    (	local_id(Local,Global)
    ->	true
    ;	unused_id(Global),
    	put_local_id(Local,Global)
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
	flag(next_id,Id,Id+1).
