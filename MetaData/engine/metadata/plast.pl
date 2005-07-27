:- module(plast,
	[	plast_node/1,
		plast_prop/2,
		plast_new_node/2,
		plast_del_tree/1,
		plast_set_prop/2,
		plast_del_prop/2
	]
).

:-dynamic node_id/1,node_attr/2.


plast_prop(compilation_unit_node(P),child(C)):-
    plast_prop(compilation_unit_node(P),member(C)).
plast_prop(brace_node(P),child(C)):-
    plast_prop(brace_node(P),argument(C)).    
plast_prop(list_node(P),child(C)):-
    plast_prop(list_node(P),elements(L)),
    member(C,L).            
plast_prop(compound_node(P),child(C)):-
    plast_prop(compound_node(P),arguments(L)),
    member(C,L).  
plast_prop(Id,Prop):-
    node_attr(Id,Prop).
    
plast_node(Id):-
	node_id(Id).
	    

unused_id(Type,Id):-
    atom_concat(Type,'_node',Fun),
	Num is random(1073741824),
	Try=..[Fun,Num],
    ( node_id(Try)
    ->unused_id(Type,Id)
    ; Id=Try
    ).	

% plast_new_node(+Type,-Id)
% creates a new node of a given type
plast_new_node(Type,Id):-
    unused_id(Type,Id),
    my_assert(node_id(Id)).
    

% plast_del_tree(+Id)
% completely removes an entire subtree.
% Will throw an exception if the node is still 
% referenced by its parent. (remove this link first)
plast_del_tree(Id):-
    (	plast_prop(Id,parent(P)),
    	plast_prop(P,child(Id))
    ->	throw(error(plast_del_tree(Id),referenced_by_parent))
    ;	do_del_tree(Id)
    ).

do_del_tree(Id):-
	forall(plast_prop(Id,child(C)),do_del_tree(C)),
	do_del_node(Id).

do_del_node(Id):-
    retractall(node_attr(Id,_)),
    retractall(node_id(Id)).
	    
plast_set_prop(Id,Prop):-
	my_assert(node_attr(Id,Prop)).

plast_del_prop(Id,Prop):-
	retractall(node_attr(Id,Prop)).	 
	
my_assert(node_id(Id)):-
    ( 	plast_node(Id)
    ->	true
	;   assert(node_id(Id))
	).

my_assert(node_attr(Id,Attr)):-
    (	plast_prop(Id,Attr)
    ->	true
    ;	assert(node_attr(Id,Attr))
    ).
    
    
	   