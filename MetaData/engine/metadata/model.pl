% The "model" defines a graph of interconnected nodes. 
% Nodes  have propertyties. Edges are also basicaly also properties.
% Each node has a well-known type. This type induces a node in the metamodel
% (see module metamodel). The metamodel can roughly be imagined as a gramma that
% produces the model.
% This module offeres predicates for general query and maintainance tasks on the
% model. The "physical" act of adding, removing and querying nodes and thier properties
% is delegated to domain specific model implementations.
% This module should be domain neutral.  Reasoning about semantics carried by the
% model (e.g. "code analysis") should happen in a higher layer.

:- module(model,[
	node/1,
	node_property/2,
	create_node/2,
	delete_node/1,
	set_node_property/2,
	add_node_property/2,
	delete_node_property/2,	
	connect_nodes/4,
	disconnect_nodes/4,
	adjacent/4,
	peer_property/5
	]
	).




:-use_module(metamodel).
:-use_module(runtime).
:-use_module(source).
    
%do_node(A):-
%    runtime_node(A).
do_node(A):-
    source_node(A).

%do_node_property(Id,Prop):-
%    runtime_node_property(Id,Prop).
do_node_property(Id,Prop):-
    source_node_property(Id,Prop).

do_create_node(Type,Id):-
    meta_node_property(Type,tree(source)),
    !,
	create_source_node(Type,Id).
do_delete_node(Id):-
    node_property(Id,type(Type)),
    meta_node_property(Type,tree(source)),
    !,
    delete_source_node(Id).
do_add_node_property(Id,Prop):-
    node_property(Id,type(Type)),
    meta_node_property(Type,tree(source)),
    !,
    add_source_node_property(Id,Prop).
do_delete_node_property(Id,Prop):-
    node_property(Id,type(Type)),
    meta_node_property(Type,tree(source)),
    !,
    delete_source_node_property(Id,Prop).



% node(?Id)
%
% Succeeds if Id can be unified with an existing node.
% 
% This predicate delegates the actual work to concrete 
% model implementations.
node(Id):-
    do_node(Id),
    my_format("touched ~w~n",[Id]).
    
% node_property(?Id, ?PropertyTerm)
%
% Succeeds if a node with the given Id exists and has
% a property that can be unified with PropertyTerm
% 
% This predicate delegates the actual work to concrete 
% model implementations. Add clauses to this predicate 
% for your custom node types.
node_property(Id,Prop):-
	do_node_property(Id,Prop),
	my_format("touched ~w --> ~w~n",[Id,Prop]).

% create_node(+Type, -Id)
%
% create a new node of a given type. The second argument will be unified
% with the Id of the new node. 
create_node(Type,Id):-
	(	ground(Type),var(Id)
	->	true
	;	edit_error(create_node/2,Type,Id,"Id must be a variable")
	),
    check_type_editable(Type),
	do_create_node(Type,Id),
	my_format("created ~w~n",[Id]).

% delete_node(+Node)
%
% delete an editable node from the model.
% This predicate first examines incoming edges (e.g. from parent node)
% to see if they can be removed savely. Otherwise the operation
% will be aborted (unless the model was told to ignore errors)
% Next, all outgoing edges will be removed, in the process of which
% existing subtrees will be orphanized. 
% Node that this predicate does not delete its children.
% 
% See also delete_tree/1, orphan/1
delete_node(Node):-
    check_node_editable(Node),
    node_property(Node,type(Type)),
    forall(meta_edge(Type,Prop),
		(	no_peers_connected(Node,_)
		->	true
		;	edit_error(delete_node/2,Node,Prop,"Cannot delete node with attached peers.")
		)
	),
	do_delete_node(Node),
	my_format("deleted ~w~n",[Node]).
	

    
% set_node_property(+NodeId,+PropertyTerm)
%
% sets a node property, removing any existing property values with
% the same functor. If the latter is not desired use add_node_property/2
%
% This predicate will try to check wether the given PropertyTerm
% is legal according to the respective definitions given in the metamodel.
set_node_property(Node,Property):-
    my_format("enter set_node_property(~w,~w)~n",[Node,Property]),
    check_node_editable(Node),
    node_property(Node,type(Type)),
	check_property_term(Type,Property),
	Property =.. [Name|ValueList],
	meta_node_property(Type,has(Name,Kind,Mult)),
	(	meta_edge_property(Type,Name,_)
	-> edit_error(set_node_property/2,Node,Property,"This predicate does not handle edges. Use connect_nodes/4 ")
	; true
	),
	(	Kind=flag,check_multiplicity(Node,Name,Kind,Mult,ok(1))
	;	ValueList=[Value],check_multiplicity(Node,Name,Kind,Mult,ok(Value))
	->	true
	;	edit_error(set_node_property/2,Node,Property,"operation would violate multiplicity constraints.")
	),
	(	atom(Property)
	->	Term=Property
	;	Term=..[Name|_]
	),
	do_delete_node_property(Node,Term),
	do_add_node_property(Node,Property),
    my_format("succeeded set_node_property(~w,~w)~n",[Node,Property]).
   
% add_node_property(+NodeId, +PropertyTerm)
%
% Add a new property to an existing node.
% This predicate will try to check wether the given PropertyTerm
% is legal according to the respective definitions given in the metamodel.
add_node_property(Node,Property):-
    my_format("enter add_node_property(~w,~w)~n",[Node,Property]),
    check_node_editable(Node),
    node_property(Node,type(Type)),
	check_property_term(Type,Property),
	Property =.. [Name|_],
	meta_node_property(Type,has(Name,Kind,Mult)),
	(	meta_edge_property(Type,Name,_)
	-> edit_error(set_node_property/2,Node,Property,"This predicate does not handle edges. Use connect_nodes/4")
	;true
	),	
	(	Kind=ordered
	->	edit_error(add_node_property/2,Node,Property,"I cannot _add_ ordered properties. Use set_node_property/2 instead.")
	; true
	),	
	(	check_multiplicity(Node,Name,Kind,Mult,can_add)
	->	true
	;	edit_error(add_node_property/2,Node,Property,"operation would violate multiplicity constraints")
	),
	do_add_node_property(Node,Property),
    my_format("succeeded add_node_property(~w,~w)~n",[Node,Property]).
    
delete_node_property(Node,Property):-
    my_format("enter delete_node_property(~w,~w)~n",[Node,Property]),
    check_node_editable(Node),
    node_property(Node,type(Type)),
	check_property_term(Type,Property),
	Property=..[Name|ValueList],
	meta_node_property(Type,has(Name,Kind,Mult)),
	(	meta_edge_property(Type,Name,_)
	-> edit_error(set_node_property/2,Node,Property,"This predicate does not handle edges. Use connect_nodes/4 ")
	;true
	),
	(	(	Kind=flag,check_multiplicity(Node,Name,Kind,Mult,can_delete(1))
		;	ValueList=[Value],check_multiplicity(Node,Name,Kind,Mult,can_delete(Value))
		)
	->	true
	;	edit_error(delete_node_property/2,Node,Property,"operation would violate multiplicity constraints.")
	),
	do_delete_node_property(Node,Property),
    my_format("succeeded delete_node_property(~w,~w)~n",[Node,Property]).	

%	connect_nodes(+Node, +Prop, +Peer +,PeerProp)
%
% connect two nodes with matching edge properties.
% This predicate attemps to check if the operation is allowed
% according to the metamodel before it proceeds.
connect_nodes(Node,Prop,Peer,PeerProp):-
    my_format("enter connect_nodes(~w, ~w, ~w, ~w)~n",[Node,Prop,Peer,PeerProp]),    
    check_node_editable(Node),
    check_node_editable(Peer),
	node_property(Node,type(Type)),
	node_property(Peer,type(PeerType)),
	(	(	meta_edge_property(Type,Prop,direction(Direction)),
			oposite_direction(Direction,PeerDirection)
		)
	->	true		
	;	edit_error(connect_nodes/4,Node,Property,"property is not a directed edge")
	),
	(	meta_edge_property(PeerType,PeerProp,direction(PeerDirection))
	->	true
	;	edit_error(connect_nodes/4,Node,Property,"no matching peer edge")
	),
	(	(	meta_edge_property(Type,Prop,peer_property(PeerProp))
		;	meta_edge_property(PeerType,PeerProp,peer_property(Prop))
		)
	->	true
	;	edit_error(connect_nodes/4,Node,Property,"edge pair fails to define peer_property")
	),
	(	(	check_multiplicity(Node,Prop,can_add),
			check_multiplicity(Peer,PeerProp,can_add)
		)
	->	true
	;	edit_error(connect_nodes/4,Node,Property,"operation violates multiplicity constraints")
	),
	add_or_grow_property(Node,Prop,Peer),
	add_or_grow_property(Peer,PeerProp,Node),
    my_format("succeeded connect_nodes(~w, ~w, ~w, ~w)~n",[Node,Prop,Peer,PeerProp]).


%	disconnect_nodes(+Node, +Prop, +Peer +,PeerProp)
%
% connect two nodes with matching edge properties.
% This predicate attemps to check if the operation is allowed
% according to the metamodel before it proceeds.
disconnect_nodes(Node,Prop,Peer,PeerProp):-
    my_format("enter disconnect_nodes(~w, ~w, ~w, ~w)~n",[Node,Prop,Peer,PeerProp]),
	(	(	adjacent(Node,Direction,Prop,Peer),
			oposite_direction(Direction,PeerDirection),
			adjacent(Peer,PeerDirection,PeerProp,Node)
		)
	->	true		
	;	edit_error(disconnect_nodes/4,Node,Property,"nodes are not mutualy connected")
	),
	(	(	check_multiplicity(Node,Prop,can_delete),
			check_multiplicity(Peer,PeerProp,can_delete)
		)
	->	true		
	;	edit_error(disconnect_nodes/4,Node,Property,"operation violates multiplicity constraints")
	),
	delete_or_shrink_property(Node,Prop,Peer),
	delete_or_shrink_property(Peer,PeerProp,Node),
    my_format("succeeded connect_nodes(~w, ~w, ~w, ~w)~n",[Node,Prop,Peer,PeerProp]).


% adjacent(+Node,?Direction,?Prop,?Peer)
%
% Succeeds if Prop is the name of a Property
% that models an edge which  is pointing in a given Direction
% and connects the given Node to another node Peer.
%		Node			-	The node to search adjacents for.
%		Direction		- 	either "incoming" or "outgoing"
% 		Prop			-	The name of the Property by which the edge is 
%							navigated from Node
%		Peer			-	The node the found edge connects to.
adjacent(Node,Direction,Prop,Peer):-
    node_property(Node,type(Type)),
    meta_node_property(Type,has(Prop,simple,_)),
    meta_edge_property(Type,Prop,direction(Direction)),
	PropTerm=..[Prop,Peer],	
	node_property(Node,PropTerm).
adjacent(Node,Direction,Prop,Peer):-
    node_property(Node,type(Type)),
    meta_node_property(Type,has(Prop,ordered,_)),
    meta_edge_property(Type,Prop,direction(Direction)),
	PropTerm=..[Prop,Peers],	
	node_property(Node,PropTerm),
	member(Peer,Peers).

% peer_property(+Node, +Prop, ?Direction, ?Peer, ?PeerProp)
%
% succeeds if the property Prop of node Node embodies an
% edge to another node Peer and Peer has a corresponding 
% property PeerProp that embodies an edge in the oposite direction.
%
% NOTE: this predicate will not check wether the
% peer edge is actualy connected to Node. In the process of,
% modifying the graph, there might be intermediate states where this
% is not the case. 
% To put it short: this predicate implies 
%		adjacent(Node,Direction,Prop,Peer)
% but it does not imply
%		adjacent(Peer,<oposite direction>,PeerProp,Node)
% In a clean graph however, both implications hold.
peer_property(Node,Prop,Direction,Peer,PeerProp):-
  	node_property(Node,type(Type)),
   	meta_edge_property(Type,Prop,peer_property(PeerProp)),
    adjacent(Node,Direction,Prop,Peer).
peer_property(Node,Prop,Direction,Peer,PeerProp):-
    adjacent(Node,Direction,Prop,Peer),
  	node_property(Peer,type(PeerType)),
   	meta_edge_property(PeerType,PeerProp,peer_property(Prop)).
 

	
%	add_or_grow_property(+Node, +Prop, +Val)
%
% internal method for gracefully blending in ordered properties.
% Will behave like do_add_property/2 for flags or simple props.
% For ordered props, it appends Val to the list instead of creating
% another list.
add_or_grow_property(Node,Prop,Val):-
    node_property(Node,type(Type)),
	(	meta_node_property(Type,has(Prop,ordered,_))
	->	OldTerm=..[Prop,OldVal],
		node_property(Node,OldTerm),
		append(OldVal,[Val],NewVal),
	   	NewTerm=..[Prop,NewVal],
	   	do_delete_node_property(Node,OldTerm),
	   	do_add_node_property(Node,NewTerm)
	;	NewTerm=..[Prop,Val],
	   	do_add_node_property(Node,NewTerm)
	).

% delete_or_shrink_property(+Node, +Prop, +Val)
%
% internal method for gracefully blending in ordered properties.
% Behaves like do_delete_property/2 for flags and simple props.
% For ordered props it delets val from the list instead of deleting
% the whole list. 
delete_or_shrink_property(Node,Prop,Val):-
    node_property(Node,type(Type)),
	(	meta_node_property(Type,has(Prop,ordered,_))
	->	OldTerm=..[Prop,OldList],
		delete(OldList,Val,NewList),
		NewTerm=..[Prop,NewList],
		do_delete_node_property(Node,OldTerm),
		do_add_node_property(Node,NewTerm)
	;	OldTerm=..[Prop,Val],
		do_delete_node_property(Node,OldTerm)
	).

% check_value_list_format(+Kind, +ValueList
%
% called by check_property_term/2 to validate the
% argument of the given property term against the
% constraints defined in the metamodel.
% 		Kind 			-	the kind of the added property 
%		ValueList		-	a list containing the arguments of the added property term
check_value_list_format(flag,[]).
check_value_list_format(simple,[_]).
check_value_list_format(ordered,[[]]).
check_value_list_format(ordered,[[_|_]]).

%	check_node_editable(+Node)
%
% performs common checks on a node that is to be edited.
%
check_node_editable(Node):-
    (	node(Node)
    ->	true
    ;	edit_error(check_node_editable/2,Node,Node,"node does not exist")
    ),
    (	node_property(Node, type(Type))
    ->	true
	;	edit_error(check_node_editable/2,Node,Node,"node has no type")
	),
	check_type_editable(Type).

% check_type_editable(+Type)
%
% checks wether the given type is editable.
check_type_editable(Type):-
     (	meta_node(Type)
     ->	true
	;	edit_error(check_type_editable/2,Type,Type,"type unkown")
	),
    (	meta_node_property(Type,editable)
    ->	true
	;	edit_error(check_type_editable/2,Type,Type,"node type is not editable")
	).
	
% check_property_term(+Type, +PropertyTerm)
%
% performs common checks on property terms that are to be added to
% a node of a given type. 
%
check_property_term(Type,PropertyTerm):-
	(	PropertyTerm =.. [Name|ValueList]
	->	true
	;	edit_error(check_property_term/2,Node,PropertyTerm,"malformed property term")
	),
	(	meta_node_property(Type,has(Name,Kind,_))
	->	true
	;   edit_error(check_property_term/2,Node,PropertyTerm,"type does not allow properties of this name")
	),
	(	check_value_list_format(Kind,ValueList)
	->	true
	;   edit_error(check_property_term/2,Node,PropertyTerm,"malformed value term")
	).


% check_multiplicity(+Node,+PropertyName,+PropertyKind,+Spec, ?Status)
%
% checks the multiplicity state of a given node property.
% see check_multiplicity/3
check_multiplicity(Node,PropertyName,flag,Mult,Status):-
	count(node_property(Node,PropertyName),Count),
	multiplicity(Mult,Count,Status).
check_multiplicity(Node,PropertyName,simple,Mult,Status):-
    Term=..[PropertyName,_],
	count(node_property(Node,Term),Count),
	multiplicity(Mult,Count,Status).
check_multiplicity(Node,PropertyName,ordered,Mult,Status):-
    Term=..[PropertyName,List],
	node_property(Node,Term),
	length(List,Count),
	multiplicity(Mult,Count,Status).
check_multiplicity(Node,PropertyName,ordered,Mult,can_add(Value)):-
    length(Value,V),
    check_multiplicity(Node,PropertyName,ordered,Mult,can_add(V)).
check_multiplicity(Node,PropertyName,simple,Mult,can_add(_)):-
    check_multiplicity(Node,PropertyName,simple,Mult,can_add(1)).
check_multiplicity(Node,PropertyName,flag,Mult,can_add(_)):-
    check_multiplicity(Node,PropertyName,flag,Mult,can_add(1)).

% check_multiplicity(+Node, +PropertyName, ?Status)
%
% successivly unifies all multiplicity states with Status
% that apply to the given property of the given node.
% In addition to the status terms offered by multiplicity/3,
% this predicates introduces can_add(Value) and
% can_delete(Value)
% see multiplicity/3
check_multiplicity(Node,PropertyName,Status):-
	node_property(Node,type(Type)),
	meta_node_property(Type,has(PropertyName,Kind,Mult)),
	check_multiplicity(Node,PropertyName,Kind,Mult,Status).

% multiplicity(+Spec, +Current, ?Status)
%
% checks a number against a specified multiplicity constraint.
%	Spec		-	the multiplicity constraint specification. See metamodel
%	Status	-	can_add, can_delete and/or ok, 
multiplicity(0-n,_,ok).
multiplicity(N-M,C,ok):-
    (compare(=,N,C); compare(<,N,C)),
    (compare(=,C,M); compare(<,C,M)).
multiplicity(_-n,_,can_add).
multiplicity(_-N,C,can_add):-
    compare(<,C, N).
multiplicity(N-_,C,can_delete):-
    compare(<,N,C).
multiplicity(N-n,_,ok(V)):-
    (compare(=,N,V); compare(<,N,V)).
multiplicity(N-M,_,ok(V)):-
    (compare(=,N,V); compare(<,N,V)),
    (compare(=,V,M); compare(<,V,M)).
multiplicity(_-n,_,can_add(_)).
multiplicity(_-N,C,can_add(V)):-
    CC is C+V,
    compare(=,CC, N),compare(<,CC, N).
multiplicity(N-_,C,can_delete(V)):-
    CC is C-V,
    compare(=, N, CC),compare(<,N,CC).


% count(+Goal, -Count)
%
% unifies Count withthe number of solutions to Goal 	
count(Goal,Count):-
    nb_setval(my_counter,0),
    (	(	Goal,
    		nb_getval(my_counter,I),
    		succ(I,II),
    		nb_setval(my_counter,II),
    		fail
    	)
    ; 	nb_getval(my_counter,Count),
    	nb_delete(my_counter)
    ).


 
% all_peers_connected(+Node, +Prop)
%
% succeeds if all peers that are reachable from Node via edges named Prop
% that have a corresponding peer property to Prop are connected to Node
% via their respective peer properties.
all_peers_connected(Node,Prop):-
	forall(peer_property(Node,Prop,Direction,Peer,PeerProp),		
		oposite_direction(Direction,PeerDirection),
		adjacent(Peer,PeerDirection,PeerProp,Node)
	).

% no_peers_connected(+Node, +Prop)
%
% succeeds if none of the peers that are reachable from Node via edges named Prop
% that have a corresponding peer property to Prop is connected to Node
% via its respective peer properties.	
%
% This is an intermediate condition that should be checked e.g. before removeing a 
% child from its parents: The child will still reference its parents, but the parents
% should detach from the child before it is removed.
no_peers_connected(Node,Prop):-
	forall(peer_property(Node,Prop,Direction,Peer,PeerProp),		
		oposite_direction(Direction,PeerDirection),
		\+ adjacent(Peer,PeerDirection,PeerProp,Node)
	).




oposite_direction(incoming,outgoing).
oposite_direction(outgoing,incoming).
    

% how_to_handle_edit_errors(?How).
% defines how errors detected in one of the model-modifying predicates
% should be handled.
% 	-	The argument will be unified with either fail, ignore, or throw.
how_to_handle_edit_errors(throw).

% edit_error(+Operation,+Node,+Property,+Message)
% 
% responds to errors in the errors detected in one of the
% model-modifying predicates.
%  -	Operation should be the predicate that encounters the problem
%	 -	Property should be the edited Property, or the Node itself if 
% 		no Property was affected.
%   -	Message should be a human-readable description of the problem.
edit_error(_,_,_,_):-
    how_to_handle_edit_errors(fail),
    fail.
edit_error(_,_,_,_):-
    how_to_handle_edit_errors(ignore),
    true.
edit_error(Operation,Node,Property,Message):-
    how_to_handle_edit_errors(throw),
    string_to_atom(Message,AMessage),
    throw_edit_error(Operation,Node,Property,AMessage).

throw_edit_error(Operation,Node,Node,Message):-
    writeln(error(model_modification_error,context(Operation,Node,Message))),
    throw(error(model_modification_error,context(Operation,Node,Message))).
throw_edit_error(Operation,Node,Property,Message):-
	writeln(error(model_modification_error,context(Operation,Node->Property,Message))),
    throw(error(model_modification_error,context(Operation,Node->Property,Message))).    

my_format(_,_).

