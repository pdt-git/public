:- module(pdt_pef_graph,
	[	pef_graph_node/3,
		pef_graph_edge/5,
		pef_graph_set_interesting/2
	]
).


/*

This module adds a layer on top of the graph induced by the pef meta
edges. In the additional layer, each node is labeled with a visibility value.
This value is updated automatically and is functionally defined as follows:

Each node has an attribute 'source', which is a reference to the node itself
or one of its neighbour nodes with a maximum visibility value.

Nodes that refer to them self are called 'fixed nodes', their visibility is not calculated,
but it is fixed to some non-negative integer.

For other nodes the visibility value is the maximum visibility value of all neighbours minus one.
Values <0 are truncated to 0.

Nodes with visibility > 0 are considered visible.

The Visibility of visible nodes is cached to improve performance. Update propagation occurs 
in a push-style and is always starting with a fixed node.   

*/


%%
% visible_distance(-Distance).
% Unifies distance with the current visible distance.
visible_distance(3).

:- use_module(library('pef/pef_base')).
:- use_module(library(pif_observe2)).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).





pef_base:pef_after_assert_hook(Id,Type):-    
    calculate(Id-Type).
    

pef_base:pef_before_retract_hook(Id,Type):-
    node_value(Id-Type,OldValue),
    set_node_data(Id-Type,0,Id-Type),
    fire_value_changed(0,OldValue,Id-Type),
    propagate_neighbours(Id-Type,0),       
    delete(Id-Type).
    


:- dynamic '$node_data'/4.

node_data((Id-Type),Value,Source):-
    '$node_data'(Id,Type,Value,Source),
    !.
node_data(_Node,0,[]).

set_node_data((Id-Type),Value,Source):-
    retractall('$node_data'(Id,Type,_,_)),
	(	Value==0,Source==[]
	->	true
	;	assert('$node_data'(Id,Type,Value,Source))
	).


spyme.

node_fixed(Node):-
    node_source(Node,Src),
    Node==Src.

node_source(Node,Source):-
    (	\+ \+ Source = _-pef_clause
    ->	spyme
    ;	true
    ),
    node_data(Node,_,Source).

node_value(Node,Value):-
	node_data(Node,Value,_).    


%% calculate(+Node)
% recalculate the visibility value of a given node.
% The visibility of adjacent nodes is assumed to be up-to-date.
calculate(Node):-
    node_fixed(Node),!.
calculate(Node):-
	node_value(Node,OldValue),
	max_neighbour(Node,MaxValue,MaxNeighbour),
	NewValue is max(MaxValue - 1, 0),
	set_node_data(Node,NewValue,MaxNeighbour),
	fire_value_changed(NewValue,OldValue,Node).

%%
% propagate(+Target,+Source,+Value).
% propagate a node value through the graph.
%
% @param Target the node that should be updated.
% @param Source a neighbour of node from which the update is propagated
% @param Value the value of the source node
propagate(Target,Source,Value):-
    propagate_XXX(Target,Source,Value),
    !.
propagate(Target,Source,Value):-
    throw(failed(propagate(Target,Source,Value))).
propagate_XXX(Target,_Source,_Value):-
    node_fixed(Target),!.
propagate_XXX(Target,Source,Value):-
    node_value(Target,TargetValue),
    TargetValue >= Value,
    !,
    (	\+ \+ node_source(Target,Source)
    ->	max_neighbour(Target,MaxValue,MaxNeighbour),
    	NewValue is max(MaxValue -1,0),
    	set_node_data(Target,NewValue,MaxNeighbour),
    	fire_value_changed(NewValue,TargetValue,Target)
    ;	true % nothing to do.
    ).
propagate_XXX(Target,Source,Value):-
    % target value < Value, but we need the old value anyway.
    node_value(Target,TargetValue),
    (	node_source(Target,Source)
    ->	NewValue is max(Value -1,0),
    	set_node_data(Target,NewValue,Source),
    	fire_value_changed(NewValue,TargetValue,Target),
    	propagate_neighbours(Target,NewValue)
    ;	%If source is not the max neighbour, even after the update,
    	%then target needs no update. If it wasn't the max neighbour, 
    	%but is now, then MaxValue equals Value.
    	max_neighbour(Target,Value,Source)
    ->	NewValue is max(Value -1,0),
    	set_node_data(Target,NewValue,Source),
    	fire_value_changed(NewValue,TargetValue,Target),
    	propagate_neighbours(Target,NewValue)
    ;	true
    ).
    

propagate_neighbours(Src,Value):-
    adjacents(Src,Neighbours),
    propagate_neighbours_X(Neighbours,Src,Value).

propagate_neighbours_X([],_,_).
propagate_neighbours_X([N|Ns],Src,Value):-
    propagate(N,Src,Value),
    propagate_neighbours_X(Ns,Src,Value).
    


adjacents(Node,Adjacents):-
    findall(Adjacent,neighbour(Node,Adjacent),Adjacents).

max_neighbour(Node,MaxValue,MaxNeighbour):-
    adjacents(Node,Neighbours),
    max_neighbour_X(Neighbours,-1,MaxValue,[],MaxNeighbour).

max_neighbour_X([],DefaultValue,DefaultValue,DefaultNode,DefaultNode).
max_neighbour_X([Node|Nodes],DefaultValue,MaxValue,DefaultNode,MaxNode):-
    node_value(Node,Value),
    (	Value > DefaultValue
    ->	NextValue = Value,
    	NextNode = Node
    ;	NextValue = DefaultValue,
    	NextNode = DefaultNode
    ),
    max_neighbour_X(Nodes,NextValue,MaxValue,NextNode,MaxNode).    


delete(Id-Type):-    
    retractall('$node_data'(Id,Type,_,_,_)).
release(Id-Type):-    
    node_data(Id-Type,Time,NewValue,_),    
    retractall('$node_data'(Id,Type,_,_,_)),
    assert('$node_data'(Id,Type,Time,NewValue,false)).
    


fire_value_changed(0,0,_):-
	!.
fire_value_changed(0,OldValue,Node):-
    !,
    OldValue > 0,
    forall(
    	neighbour(Node,_,From,To),
    	fire_edge_removed(From,To)
    ),
    fire_node_removed(Node).   
fire_value_changed(NewValue,0,Node):-
    !,
    NewValue > 0,
    fire_node_added(Node),
    forall(
    	(	neighbour(Node,Neighbour,From,To),
    		node_visible(Neighbour)
    	),
    	fire_edge_added(From,To)
    ).
        
fire_value_changed(_,_,_).    


fire_node_added(Id-Type):-
    pif_notify(pef_node(Id,Type),added).
fire_node_removed(Id-Type):-
    pif_notify(pef_node(Id,Type),removed).
fire_edge_added(FId-FType,TId-TType):-
    pif_notify(pef_edge(FId,FType,TId,TType),added).
fire_edge_removed(FId-FType,TId-TType):-
    pif_notify(pef_edge(FId,FType,TId,TType),removed).


pef_graph_node(Id,Type,Labels):-
    pef_base:pef_node(Id,Type,Labels),
    node_visible(Id-Type).

pef_graph_set_interesting(Id,Type):-
	visible_distance(NewValue),
	node_value(Id-Type,OldValue),
    set_node_data(Id-Type,NewValue,Id-Type),
    fire_value_changed(NewValue,OldValue,Id-Type),
    propagate_neighbours(Id-Type,NewValue).       
    
    
pef_graph_edge(From,FromType,To,ToType,Label):-
	pef_base:pef_edge(From,FromType,Label,To,ToType),
	edge_visible(From-FromType,To-ToType).
	
node_visible(Node):-
	node_value(Node,Val),
	Val > 0.

neighbour(Node,Neighbour):-
	neighbour(Node,Neighbour,_,_).    
neighbour(Id-Type,NId-NType,Id-Type,NId-NType):-    
    pef_base:pef_edge(Id,Type,_Label,NId,NType).
neighbour(Id-Type,NId-NType,NId-NType,Id-Type):-    
    pef_base:pef_edge(NId,NType,_Label,Id,Type).

    
    
edge_visible(From,To):-
    node_visible(From),
    node_visible(To).
    
