:- module(pdt_pef_graph,
	[	pef_graph_node/3,
		pef_graph_edge/5,
		pef_graph_set_interesting/1
	]
).

:- use_module(library('pef/pef_base')).
:- use_module(library(pif_observe2)).

:- dynamic '$interesting'/1.


pef_base:pef_after_assert_hook(Id,Type):-
    node_visible(Id),
    notify_node_added(Id,Type).

pef_base:pef_before_retract_hook(Id,Type):-
    node_visible(Id),
    notify_node_removed(Id,Type).



pef_graph_node(Id,Type,Labels):-
    pef_base:pef_node(Id,Type,Labels),
    node_visible(Id).

pef_graph_set_interesting(Id):-
	'$interesting'(Id),!.    
pef_graph_set_interesting(Id):-
    (	node_visible(Id)
    ->	true
    ;	pif_notify(pef_graph(N,Type),added)
   	),
   	forall(
		neighbour(Id,N,From,To),					
		(	node_visible(N)
		->	pif_notify(pef_graph(N,Type),added)
		;	pif_notify(pef_graph(N,Type),added),
			
		)
	),
	assert('$interesting'(Id)).    
    
pef_graph_edge(From,FromType,To,ToType,Label):-
	pef_base:pef_edge(From,FromType,Label,To,ToType),
	edge_visible(From,To).
	
node_visible(Id):-
	'$interesting'(Id).
node_visible(Id):-
    neighbour(Id,Neighbour),
    '$interesting'(Neighbour).

neighbour(Id,Neighbour):-
	neighbour(Id,Neighbour,_,_).    
neighbour(Id,Neighbour,Id,Neighbour):-    
    pef_base:pef_edge(Id,_FromType,_Label,Neighbour,_ToType).
neighbour(Id,Neighbour,Neighbour,Id):-    
    pef_base:pef_edge(Neighbour,_FromType,_Label,Id,_ToType).

    
adjacent_edge(Node,From,To):-
    neighbour(Node,_,From,To).
    
edge_visible(From,To):-
    node_visible(From),
    node_visible(To).
    
notify_node_added(Id,Type):-
	pif_notify(pef_node(Id,Type),added),
	%if node ID is interesting, all adjacent edges are also interesting; 
	%any invisible adjacent nodes should become visible.
	%
	%if the node is just visible, only make those edges visible that 
	%connect ID with another visible node.
	(	'$intersting'(Node)
	->	forall(adjacent_edge(Node,From,To)