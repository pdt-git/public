:- module(pdt_pef_graph,
	[	pef_graph_node/3,
		pef_graph_edge/3,
		pef_graph_set_visible/2,
		pef_graph_clear/0
	]
).


/*

This module adds a layer on top of the graph induced by the pef meta
edges. Since the user should be presented with a small part of the graph only, 
this layer acts as a filter.  

Nodes are either visible or not.

Edges are visible if both involved nodes are visible.

Users of this interface may modify the visibility of nodes.
This interface will notify users of edges/nodes becoming visible/invisible.

Fancy ways of selecting should all be realized in a higher level, maybe even on the java side.
This interface may provide helper predicates for this purpose.
*/
:- use_module(library('pef/pef_base')).
:- use_module(library(pif_observe2)).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).





:- dynamic '$node_visible'/1.

pef_graph_clear:-
    retractall('$node_visible'(_)).



spyme.



fire_node_added(Id):-
    pif_notify(pef_node(Id),added).
fire_node_removed(Id):-
    pif_notify(pef_node(Id),removed).
fire_edge_added(FId,Arg,TId):-
    pif_notify(pef_edge(FId,Arg,TId),added).
fire_edge_removed(FId,Arg,TId):-
    pif_notify(pef_edge(FId,Arg,TId),removed).


pef_graph_node(Id,Type,Labels):-
    '$node_visible'(Id),
    pef_node(Id,Type,Labels).
    

    
    
pef_graph_edge(From,Label,To):-
	'$node_visible'(From),
	pef_edge(From,Label,To),	
	'$node_visible'(To).
		

pef_graph_set_visible(Node,Value):-
    (	Value==true, \+ '$node_visible'(Node)
    ->	assert('$node_visible'(Node)),
    	fire_node_added(Node),
    	forall(
    		(	pef_edge(Node,Arg,To),
    			'$node_visible'(To)
    		),
    		fire_edge_added(Node,Arg,To)
    	),
    	forall(
    		(	pef_edge(From,Arg,Node),
    			'$node_visible'(From)
    		),
    		fire_edge_added(From,Arg,Node)
    	)
    ;	Value==false, '$node_visible'(Node)
    ->	retract('$node_visible'(Node)),
    	fire_node_removed(Node),
    	forall(
    		(	pef_edge(Node,Arg,To),
    			'$node_visible'(To)
    		),
    		fire_edge_removed(Node,Arg,To)
    	),
    	forall(
    		(	pef_edge(From,Arg,Node),
    			'$node_visible'(From)
    		),
    		fire_edge_removed(From,Arg,Node)
    	)
    ;	true
    ).
    
    
