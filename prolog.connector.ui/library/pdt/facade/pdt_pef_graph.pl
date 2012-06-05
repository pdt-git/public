:- module(pdt_pef_graph,
	[	pef_graph_node/3,
		pef_graph_edge/3,
		pef_graph_set_visible/2,
		pef_graph_clear/0,
		pef_graph_refresh/0,
		pef_graph_property/4,
		pef_graph_action/5,
		pef_graph_action_run/1
	]
).
:- set_prolog_flag(double_quotes,string).
%% pef_graph_action(+Selection,-Action,-Path,-Label,-Style).
%
% find PEF Graph actions for a given selection.
% 
% @param Selection A list of PEF IDs.
% @param Action A term identifying the Action.
% @param Path An atom describing the path to the submenu in which the action should be placed.
% @param Label A string that should be displayed to the user as a label for this action.
% @param Style An atom, one of 'menu', 'action', 'toggle' or 'radio'. Currently only 'menu' and 'action' are supported.

pef_graph_action(Ids,show_neighbours(Ids),[],"Show all neighbours",action).
pef_graph_action([_],args,[],"Show Attributes",menu).
pef_graph_action([_],refs,[],"Show References",menu).
pef_graph_action([Id],ref(Type),[refs],Label,menu):-
    setof(Type,
    	(	pef_edge(Ref,_,Id),
    		pef_type(Ref,Type)
    	),
    	Types
    ),
    member(Type,Types),
    format(string(Label),"~w",[Type]).
pef_graph_action([Id],show_node(To),[args],Label,action):-
	pef_edge(Id,Arg,To),
    format(string(Label),"Show ~w (via ~w)",[To,Arg]).
pef_graph_action([Id],show_node(From),[refs,ref(Type)],Label,action):-
	pef_edge(From,Arg,Id),
	pef_type(From,Type),
    format(string(Label),"Show ~w (via ~w)",[From ,Arg]).
pef_graph_action(Ids,show_args(Ids),[args],"Show all",action).
pef_graph_action(Ids,show_refs(Ids),[refs],"Show all",action).
pef_graph_action(Ids,show_refs(Ids,Type),[refs,ref(Type)],"Show all",action):-    
    setof(Type,
    	Ref^(	member(Id,Ids),
    		pef_edge(Ref,_,Id),
    		pef_type(Ref,Type)
    	),
    	Types
    ),
    member(Type,Types).

pef_graph_action_run(show_node(Pef)):-
    pef_graph_set_visible(Pef,true),
    pef_graph_refresh.
pef_graph_action_run(show_args(Ids)):-
    forall(
		(	member(Id,Ids),
			pef_edge(Id,_,To)
		),
		pef_graph_set_visible(To,true)
	),
    pef_graph_refresh.    
pef_graph_action_run(show_refs(Ids)):-
    forall(
		(	member(Id,Ids),
			pef_edge(From,_,Id)
		),
		pef_graph_set_visible(From,true)
	),
    pef_graph_refresh.    
pef_graph_action_run(show_refs(Ids,Type)):-
    forall(
		(	member(Id,Ids),
			pef_edge(From,_,Id),
			pef_type(From,Type)
		),
		pef_graph_set_visible(From,true)
	),
    pef_graph_refresh.    
pef_graph_action_run(show_neighbours(Ids)):-
	forall(
		(	member(Id,Ids),
			pef_edge(From,_,Id)
		),
		pef_graph_set_visible(From,true)
	),
	forall(
		(	member(Id,Ids),
			pef_edge(Id,_,To)
		),
		pef_graph_set_visible(To,true)
	),
	pef_graph_refresh.

pef_graph_property(argument,Id,Name,Value):-
    pef_type(Id,Type),
    metapef_template(Type,Template),
    pef_generic_query(Type,[id=Id],Data),
    arg(N,Template,Name),
    arg(N,Data,Value).

pef_graph_property(attached,Id,Name,Value):-
    pef_property_query([pef=Id,key=Name,value=Value]).

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







fire_node_added(Id):-
    pif_notify(pef_node(Id),added).
fire_node_removed(Id):-
    pif_notify(pef_node(Id),removed).
fire_edge_removed(FId,Arg,TId):-
    pif_notify(pef_edge(FId,Arg,TId),removed).
fire_edge_added(FId,Arg,TId):-
    pif_notify(pef_edge(FId,Arg,TId),added).

pef_graph_refresh:-
    pif_notify(pef_graph,refresh).

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
    
    
