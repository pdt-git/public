:- module(builder__actions,
	[	add_req/3, 
		add_bwd/3,
		add_lck/3,
		rem_lck/3,
		add_dep/2,
		start_build/3,
		build_done/1,
		build_obsolete/1,		
		report_error/2,
		propagate_invalid/1
	]
).

:- use_module(builder__graph).
:- use_module(builder__messages).

add_req(From,Client,To):-
	set_edge_label(From,To,Client,req). 

add_bwd(From,Client,To):-
	% determine depth of To
	target_depth(To,ToDepth),
	(	edge_label(From,Other,Client,bwd(OtherDepth))
	->	% If From already has  an outgoing bwd edge,
		% two cases have to be differentiated:
		(	OtherDepth =< ToDepth
		->	% if the existing edge has a smaller or equal 
			% depth than the new one, keep it. Instead
			% of the new one, we add a normal dependency.
			add_dependency(From,To), %just in case there is no edge yet.
			clear_edge_label(From,To,Client)
		;	% Otherwise replace the existing edge with a
			% normal dependency, and add the new edge.
			clear_edge_label(From,Other,Client),
			set_edge_label(From,To,Client,bwd(ToDepth))
		)
	;	% If from has no outgoing bwd edge yet, we can simply
		% add the new edge.
		set_edge_label(From,To,Client,bwd(ToDepth))
	),
	% in each case, we need to notify the client.
	send_message_target_client(To,Client,implied(To)).

target_depth(Target,Depth):-
	(	edge_label(Target,_,_,bwd(N))
	->  Depth=N
	;	edge_label(_,Target,_,bld(N))
	->	Depth=N
	).

add_lck(From,Client,Target):-
	% PDT-310: need to avoid confluent lck edges
	% if there is an incoming lck edge by the requesting client, 
	% send implied instead of grant. Also do not propagate the request in this case.
	(	edge_label(_,Target,Client,lck)
	->	Notification = implied(Target)
	;	Notification = grant(Target)
		% PDT-308: We need to propagate the request along outgoing dep edges
		forall(
			target_depends(Target,To),
			send_message_target_target(Target,To,req(Target,Client)
		)
	),	
	set_edge_label(From,Target,Client,lck),
	% do not send notification if this is an implied lock
	% i.e. it's not direct, and From is not being build.
	(	From == []
	->	send_message_target_client(Target,Client,Notification)
	;	edge_label(_,From,Client,bld(_))
	->	send_message_target_client(Target,Client,Notification)
	;	true
	).

% see PDT-307
grant_waiting(Target):-
	% turn all incoming req edges to lck edges,
	% and notify clients.
	forall(
		edge_label(From,Target,Client,req),
		add_lck(From,Client,To) % this should do.
	).
	

	
rem_lck(From,Client,To):-
	% NB: This one is not described in the DA. 
	% But if i remember correctly, it was trivial:
	clear_edge_label(From,To,Client).

add_dep(From,To):- 
	% we assume that some client edge exists.
	% if we clear all client edges, only the dependency remains.
	clear_edge_label(From,To,_).
	
start_build(From,Client,To):-
	clear_dependencies(To),
	(	edge_label(_,From,Client,bld(N))
	->	M is N +1
	;	M = 1
	),		
	set_edge_label(From,To,Client,bld(M)),
	send_message_target_client(To,Client,build(To)).

build_done(Target):-
	% If there is an outgoing bwd edge, replace it with a normal dep edge. See PDT-306
	forall(
		edge_label(Target,To,Client,bld(_)),
		clear_edge_label(Target,To,Client)
	),		
	% clear incoming bld(_) edge, leaving a simple dependency
	forall(
		edge_label(From,Target,Client,bld(_)),
		clear_edge_label(From,Target,Client)
	),
	
	% do the same for incoming bwd(_) edges, but send success
	% messages back to the respective peers. 
	% This should recursively mark the whole scc as 
	% consistent, once the build of the first node is complete.
	forall(
		edge_label(From,Target,Client,bwd(_)),
		(	clear_edge_label(From,Target,Client),
			send_message_target_target(Target,From,success)
		)
	).
	
build_obsolete(Target):-
	% funny as this may sound: the actions are 
	% completely the same.
	build_done(Target).	

report_error(Target, Error):-
	% the error has to be propagated back along all
	% incoming req.	
	forall(
		edge_label(_,Target,Client,req),				
		send_message_target_client(Target,Client,error(Error))		
	),
	% all incoming edges are (req, bld, bwd) are replaced with 
	% normal dep edges.
	clear_edge_label(_,Target,_).
	
	
propagate_invalid(Target):-
	% propagate back along incoming deps.
	% Note: ANY edge implies a dep edge. It suffices to
	% propagate along incoming dep edges.
	forall(
		target_depends(From,Target),
		send_message_target_target(Target,From,invalidate)
	).
