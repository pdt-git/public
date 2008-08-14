:- module(builder__actions,
	[	add_req/3, 
		add_bwd/3,
		add_lck/3,
		add_dep/2,
		start_build/1,
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
	% TODO: only propagate max bwd.
	set_edge_label(From,To,Client,bwd),
	send_message_target_client(To,Client,implied(To)).

add_lck(From,Client,To):-
	set_edge_label(From,To,Client,lck),
	% do not send notification if this is an implied lock
	% i.e. it's not direct, and From is not beeing build.
	(	From == []
	->	send_message_target_client(To,Client,grant(To))
	;	edge_label(_,From,Client,bld(_))
	->	send_message_target_client(To,Client,grant(To))
	;	true
	).
		

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
	
build_obsolete/1,		
report_error/2,
propagate_invalid/1