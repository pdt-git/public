:- module(builder__debug,
	[	dbg_step/1,
		dbg_step_describe/2,
		dbg_step_receive/2,
		dbg_step_send/3,
		dbg_write_dot/0
	]
).

:- dynamic step_subject_predecessor/3, step_data/2,message/3,step_trigger/2.
:- use_module(builder__graph).

dbg_disabled:-fail.

dbg_step(_):-
	dbg_disabled,
	!.
dbg_step(Subject):-
	check_nonvar(Subject),
	flag('$builder__debug_step_counter',Step,Step+1),
	(	current_step(Subject,Prev)
	->	true
	;	Prev=[]
	),
	asserta(step_subject_predecessor(Step,Subject,Prev)).

current_step(Subject,Step):-
	check_nonvar(Subject),
	once(step_subject_predecessor(Step,Subject,_)).

have_current_step(Subject,Step):-
	check_nonvar(Subject),
	(	current_step(Subject,Step)
	->	true
	;	dbg_step(Subject),
		current_step(Subject,Step)
	).

dbg_step_describe(_,_):-
	dbg_disabled,
	!.
dbg_step_describe(Subject,Description):-
	check_nonvar(Subject),
	have_current_step(Subject,Step),
	assert(step_data(Step,Description)).

dbg_step_receive(_,_):-
	dbg_disabled,
	!.	
dbg_step_receive(Subject,Message):-
	check_nonvar(Subject),
	have_current_step(Subject,Step),
	(	step_trigger(Step,_)
	->	throw(error(multiple_triggers,_))
	;	assert(step_trigger(Step,Message))
	).
	
dbg_step_send(_,_,_):-
	dbg_disabled,
	!.		
dbg_step_send(Subject,Message,Data):-
	check_nonvar(Subject),
	have_current_step(Subject,Step),
	assert(message(Message,Step,Data)).

 
 check_nonvar(Subject):-
 	(	nonvar(Subject)
 	->	true
 	;	trace,
 		throw(no_subject)
 	).
 	
 concat_label_lines([],[]).
 concat_label_lines([Data|Lines],Label):-
 	format(codes(Label,Tail),"~W",[Data,[numbervars(true)]]),
 	concat_label_lines2(Lines,Tail).	
 concat_label_lines2([],[]).
 concat_label_lines2([Data|Lines],Label):-
 	format(codes(Label,Tail),"~n~W",[Data,[numbervars(true)]]), 	
 	concat_label_lines2(Lines,Tail).	

seq_node(Id,AttrString):-
	step_subject_predecessor(Id,Subject,_),
	findall(Line,step_data(Id,Line),Lines),
 	concat_label_lines(Lines,Label),
	format(string(AttrString),"[label=\"{~w ~w|~s}\"]",[Id,Subject,Label]).

 	
seq_edge(From,To,AttrString):- % message edge
 	step_trigger(To,Message),
 	message(Message,From,Data),
 	%format(string(AttrString),"[label=\"~w\", color=blue, constraint=false]",[Data]).   
 	format(string(AttrString),"[label=\"~w\", color=blue]",[Data]).
seq_edge(From,To,AttrString):- % sequence edge
     step_subject_predecessor(To,_,From),
     From \== [],
     format(string(AttrString),"[color=black]",[]).
 
 target_node(Id,AttrString):-
 	setof(
 		Id0,
 		A^B^(	target_depends(A,B),
 			(	Id0=A
 			;	Id0=B
 			)
 		),
 		Ids
 	),
 	member(Id,Ids),
 	current_target_state(Id,state(Activity,Status)),
 	(	Status==obsolete,Activity==idle
 	->	Color=gray
 	;	Status==consistent
 	->	Color=green
 	;	Status==obsolete
 	->	Color=red
 	;	Color=yellow
 	),
 	format(
 		string(AttrString),
 		"[label=\"{~w | ~w ~w}\", color=~w]",
 		[Id,Activity,Status,Color]
 	).

target_edge(From,To,AttrString):- % dep edge 			
 	target_depends(From,To),
 	\+ edge_label(From,To,_,_),
 	format(string(AttrString),"[style=dotted]",[]).
target_edge(From,To,AttrString):- % other edge 			
 	edge_label(From,To,Client,Label),
 	format(string(AttrString),"[style=solid,label=\"~w:~w\"]",[Client,Label]).

 	 		
dbg_write_dot:-	
	setup_and_call_cleanup(
		tell('targets.dot'),
		dbg_write_target_graph,
		told
	),
	setup_and_call_cleanup(
		tell('sequence.dot'),
		dbg_write_seq_graph,
		told
	).	 		
dbg_write_seq_graph:-
	format("digraph G {~nnode [style=filled,shape = \"record\"]~n",[]),
    forall(
    	seq_node(Id,AttrString),
    	format("\"~w\" ~w~n",[Id,AttrString])    	
    ),    
    forall(
    	seq_edge(From,To,AttrString),
    	format("\"~w\" -> \"~w\" ~s~n",[From,To,AttrString])    	
    ),
	format("}~n",[]).
	
dbg_write_target_graph:-
	format("digraph G {~nnode [style=filled,shape = \"record\"]~n",[]),
    forall(
    	target_node(Id,AttrString),
    	format("\"~w\" ~w~n",[Id,AttrString])    	
    ),    
    forall(
    	target_edge(From,To,AttrString),
    	format("\"~w\" -> \"~w\" ~s~n",[From,To,AttrString])    	
    ),
	format("}~n",[]).	