:- module(builder__debug,
	[	dbg_step/1,
		dbg_step_describe/2,
		dbg_step_receive/2,
		dbg_step_send/3
	]
).

:- dynamic step_subject_predecessor/3, step_data/2,message/3,step_trigger/2.

dbg_disabled:-fail.

dbg_step(_):-
	dbg_disabled,
	!.
dbg_step(Subject):-
	flag('$builder__debug_step_counter',Step,Step+1),
	(	current_step(Subject,Prev)
	->	true
	;	Prev=[]
	),
	asserta(step_subject(Step,Subject,Prev)).

current_step(Subject,Step):-
	once(step_subject_predecessor(Step,Subject,_)).

have_current_step(Subject,Step):-
	(	current_step(Subject,Step)
	->	true
	;	dbg_step(Subject),
		current_step(Subject,Step)
	).

dbg_step_describe(_,_):-
	dbg_disabled,
	!.
dbg_step_describe(Subject,Description):-
	have_current_step(Subject,Step),
	assert(step_data(Step,Description)).

dbg_step_receive(_,_):-
	dbg_disabled,
	!.	
dbg_step_receive(Subject,Message):-
	have_current_step(Subject,Step),
	(	step_trigger(Step,_)
	->	throw(error(multiple_triggers,_))
	;	assert(step_trigger(Step,Message))
	).
	
dbg_step_send(_,_,_):-
	dbg_disabled,
	!.		
dbg_step_send(Subject,Message,Data):-
	have_current_step(Subject,Step),
	assert(message(Message,Step,Data)).

node(Id,AttrString):-
	step_subject_predecessor(Id,Subject,_),
	findall(Line,step_data(Id,Line),Lines),
 	concat_label_lines(Lines,Label),
	format(string(AttrString),"[label=\"{~w ~w|~s}\"]",[Id,Subject,Label]).
 
 	
 concat_label_lines([],[]).
 concat_label_lines([Data|Lines],Label):-
 	format(string(Line),"~W",[Data,[numbervars(true)]]),
 	append(Line,Tail,Label),
 	concat_label_lines2(Lines,Tail).	
 concat_label_lines2([Data|Lines],Label):-
 	format(string(Line),"~W",[Data,[numbervars(true)]]),
 	append(['\n'|Line],Tail,Label),
 	concat_label_lines2(Lines,Tail).	
 	
 edge(From,To,AttrString):- % message edge
 	step_trigger(To,Message),
 	message(Message,From,Data),
 	format(string(AttrString),"[label=\"~w\", color=blue]",[Data]).   
 edge(From,To,AttrString):- % sequence edge
     step_subject_predecessor(To,_,From),
     From \== [],
     format(string(AttrString),"[color=black]",[]).
 
 		
dbg_write_dot:-
	format("digraph G {~nnode [style=filled,shape = \"record\"]~n",[]),
    forall(
    	node(Id,AttrString),
    	format("\"~w\" ~w~n",[Id,AttrString])    	
    ),    
    forall(
    	edge(From,To,AttrString),
    	format("\"~w\" -> \"~w\" ~s~n",[From,To,AttrString])    	
    ),
	format("}~n",[]).	