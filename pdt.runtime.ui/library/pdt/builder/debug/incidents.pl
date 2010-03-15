:- module(incidents,[
	log_mark/2,
	log_action/2,
	log_event/2,
	log_receive/1,
	log_send/4,
	log_transition/2,
	log_push_state/2,
	log_pop_state/1,
	write_incidents/2,
	write_incidents/4
]).
/*
The intention of this module is to facilate off-line debugging of highly concurrent systems, like our build-system.

We use a model consisting of state machines interacting through asynchronous message passing.

In the code that is to be debugged, transitions, events and actions have to be "marked" by 
calling the public API of this module.

Incidents are actions, events and transitions.
All incidents have a unique seriel number. Seriel numbers are strictly monotone, i.e. if 
an incident A occurs after an incident B in time, then sn(A) > sn(B).  

All actions are expressed as sending a message.
All events are expressed as receiving a message.
Any transition is caused by exactly one event.
Any action is caused by exactly one transition.

All incidents are recorded in the database and can be querried through this module's public api.
The idea is to visualize this information in a graph that is somewhat dual to the state machine view. 

*/

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- pdt_define_context(incident(sn,subject,object,cause,type,data,thread)).
:- dynamic incident/7, '$stack'/2.

incident_query(AttrList):-
	incident_new(I),
	incident_get(I,AttrList),
	I.

reserve_sn(SN):-
    flag('$pdt_incidents_sn',SN,SN+1).

log_send(Subject,Msg,Object,SN):-
    incident_new(Cause),
    incident_type(Cause,transition),
    incident_subject(Cause,Subject),
    once(Cause),
    incident_sn(Cause,CauseSN),
	thread_self(Thread),
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Subject),
	incident_object(I,Object),
	incident_type(I,action),
	incident_data(I,Msg),
	incident_thread(I,Thread),
	incident_cause(I,CauseSN),
	asserta(I).

log_receive(SendSN):-
    thread_self(Thread),
    incident_new(SendI),
    incident_sn(SendI,SendSN),
    (	SendI
    ->	true
    ;	throw(no_matching_send_incident(SendSN))
    ),
    incident_object(SendI,Receiver),
    incident_subject(SendI,Sender),
    incident_data(SendI,Msg),
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Receiver),
	incident_object(I,Sender),
	incident_type(I,event),
	incident_data(I,Msg),
	incident_thread(I,Thread),
	incident_cause(I,SendSN),
	assert(I).

log_event(Subject,Event):-
	thread_self(Thread),    
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Subject),
	incident_object(I,Subject),
	incident_type(I,event),
	incident_data(I,Event),
	incident_thread(I,Thread),
	incident_cause(I,[]),
	assert(I).
	
log_mark(Subject,Action):-
	incident_new(Cause),
    incident_type(Cause,transition),
    incident_subject(Cause,Subject),
    once(Cause),
    incident_sn(Cause,CauseSN),
	thread_self(Thread),    
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Subject),
	incident_object(I,Subject),
	incident_type(I,mark),
	incident_data(I,Action),
	incident_thread(I,Thread),
	incident_cause(I,CauseSN),
	assert(I),
	debug(incidents,"mark: ~w, ~w~n",[SN,Action]).

log_action(Subject,Action):-
	incident_new(Cause),
    incident_type(Cause,transition),
    incident_subject(Cause,Subject),
    once(Cause),
    incident_sn(Cause,CauseSN),
	thread_self(Thread),    
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Subject),
	incident_object(I,Subject),
	incident_type(I,action),
	incident_data(I,Action),
	incident_thread(I,Thread),
	incident_cause(I,CauseSN),
	assert(I).


log_transition(Subject,NewState):-
	incident_new(Cause),
    incident_type(Cause,event),
    incident_subject(Cause,Subject),
    once(Cause),
    incident_sn(Cause,CauseSN),
	    
   	thread_self(Thread),    
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Subject),
	incident_object(I,Subject),
	incident_type(I,transition),
	incident_data(I,NewState),
	incident_cause(I,CauseSN),
	incident_thread(I,Thread),
	assert(I).


log_push_state(Subject,State):-
    asserta('$stack'(Subject,State)),
    log_transition(Subject,NewState).
    
log_pop_state(Subject):-
	retract('$stack'(Subject,_)),
	'$stack'(Subject,State),
	log_transition(Subject,State).    

    

node(Incidents,NodeSN,Options):-
    member(NodeSN,Incidents),
    incident_query([sn=NodeSN,subject=Subject,data=NewState]),
 	format(string(Options),"[label=\"{~w ~w|~w}\"]",[NodeSN,Subject,NewState]).
 	
edge(Incidents,FromSN,ToSN,Options):-
 	message_edge(Incidents,FromSN,ToSN,Options).
edge(Incidents,FromSN,ToSN,Options):-
 	sequence_edge(Incidents,FromSN,ToSN,Options).
     
 
 message_edge(Incidents,FromSN,ToSN,Options):-
 	member(ToSN,Incidents),
 	incident_query([sn=ToSN,type=transition,cause=EventSN]),
 	incident_query([sn=EventSN,data=Msg,cause=ActionSN]),
 	incident_query([sn=ActionSN,cause=FromSN]),
 	format(string(Options),"[label=\"~w\", color=blue]",[Msg]).
    
 sequence_edge([FromSN|Incidents],FromSN,ToSN,Options):-
     incident_query([sn=FromSN,subject=Subject]),
     once(next_incident(Subject,Incidents,ToSN)),
     format(string(Options),"[color=black]",[]).
 sequence_edge([_|Incidents],FromSN,ToSN,Options):-
 	sequence_edge(Incidents,FromSN,ToSN,Options).
 
 next_incident(Subject,Incidents,ToSN):-
 	member(ToSN,Incidents),
 	incident_query([sn=ToSN,subject=Subject]).

filter(From,To,Subjects,Incidents):-
    setof(
    	I,
    	(	incident_query([sn=I,subject=Subject]),
    		From =< I,
    		I =< To,
    		memberchk(Subject,Subjects)
    	),
    	Incidents
    ).
    		
write_incidents(From,To,Subjects,Tmp):-
	filter(From,To,Subjects,Incidents),
	write_incidents(Incidents,Tmp).
	 	
write_incidents(Incidents,Tmp):-
	tmp_file(graph,Tmp),
    tell(Tmp),call_cleanup(print_graph(Incidents),told).
 	
print_graph(Incidents):-   	 
    format("digraph G {~nnode [style=filled,shape = \"record\"]~n",[]),
    forall(
    	node(Incidents,Id,AttrString),
    	format("\"~w\" ~w~n",[Id,AttrString])    	
    ),    
    forall(
    	edge(Incidents,From,To,AttrString),
    	format("\"~w\" -> \"~w\" ~s~n",[From,To,AttrString])    	
    ),
	format("}~n",[]).