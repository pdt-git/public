:- module(incidents,[]).
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
:- pdt_define_context(incident(sn,subject,object,type,data,thread)).

reserve_sn(SN):-
    flag('$pdt_incidents_sn',SN,SN+1).

log_send(Subject,Msg,Object,SN):-
	thread_self(Thread),
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Subject),
	incident_object(I,Object),
	incident_type(I,send),
	incident_data(I,Msg),
	incident_thread(I,Thread),
	asserta(I).

log_receive(SendSN):-
    thread_self(Thread),
    incident_new(SendI),
    incident_sn(SendI,SendSN),
    (	SendI
    ->	true
    ;	throw(no_matching_send_incident(SendSN))
    ),
    incident_object(SendI,Object),
    incident_subject(SendI,Subject),
    incident_data(SendI,Msg),
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Subject),
	incident_object(I,Object),
	incident_type(I,receive),
	incident_data(I,Msg),
	incident_thread(I,Thread),
	assert(I).

log_external_event(Subject,Event).
	thread_self(Thread),    
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Subject),
	incident_object(I,Subject),
	incident_type(I,external_event),
	incident_data(I,Event),
	incident_thread(I,Thread),
	assert(I).
log_external_action(Subject,Action).
	thread_self(Thread),    
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Subject),
	incident_object(I,Subject),
	incident_type(I,external_action),
	incident_data(I,Event),
	incident_thread(I,Thread),
	assert(I).

log_transition(Subject,Event,NewState):-
    	thread_self(Thread),    
	reserve_sn(SN),
	incident_new(I),
	incident_sn(I,SN),
	incident_subject(I,Subject),
	incident_object(I,Subject),
	incident_type(I,transition),
	incident_data(I,transition(Event,NewState)),
	incident_thread(I,Thread),
	assert(I).
    
 