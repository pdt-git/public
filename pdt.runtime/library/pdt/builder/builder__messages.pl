:- module(builder__messages,
	[	send_message_client_target/2,
		send_message_target_client/3,
		send_message_target_target/3,
		next_target_message/3,
		next_client_message/2
	]
).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- pdt_define_context(msg(sn,sender,receiver,data)).

% Message queue for messages exchanged between targets. 
% We want them to be processed with priority, so we create a separate
% queue, using a dynamic, thread_local predicate. Might also work with 
% with SWI's message queues. 
:- dynamic '$fast_lane'/1.
:- thread_local '$fast_lane'/1.

reserve_sn(SN):-
    flag('$pdt_builder__messages_sn',SN,SN+1).

gen_message(Sender,Receiver,Data,Msg):-
	reserve_sn(SN),
	msg_new(Msg),
	msg_get(Msg,
		[	sn=SN,sender=Sender,
			receiver=Receiver,
			data=Data
		]
	).

send_message_client_target(Target,Data):-
	thread_self(Me),
	gen_message(Me,Target,Data,Msg),
	thread_send_message(arbiter,Msg).
	
send_message_target_client(Target,Client,Data):-
	gen_message(Target,Client,Data,Msg),
	thread_send_message(Target,Msg).
	
send_message_target_target(Sender,Receiver,Data):-
	gen_message(Sender,Receiver,Data,Msg),
	assert('$fast_lane'(Msg)).


next_target_message(Target,Sender,Data):-	
	msg_new(Msg),
	msg_receiver(Msg,Target),
	msg_sender(Msg,Sender),
	msg_data(Data),
	(	retract('$fast_lane'(Msg))
	->	true
	;	thread_get_message(Msg)
	).
	
next_client_message(Sender,Data):-	
	msg_new(Msg),	
	msg_sender(Msg,Sender),
	msg_data(Data),
	thread_get_message(Msg).
