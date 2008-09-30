:- module(builder__messages,
	[	send_message_client_target/2,
		send_message_client_target/3,
		send_block_client_target/1,
		send_block_client_target/2,
		send_message_target_client/3,
		send_message_target_target/3,
		next_target_message/3,
		next_client_message/2,
		next_client_message/3,
		peek_client_message/2,
		peek_client_message/3
	]
).

%hmm... cyclic dependency. :-(
:- use_module(builder__arbiter).
:- use_module(builder__debug).
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

gen_block([],_,[]).
gen_block([Receiver-Data|MoreData],Sender,[Msg|MoreMsg]):-
	gen_message(Sender,Receiver,Data,Msg),
	msg_sn(Msg,SN),
	dbg_step_send(Sender,SN,Data),
	gen_block(MoreData,Sender,MoreMsg).

send_message_client_target(Target,Data):-
	thread_self(Sender),
	send_message_client_target(Sender,Target,Data).	
send_message_client_target(Sender,Target,Data):-	
	gen_message(Sender,Target,Data,Msg),
	ensure_arbiter_is_running,	
	thread_send_message(build_arbiter,Msg),
	msg_sn(Msg,SN),
	dbg_step_send(Sender,SN,Data),
	!.
send_message_client_target(Sender,Target,Data):-
	throw(failed(send_message_client_target(Sender,Target,Data))).

send_block_client_target(Data):-
	thread_self(Sender),
	send_block_client_target(Data,Sender).
	
send_block_client_target([],_):-!.
send_block_client_target(Data,Sender):-	
	gen_block(Data,Sender,Block),
	ensure_arbiter_is_running,	
	thread_send_message(build_arbiter,Block).

	
send_message_target_client(Target,Client,Data):-
	gen_message(Target,Client,Data,Msg),
	thread_send_message(Client,Msg),
	msg_sn(Msg,SN),
	dbg_step_send(Target,SN,Data).
	
send_message_target_target(Sender,Receiver,Data):-
	gen_message(Sender,Receiver,Data,Msg),
	assert('$fast_lane'(Msg)),
	msg_sn(Msg,SN),
	dbg_step_send(Sender,SN,Data).


next_target_message(Target,Sender,Data):-
		
	% check for message blocks
	% if any is in the queue, put the messages it contains 
	% on the fast lane.
	Block = [_|_],
	(	thread_peek_message(Block)
	->	thread_get_message(Block),
		forall(
			member(BlockMsg,Block),
			assert('$fast_lane'(BlockMsg))
		)
	;	true
	),
	
	% Now, go on with the delivery, by creating a message template.
	msg_new(Msg),
	msg_receiver(Msg,Target),
	msg_sender(Msg,Sender),
	msg_data(Msg,Data),
	% Check for messages on the fast lane. If there are none, 
	% look on the queue.
	(	retract('$fast_lane'(Msg))
	->	true
	;	thread_get_message(Msg)
	),
	msg_sn(Msg,SN),
	dbg_step(Target),
	dbg_step_receive(Target,SN).
	
next_client_message(Sender,Data):-		
	thread_self(Client),
	next_client_message(Client,Sender,Data).
	

next_client_message(Client,Sender,Data):-
	dbg_step(Client),		
	msg_new(Msg),	
	msg_sender(Msg,Sender),
	msg_data(Msg,Data),
	repeat,
		ensure_arbiter_is_running,
		catch(
			call_with_time_limit( 1, thread_get_message(Client,Msg)),
			time_limit_exceeded,
			fail
		),
	!,
	msg_sn(Msg,SN),
	dbg_step_receive(Client,SN).
	
peek_client_message(Client,Sender,Data):-
	msg_new(Msg),	
	msg_sender(Msg,Sender),
	msg_data(Msg,Data),
	thread_peek_message(Client,Msg).
	
peek_client_message(Sender,Data):-
	thread_self(Client),
	peek_client_message(Client,Sender,Data).