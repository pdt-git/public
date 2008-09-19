:- module(builder__test,[builder_test/0]).
:- use_module(builder__arbiter).
:- use_module(builder__messages).
:- use_module(library('util/concurrent_tests')).

:- guitracer.

%:- tspy(builder__messages:next_client_message/3).

max_total_depth(150).
max_step_depth(50).

builder_test:-
	(	current_thread(test_driver,_)
	->	thread_join(test_driver,_)
	;	true
	),
	thread_create(
		run_concurrent_test(my_test),
		_,
		[alias(test_driver)]
	).
builder_test_count(C):-	
	concurrent_tests:count_paths(sequence(my_test),builder__test,C).
	
	
:- dynamic depends/2.
depends(a,b).
depends(a,c).
depends(c,d).
depends(c,e).
depends(e,b).
depends(b,c).

client(c1).
%client(c2).
%client(c3).
fixture_setup(Num):-
	thread_self(Me),
	send_message_client_target(meta,ping(Me)),
	wait_for(meta,pong),
	forall(
		client(C),
		message_queue_create(C)		
	),
	stop_arbiter,
	builder__arbiter:start_arbiter(Num),
	forall(
		pending_message_for_client(Num,Msg,C), 
		thread_send_message(C,Msg)		
	).

:- dynamic pending_message_for_client/3.
fixture_store(Num):-
	% collect any pending message addressed to clients
	% NOTE TO MYSELF: 
	% this would also work for messages send by clients to the arbiter.
	% When store is entered, set some flag in the message subsystem.
	% further messages are not enqueued, but stored together with the rest of the 
	% state dump. 
	% After the arbiter was asked to store its state, synchronize using ping/pong sequence.
	% then unset the flag, send the pending messages. 
		
	builder__arbiter:store_arbiter_state(Num),
	thread_self(Me),
	send_message_client_target(meta,ping(Me)),
	wait_for(meta,pong),
	forall( client(C), store_messages_for_client(C,Num)).

store_messages_for_client(C,Num):-
	repeat, 
		(	thread_peek_message(C,Msg)
		->	thread_get_message(C,Msg),
		    assert(pending_message_for_client(Num,Msg,C)),
			fail
		;	true
		),
	!.

	
fixture_teardown(_Num):-
	current_thread(build_arbiter,Status),
	(	Status == running
	-> 	true
	;	throw(builder_not_running(Status))
	),
	thread_self(Me),
	send_message_client_target(meta,ping(Me)),
	wait_for(meta,pong),
	forall(
		client(C),
		message_queue_destroy(C)	
	).

wait_for(Target,Msg):-
	repeat,
		catch(
			call_with_time_limit( 1, next_client_message(Target,Msg)),
			time_limit_exceeded,
			fail
		),
	!.
% special verison for concurrent testing.
%  We first send a ping. 
%  Now we know that the arbiter HAS to send something eventually.
%  If we receive a pong, then there hasn't been any server activity in the mean time.
%  We also know that there was no client activity in the mean time (we control all clients!)
%  So a pong means: we are locked. The system cannot step. The current path / interleaving
%  should be discarded. 
%  If we receive something else, than there was some server activity. We have to wait for the pong, though,
%  to remove it from the queue.  
my_next_client_message(Client,From,Msg):-	
	send_message_client_target(Client,meta,ping(Client)),
	next_client_message(Client,From,Msg),
	(	Msg == pong
	->	fail
	;	next_client_message(Client,meta,pong)
	).


	

sequence(
	my_testX,
	builder__test,
	(	fail
	?	true
	?	( fail		
		else throw(error(inner))
		)	
	~>	true
	else throw(error(outer))
	)
).

sequence(
	my_test,
	builder__test,
	(	sequence(simple_request(c1,[],a,[],Locks))
	%~>	check(
	%		(	memberchk(a,Locks)
	%		;	memberchk(error(test_error(a,_)),Locks)	
	%		)
	%	)
	%~>	check(
	%		forall(
	%			member(error(E),Locks),
	%			functor(E,test_error,_)
	%		)
	%	) 
	%#	sequence(simple_request(c2,[],b))
	%#	sequence(simple_request(c3,[],c))
	)
).



/*
a sequence realizing the behaviour exhibited by a client
requesting a target. 
*/
sequence(
	simple_request(C,From,T,LocksIn,LocksOut),
	builder__test,	
	(	send_message_client_target(C,T,req(From,C))
	~>	wait(	
			my_next_client_message(C,_,Msg), 
		 	(	Msg = build(T2)
			~>  meta(
					depends(T2,T3), 
					(~>), 
					(	\+ member(error(_),LIn)
					~>	sequence(simple_request(C,T2,T3,LIn,LOut))
					?	memberchk(error(_),LIn),
						LOut = LIn
					),
					share(LIn,LocksIn,LOut)
				)
			~>	(	\+ memberchk(error(_),LOut),
					send_message_client_target(C,T2,success)
				~>	sequence(simple_request(C,From,T,LOut,LocksOut))			
				%?	LocksOut=[error(test_error(T2,local))|LOut],
				%	send_message_client_target(C,T2,error(test_error(T2,nonlocal)))			
				)		
			? 	Msg = grant(T),
			 	LocksOut = [T|LocksIn]		 	
			? 	Msg = error(E),
				LocksOut = [error(E)|LocksIn]
			else throw(error(unexpected_message(Msg)))
			)
		)
	%~>	check(ground(LocksOut))		
	)	
).


/*
sequence(
	my_test,
	builder__test,
	(	meta(
			member(Elm,[1,2,3]),
			(~>),
			sequence(other_seq(In,Elm,Out)),
			share(In,[],Out)
		)
	~>	check(ground(Out))
	)
).
*/

sequence(
	other_seq(In,Elm,Out),
	builder__test,	
	(	meta(
			member(Elm2,[a,b,c]),
			(~>),
			sequence(third_seq(In2,Elm-Elm2,Out)),
			share(In2,In,Out)
		)
	)
).

sequence(
	third_seq(In,Elm,Out),
	builder__test,	
	(	Out= [Elm|In]
	)
).



sequence(
	testsequence(In,Out),
	builder__test,
	Out=f(In)
).
sequence(
	testsequence(In,Out),
	builder__test,
	Out=g(In)
).

sequence(
	pdt_301,
	builder__test,
	(  send_message_client_target(c1, a, req([], c1))
	~> next_client_message(c1, a, build(a))	
	~> send_message_client_target(c1, b, req(a, c1))
	~> next_client_message(c1, b, build(b))
	~> send_message_client_target(c1, b, error(test_error(b, nonlocal)))
	~> send_message_client_target(c1, a, error(test_error(a, nonlocal)))	
	)
).
