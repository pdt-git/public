:- module(builder__test,[]).
:- use_module(builder__arbiter).
:- use_module(builder__messages).
:- use_module(library('util/concurrent_tests')).

depends(a,b).
depends(a,c).
depends(c,d).
depends(c,e).
depends(e,b).
depends(e,b).
depends(b,c).

client(c1).
client(c2).
client(c3).
setup(_):-
	stop_arbiter.
teardown(_).	

/*

   
*/
sequence(
	my_test,
	builder__test,
	meta(
		client(C),
		(#),
		meta(
			depends(T,_),
			(?),
			sequence(simple_request(C,[],T))
		)
	)
).

sequence(
	my_test2,
	builder__test,
	(	honk(1,a) ?	honk(1,b) ?	honk(1,n)
	#	honk(2,a) ?	honk(2,b) ?	honk(2,n)
	#	honk(3,a) ?	honk(3,b) ?	honk(3,n)
	)
).


/*
a sequence realizing the behaviour exhibited by a client
requesting a target. 
*/
sequence(
	simple_request(C,From,T),
	builder__test,	
	(	send_message_client_target(C,T,req(C,From))
	~>	next_client_message(C,Msg) 
	~> 	(	Msg = build(T2)
		~>  meta(
				depends(C,T3), 
				(~>), 
				sequence(simple_request(C,T2,T3))
			)
		~>	(	send_message(C,T2,success)
			?	send_message(C,T2,error(test_error))
			)
		~>	sequence(simple_request(C,T))
		? 	Msg = grant(T)
		~> 	send_message_client_target(C,T,rel(C,From))
		? 	Msg = error(_)
		)
	)	
).
