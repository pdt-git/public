:- use_module(library('/org/cs3/pdt/model/pdt_index')).

put_random_num:-
    pdt_index_load(test_ix, Ix),
    A is random(10^40),
    pdt_index_put(Ix,A,Ix,Ix2),
    pdt_index_store(test_ix,Ix2).


run_index_test:-
    thread_create(profile(test_index),_,[alias(index_test_runner)]).
    
abort_index_test:-
	thread_send_message(index_test_runner,abort).    

test_index:-
    flag(ix_test_counter,_,0),
	repeat,
		flag(ix_test_counter,C,C+1),
		catch(put_random_num,E,writeln(C:E)),
		(nonvar(E);thread_peek_message(abort)),
		!.
		    
    