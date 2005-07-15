:- module(decorator_test,[]).
:- load_foreign_library('stream_decorator.so').

stream_decorator_read_hook(Stream,_,_,raw_tty,[C]):-
	orig_write_all(current_output, ['\\','s']),
	orig_read(Stream,2,[C,'\n']).
	
stream_decorator_write_hook(Stream,_,['\\'|Tail],Tail):-
	orig_write_all(Stream,['\\','\\']).	
	

	

