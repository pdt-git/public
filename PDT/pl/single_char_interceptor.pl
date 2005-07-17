:- module(single_char_interceptor,[sd_load/0,sd_install/0, sd_uninstall/0]).

stream_decorator_read_hook(Stream,_,_,raw_tty,[C]):-
  orig_write_all(current_output, ['\\','s']),
  orig_read(Stream,2,[C,'\n']).
%stream_decorator_read_hook(_,_,_,A,_):-
%	writeln(A),fail.
    
stream_decorator_write_hook(Stream,_,Chars,Left):-
  write_escaped(Stream,Chars,Left).
    
write_escaped(_,[],[]).	
write_escaped(Stream,['\\'|Tail],Tail):-
  orig_write_all(Stream,['\\','\\']).	
write_escaped(Stream,All,Left):-
  append(Clean,['\\'|Tail],All),
  orig_write(Stream,Clean,CleanTail),
  append(CleanTail,['\\'|Tail],Left).

orig_write_all(_,[]).
orig_write_all(Stream,Chars):-
    orig_write(Stream,Chars,Left),
    orig_write_all(Stream,Left).


:- prolog_load_context(directory,A), user:assert(file_search_path(foreign,A)).

sd_load:-
    load_foreign_library(foreign('stream_decorator.so')).
        
sd_install:-
    sd_load,
    hijack_stream(current_input,single_char_interceptor,[]),
    hijack_stream(current_output,single_char_interceptor,[]).    
    
sd_uninstall:-
	unhijack_stream(current_input),
	unhijack_stream(current_output).    
