:- module(single_char_interceptor,[sd_load/0,sd_install/0, sd_uninstall/0]).

stream_decorator_read_hook(Stream,_,_,raw_tty,[C]):-
    char_code(A,42),
  orig_write_all(current_output, [A,'s']),
  orig_read(Stream,2,[C,'\n']).
%stream_decorator_read_hook(_,_,_,A,_):-
%	writeln(A),fail.
    
stream_decorator_write_hook(Stream,_,Chars,[]):-
    char_code(A,42),
	escape(A,Chars,EscapedChars),
	orig_write_all(Stream,EscapedChars).
    

orig_write_all(_,[]).
orig_write_all(Stream,Chars):-
    orig_write(Stream,Chars,Left),
    orig_write_all(Stream,Left).

escape(_,[],[]).
escape(A,In,Out):-
    (	memberchk(A,In)
    ->	append(Clean,[A|Tail],In),
    	escape(A,Tail,TailOut),
    	append(Clean,[A,A|TailOut],Out)	
    ;	Out=In
    ).
    

:- prolog_load_context(directory,A), user:assert(file_search_path(foreign,A)).

sd_load:-

    lib_name(LibName),
    
    (	current_foreign_library(LibName,_)
    ->	true
    ;	load_foreign_library(foreign(LibName))
    ).

lib_name(LibName):-
    current_prolog_flag(shared_object_extension,Ext),
    current_prolog_flag(arch,Arch),
    concat_atom([stream_decorator,Arch,Ext],'.',LibName),
    expand_file_search_path(foreign(LibName),A),
    exists_file(A),
    !.
       
lib_name(LibName):- %fallback to "default" arch
    current_prolog_flag(shared_object_extension,Ext),
    concat_atom([stream_decorator,Ext],'.',LibName).
       
        
sd_install:-
    sd_load,
    %set_prolog_flag(tty_control,true),
    set_stream(current_input,tty(true)),
    set_stream(current_output,tty(true)),
    (	is_hijacked_stream(current_input)
    ->	true
    ;	hijack_stream(current_input,single_char_interceptor,[])
    ),
    (	is_hijacked_stream(current_output)
    ->	true
    ;	hijack_stream(current_output,single_char_interceptor,[])
    ).    
    
sd_uninstall:-
	unhijack_stream(current_input),
	unhijack_stream(current_output).    
