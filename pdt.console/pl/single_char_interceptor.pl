:- module(single_char_interceptor,[sci_install/0]).

:- prolog_load_context(directory,A), user:assert(file_search_path(foreign,A)).

:- dynamic sci_stream_args/2.

sci_setting(escape_char,'*').
sci_setting(char(cooked_tty),'c').
sci_setting(char(raw_tty),'s').
sci_setting(char(no_tty),'n').
sci_setting(cio_base_name,custom_io).

cio_read(_,sci(read,In,Out),Size,TTYMode,Chars):-
%    writeln(in_read(TTYMode)),
    sci_setting(escape_char,Esc),
    sci_setting(char(TTYMode),TTYChar),
    put_char(Out,Esc),
    put_char(Out,TTYChar),
    flush_output(Out),
    do_read(In,TTYMode,Size,Chars).
%writeln(out_read(Chars)).
	
do_read(In,cooked_tty,Size,Chars):-
	cio_read_n_chars(In,Size,Chars).
do_read(In,no_tty,Size,Chars):-
	cio_read_n_chars(In,Size,Chars).
do_read(In,raw_tty,_,[C]):-
	cio_read_n_chars(In,2,[C|_]).	

%cio_read(_,sci(read,In,_),Size,_,Chars):-
%    writeln(read),
%	cio_read_n_chars(In,Size,Chars).


%cio_read(A,B,C,D,E):-
%    writeln(cio_read(A,B,C,D,E)).

cio_write(_,sci(write,_,Out),Chars,[]):-
%    write(Out,write),nl(Out),
	sci_setting(escape_char,C),
	escape(C,Chars,EscapedChars),
	put_chars(Out,EscapedChars).



cio_close(_,sci(read,In,_)):-
	close(In).
cio_close(_,sci(write,_,Out)):-
	close(Out).


put_chars(_,[]).
put_chars(Out,[H|T]):-
    put_char(Out,H),
    put_chars(Out,T).
 

escape(_,[],[]).
escape(A,In,Out):-
    (	memberchk(A,In)
    ->	append(Clean,[A|Tail],In),
    	escape(A,Tail,TailOut),
    	append(Clean,[A,A|TailOut],Out)	
    ;	Out=In
    ).
    



sci_load:-

    lib_name(LibName),
    writeln(trying(LibName)),
    (	current_foreign_library(LibName,_)
    ->	true
    ;	load_foreign_library(foreign(LibName))
    ).

lib_name(LibName):-
    current_prolog_flag(shared_object_extension,Ext),
    current_prolog_flag(arch,Arch),
    sci_setting(cio_base_name,Base),
    concat_atom([Base,Arch,Ext],'.',LibName),
    expand_file_search_path(foreign(LibName),A),
    exists_file(A),
    !.
       
lib_name(LibName):- %fallback to "default" arch
    current_prolog_flag(shared_object_extension,Ext),
    sci_setting(cio_base_name,Base),
    concat_atom([Base,Ext],'.',LibName).
       
  
  
sci_install:-
    sci_load,
    current_output(OrigOut),
    current_input(OrigIn),
    set_stream(current_input,tty(true)),
    set_stream(current_output,tty(true)),
    (	sci_stream_args(OrigIn,_)
    ->	throw(sci(allready_wrapped,input,OrigIn))
    ;    cio_create(single_char_interceptor,sci(read,OrigIn,OrigOut),read,In),
    		assert(sci_stream_args(In,sci(read,OrigIn,OrigOut)))
    	),
    	(	sci_stream_args(OrigOut,_)
    ->	throw(sci(allready_wrapped,output,OrigOut))
    ;    cio_create(single_char_interceptor,sci(write,OrigIn,OrigOut),write,Out),
    		assert(sci_stream_args(Out,sci(write,OrigIn,OrigOut)))
    	),
	set_prolog_IO(In,Out,Out),
    set_stream(In,tty(true)),
    set_stream(Out,tty(true)).    

        
