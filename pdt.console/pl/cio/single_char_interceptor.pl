/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

:- module(single_char_interceptor,[arch_lib_name/1,sci_install/0, sci_setting/2]).

:- prolog_load_context(directory,A), user:assertz(file_search_path(foreign,A)).

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
    format(' >>> Trying to load optional library: ''~a''.~n', [LibName]),
    format(' >>> Errors while loading optional libraries can safely be ignored.~n'),
    (	current_foreign_library(LibName,_)
    ->	true
    ;	load_foreign_library(foreign(LibName))
    ).

arch_lib_name(LibName):-
    current_prolog_flag(shared_object_extension,Ext),
    current_prolog_flag(arch,Arch),
    current_prolog_flag(version,A),
    (A<50700 -> Ver = 56 ; Ver = 57),
    sci_setting(cio_base_name,Base),
    concat_atom([Base,Ver,Arch,Ext],'.',LibName).


lib_name(LibName):-
    arch_lib_name(LibName),
    expand_file_search_path(foreign(LibName),A),
    exists_file(A),
    !.
       
lib_name(LibName):- %fallback to "default" arch
    current_prolog_flag(shared_object_extension,Ext),
    current_prolog_flag(version,A),
    (A<50700 -> Ver = 56 ; Ver = 57),
    sci_setting(cio_base_name,Base),
    concat_atom([Base,Ver,Ext],'.',LibName).
       
  
  
sci_install:-
    sci_load,
    current_output(OrigOut),
    current_input(OrigIn),
    set_stream(current_input,tty(true)),
    set_stream(current_output,tty(true)),
    (	sci_stream_args(OrigIn,_)
    ->	throw(sci(allready_wrapped,input,OrigIn))
    ;    cio_create(single_char_interceptor,sci(read,OrigIn,OrigOut),read,In),
    		assertz(sci_stream_args(In,sci(read,OrigIn,OrigOut)))
    	),
    	(	sci_stream_args(OrigOut,_)
    ->	throw(sci(allready_wrapped,output,OrigOut))
    ;    cio_create(single_char_interceptor,sci(write,OrigIn,OrigOut),write,Out),
    		assertz(sci_stream_args(Out,sci(write,OrigIn,OrigOut)))
    	),
	set_prolog_IO(In,Out,Out),
    set_stream(In,tty(true)),
    set_stream(Out,tty(true)).    

        


