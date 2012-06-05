
write_escaped(Term,Mode):-
    write_term_to_memfile(Term,Mode,Memfile),
    open_memory_file(Memfile,read,Stream),
    escape_stream(Stream,current_output),
    close(Stream),
    free_memory_file(Memfile).

write_term_to_memfile(Term,canonical,Memfile):-
	new_memory_file(Memfile),
	open_memory_file(Memfile,write,Stream),
	write_canonical(Stream,Term),
	close(Stream).
write_term_to_memfile(Term,_,Memfile):-
	new_memory_file(Memfile),
	open_memory_file(Memfile,write,Stream),
	write(Stream,Term),
	close(Stream).

escape_stream(In,Out):-
    repeat,	    
    (	at_end_of_stream(In)
    ->	!,true
    ;   get_char(In,Char),    	
	    write_escaped_char(Out,Char),
	    fail
	).
	

write_escaped_char(Out,'<'):-
	write(Out,'&lt;'),
	!.
write_escaped_char(Out,'>'):-
	write(Out,'&gt;'),
	!.
write_escaped_char(Out,'{'):-
	write(Out,'&cbo;'),
	!.
write_escaped_char(Out,'}'):-
	write(Out,'&cbc;'),
	!.
write_escaped_char(Out,'&'):-
	write(Out,'&amp;'),
	!.
write_escaped_char(Out,'"'):-
	write(Out,'&quot;'),
	!.
write_escaped_char(Out,'\''):-
	write(Out,'&apos;'),
	!.
write_escaped_char(Out,C):-
	put_char(Out,C).	
	    