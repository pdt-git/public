:- use_module(pdt_match).
:- use_module(library('org/cs3/pdt/util/pdt_util_io')).
:- use_module(library('org/cs3/pdt/util/pdt_util_term_position')).
:- ensure_loaded(library('org/cs3/pdt/compatibility/compatiblitySWI.pl')).



try_file(File,Pattern):-
    new_memory_file(MemFile),
    copy_file_to_memfile(File,MemFile),
    memory_file_to_atom(MemFile,Text),
    open_memory_file(MemFile,read,Stream),    
    my_call_cleanup(
    	try_stream(Text,Stream,Pattern),
	    (	close(Stream),
	    	free_memory_file(MemFile)
	    )
	).
	
try_stream(Text,Stream,Pattern):-
    repeat,
		read_term(Stream,Term,[
			subterm_positions(Positions),
			syntax_errors(fail)		
		]),
		process_term(Text,Term,Pattern,Positions),
	at_end_of_stream(Stream).
	
	
process_term(Text,Term,Pattern,Positions):-
    forall(
    	pdt_match(Term,Pattern,match(Path,_)),
    	show_that_it_works(Text,Term,Path,Positions)
    ).
	
	
show_that_it_works(Text,_,[],Positions):-
	top_position(Positions,From,To),
	Len is To-From,
	sub_atom(Text,From,Len,_,SubText),
	writeln(match(From,To,SubText)).
show_that_it_works(Text,Term,[H|T],Positions):-
	sub_position(Positions,H,SubPositions),
	arg(H,Term,SubTerm),	
    show_that_it_works(Text,SubTerm,T,SubPositions).
    
 
