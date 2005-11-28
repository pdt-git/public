:- use_module(pdt_match).
:- use_module(library('org/cs3/pdt/util/pdt_util_io')).
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
	sub_positions(Positions,H,SubPositions),
	arg(H,Term,SubTerm),	
    show_that_it_works(Text,SubTerm,T,SubPositions).
    
 
top_position(From-To,From,To).
top_position(string_position(From,To),From,To).
top_position(brace_term_position(From,To,_),From,To).
top_position(list_position(From,To,_,_),From,To).	
top_position(term_position(From,To,_,_,_),From,To).	

sub_positions(brace_term_position(_,_,T),1,T).
sub_positions(list_position(_,_,[H|_],_),1,H).		
sub_positions(list_position(_,_,[_|[First|T]],none),2,list_position(From,To,[First|T],none)):-
	last([First|T],Last),
	top_position(First,From,_),
	top_position(Last,_,To).	
sub_positions(list_position(_,_,Elms,Tail),2,list_position(From,To,[First|T],none)):-
    \+Tail=none,
    append(Elms,[Tail],[_|[First|T]]),
    last([First|T],Last),
	top_position(First,From,_),
	top_position(Last,_,To).	    
sub_positions(term_position(_,_,_,_,ArgPositions),N,SubPos):-
	nth1(N,ArgPositions,SubPos).