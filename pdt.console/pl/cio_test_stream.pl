:- module(cio_test_stream,[create_test_stream/2,delete_test_stream/2]).
:- load_foreign_library(custom_io).

cio_read(S,stream_args(MF,MemStream,read),Size,no_tty,Chars):-
    recordz(cio_callbacks,cio_read(S,stream_args(MF,MemStream,read),Size,no_tty,Chars)),
	cio_read_n_chars(MemStream,Size,Chars).
cio_read(S,stream_args(MF,MemStream,read),Size,cooked_tty,Chars):-
    recordz(cio_callbacks,cio_read(S,stream_args(MF,MemStream,read),Size,cooked_tty,Chars)),    
	cio_read_n_chars(MemStream,Size,Chars).   
cio_read(S,stream_args(MF,MemStream,read),Size,raw_tty,Chars):-
    recordz(cio_callbacks,cio_read(S,stream_args(MF,MemStream,read),Size,raw_tty,Chars)),
    cio_read_n_chars(MemStream,1,Chars).


cio_write(S,stream_args(MF,MemStream,write),Chars,[]):-
    recordz(cio_callbacks,cio_write(S,stream_args(MF,MemStream,write),Chars,[])),
    concat_atom(Chars,Atom),
    write(MemStream,Atom),
    flush_output(MemStream).

cio_close(S,stream_args(MF,MemStream,Mode)):-
    recordz(cio_callbacks,cio_close(S,stream_args(MF,MemStream,Mode))),
    writeln(closing),
    close(MemStream),
    writeln(closed).
    
    

create_test_stream(Mode,Stream,stream_args(MemFile,MemStream,Mode)):-
    new_memory_file(MemFile),
    init_mem_file(MemFile,Mode),
    open_memory_file(MemFile,Mode,MemStream),
    cio_create(cio_test_stream,stream_args(MemFile,MemStream,Mode),Mode,Stream).
    

    
init_mem_file(_,write).
init_mem_file(Memfile,read):-
    open_memory_file(Memfile,write,Out),
    write(Out,taci_taci_che_gia_pia),
 %   nl(Out),
    write(Out,porge_i_baci_al_mio_labro_l_alba_mia),
  %  nl(Out),
    close(Out).

do_test_read:-
	create_test_stream(read,Stream,stream_args(MemFile,MemStream,Mode)),
	read_stream_to_codes(Stream,Codes),
	close(Stream),
	memory_file_to_codes(MemFile,Codes),
	free_memory_file(MemFile),
	all_records(List),
	forall(
		member(cio_read(TheStream,TheArgs,TheSize,TheTTY,TheChars),List),
		(
		ground(TheStream),TheStream=Stream,
		ground(TheArgs),TheArgs=	stream_args(MemFile,MemStream,Mode),
		ground(TheSize),TheSize > 0,
		ground(TheTTY),TheTTY=no_tty,
		var(TheChars)
		)
	),
	clear_records.

do_test_write:-
    create_test_stream(write,Stream,stream_args(MemFile,_,_)),
    write(Stream,selber_doof),
    nl(Stream),
    close(Stream),
    memory_file_to_codes(MemFile,"selber_doof\n"),
    free_memory_file(MemFile).
	

do_test_rawtty:-
	create_test_stream(read,In,stream_args(MemFile,MemStream,Mode)),
	create_test_stream(write,Out,_),
	current_input(OrigIn),
	current_output(OrigOut),
	set_prolog_IO(In,Out,Out),
	get_single_char(A),
	get_single_char(B),
	get_single_char(C),

	set_prolog_IO(OrigIn,OrigOut,OrigOut),
	close(In),
	close(Out),
	atom_codes('tac',[A,B,C]),
	free_memory_file(MemFile),
	all_records(List),
	forall(
		member(cio_read(TheStream,TheArgs,TheSize,TheTTY,TheChars),List),
		(
		ground(TheStream),TheStream=In,
		ground(TheArgs),TheArgs=	stream_args(MemFile,MemStream,Mode),
		ground(TheSize),TheSize > 0,
		ground(TheTTY),TheTTY=raw_tty,
		var(TheChars)
		)
	),
	clear_records.
	

do_test_mixed_read:-
	create_test_stream(read,In,stream_args(MemFile,MemStream,Mode)),
	create_test_stream(write,Out,_),
	current_input(OrigIn),
	current_output(OrigOut),
	set_prolog_IO(In,Out,Out),
	get_code(A),
	get_single_char(B),
	get_code(C),

	set_prolog_IO(OrigIn,OrigOut,OrigOut),
	close(In),
	close(Out),
	atom_codes('tac',[A,B,C]),
	free_memory_file(MemFile).
	/*all_records(List),
	forall(
		member(cio_read(TheStream,TheArgs,TheSize,TheTTY,TheChars),List),
		(
		ground(TheStream),TheStream=In,
		ground(TheArgs),TheArgs=	stream_args(MemFile,MemStream,Mode),
		ground(TheSize),TheSize > 0,
		ground(TheTTY),TheTTY=raw_tty,
		var(TheChars)
		)
	),
	clear_records.*/
	
	
%clear_records.	
clear_records:-
 	forall(recorded(cio_callbacks,_,Ref),erase(Ref)).

all_records(List):-
    findall(Record,recorded(cio_callbacks,Record),List).
 		
%:-do_test_read.
:-do_test_write.
%:-do_test_rawtty.
%:-do_test_mixed_read.
:-forall(recorded(cio_callbacks,Record),writeln(Record)).
:-halt.