:- module(pdt_util_io,[
	copy_file_to_memfile/2,
	copy_memfile_to_file/2,
	pretty_print/3
]).

:- use_module(library('org/cs3/pdt/util/pdt_util')).


/**
copy_file_to_memfile(+File, +MemFile)

Copies the contents of File to the memory file MemFile.

File - a file spec. (path or aliased path)
MemFile - a handle to a Memfile. 

*/
copy_file_to_memfile(File,MemFile):-
    pdt_file_spec(File,Abs),
    open_memory_file(MemFile,write,MemStream),    
    open(Abs,read,Stream),
    copy_stream_data(Stream,MemStream),
    close(Stream),
    close(MemStream).
    
    
/**
copy_memfile_to_file(+MemFile, +File)

Copies the contents of File to the memory file MemFile.

File - a file spec. (path or aliased path)
MemFile - a handle to a Memfile. 

*/
copy_memfile_to_file(MemFile,File):-
    pdt_file_spec(File,Abs),
    open_memory_file(MemFile,read,MemStream),    
    open(Abs,write,Stream),
    copy_stream_data(MemStream,Stream),
    close(Stream),
    close(MemStream).    
    
pretty_print(Stream,Indent,aterm(A,T)):-
    compound(T),
    !,
    T=..[Functor|Args],
    format(Stream, "~waterm(~w,'~w'(~n",[Indent,A,Functor]),
    atom_concat(Indent,'   ',IIndent),
    pretty_print_args(Stream,IIndent,Args),
    format(Stream,"~w))",[Indent]).        
pretty_print(Stream,Indent,aterm(A,T)):-
    \+ compound(T),
    !,
    format(Stream, "~waterm(~w,~w)",[Indent,A,T]).

pretty_print(Stream,Indent,T):-
    compound(T),
    !,
    T=..[Functor|Args],
    format(Stream, "~w'~w'(~n",[Indent,Functor]),
    atom_concat(Indent,'   ',IIndent),
    pretty_print_args(Stream,IIndent,Args),
    format(Stream,"~w)",[Indent]).        
pretty_print(Stream,Indent,T):-
    \+ compound(T),
    !,
    format(Stream, "~w~w",[Indent,T]).

pretty_print_args(_,_,[]):-!.
pretty_print_args(Stream,Indent,[H|[]]):-
    !,
	pretty_print(Stream,Indent,H),
	nl(Stream).
pretty_print_args(Stream,Indent,[H|T]):-
    !,
	pretty_print(Stream,Indent,H),
	write(Stream,','),
	nl(Stream),
	pretty_print_args(Stream,Indent,T).