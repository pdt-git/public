test(Data,Pos):-
    atom_to_memory_file(Data,MemFile),
    open_memory_file(MemFile,read,Input),
    read_term(Input,_,[subterm_positions(Pos),comments(_)]),
    close(Input),
    free_memory_file(MemFile).



