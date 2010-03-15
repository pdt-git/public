test_size(Bytes, Size):-
	new_memory_file(MF),
	open_memory_file(MF,write,MFOut,[encoding(octet)]),
	set_stream(MFOut,encoding(octet)),
	put_byte(MFOut,0xC0),
	put_byte(MFOut,0x80),
	close(MFOut),
	size_memory_file(MF,Size),	
	open_memory_file(MF,read,MFIn,[encoding(octet)]),
	set_stream(MFIn,encoding(octet)),
	read_bytes(MFIn,Bytes),
	close(MFIn),
	free_memory_file(MF).


read_bytes(Stream,[]):-
    at_end_of_stream(Stream),
    !.
read_bytes(Stream,[B|Bs]):-
    get_byte(Stream,B),
    read_bytes(Stream,Bs).
