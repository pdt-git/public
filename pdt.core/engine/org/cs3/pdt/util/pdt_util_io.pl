:- module(pdt_util_io,[
	copy_file_to_memfile/2
]).


/**
copy_file_to_memfile(+File, +MemFile)

Copies the contents of File to the memory file MemFile.

File - a file spec. (path or aliased path)
MemFile - a handle to a Memfile. 

*/
copy_file_to_memfile(File,MemFile):-
    absolute_file_name(File,[extensions(['.pl','.ct','']),access(read)],Abs),
    open_memory_file(MemFile,write,MemStream),    
    open(Abs,read,Stream),
    copy_stream_data(Stream,MemStream),
    close(Stream),
    close(MemStream).