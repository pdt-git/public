:- module(pdt_annotator_cache,[
	pdt_cache_time/2,
	pdt_clear_cache/1,
	pdt_read_cache/1,
	pdt_write_cache/1,
	pdt_read_cache_index/0,
	pdt_write_cache_index/0,
	pdt_cache_dir/1
]).
:- use_module(library('/org/cs3/pdt/util/pdt_preferences')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_cs')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/util/pdt_util')).
:- use_module(library('pif_observe')).
:- pdt_add_preference(
	cache_dir,
	'Cache Directory', 
	'The annotator will store its cached metadata here.',
	'/tmp/pdt_annotation_cache'
).
	
:-dynamic cache_index/4. %cache_file(+Hash,-SourceFile,-TimeStamp,-SerialNum) 
cache_file(FileSpec,CacheFile):-
	pdt_file_spec(FileSpec,File),
	hash_term(File,Hk),
	cache_file_serial(Hk,File,Serial),
	cache_dir(CacheDir),
	concat_atom([CacheDir,'/',Hk,'-',Serial],CacheFile).
	
	
pdt_cache_dir(CacheDir):-	
	cache_dir(CacheDir).
	
cache_dir(CacheDir):-	
	pdt_preference_value(cache_dir,Tmp),	
	(	atom_concat(Stripped,'/',Tmp)
	->	CacheDir = Stripped
	;	CacheDir = Tmp
	),
	ensure_dir_exists(CacheDir).
	
ensure_dir_exists(CacheDir):-
    exists_directory(CacheDir),
    !.
ensure_dir_exists(CacheDir):-
    file_directory_name(CacheDir,Parent),
	ensure_dir_exists(Parent),
	make_directory(CacheDir).
    
%the file is already in the index.
cache_file_serial(Hk,File,Serial):- 
    cache_index(Hk,File,_,Serial),
    !.
%the file is not in the index, but the hash key may have been used already.
% --> find the first unused serial number
cache_file_serial(Hk,_File,Serial):-
    pdt_unique(cache_file_serial,Flag),
    flag(Flag,_,0),
    repeat,
	    flag(Flag,Serial,Serial+1),
	    \+ cache_index(Hk,_,_,Serial),
	!.
	
pdt_write_file_annotation(Stream,FileSpec):-
    write_file_header(Stream,FileSpec),
    write_file_records(Stream,FileSpec).

write_file_header(Stream,FileSpec):-
    pdt_file_annotation(FileSpec,Annotation),
	write_canonical(Stream,file_annotation(FileSpec,Annotation)),
	write(Stream,'.'),
	nl(Stream).
	
write_file_records(Stream,FileSpec):-
    forall(
    	(	pdt_file_record_key(Kind,FileSpec,Key),
    		pdt_file_record(Key,Record),
    		\+Record==end_of_file
    	),
    	write_file_record(Stream,Kind,Record)
    ).
    

write_file_record(Stream,Kind,Record):-
	write_canonical(Stream,file_record(Kind,Record)),
	write(Stream,'.'),
	nl(Stream).


pdt_read_file_annotation(Stream):-
    read_file_header(Stream,File,Annotation),
    pdt_annotator:assert(file_annotation(File,Annotation)),
    read_file_records(Stream,File).



read_file_header(Stream,File,Annotation):-
    read_term(Stream,file_annotation(File,Annotation),[double_quotes(string)]).
    
read_file_records(Stream,File):-
    repeat,
		read_term(Stream,Term,[double_quotes(string)]),
		add_record(File,Term),
		Term==end_of_file,
	!.
add_record(File,end_of_file):-
    !,
    pdt_file_record_key(term,File,Key),
    recordz(Key,end_of_file).
add_record(File,file_record(Kind,Record)):-
    pdt_file_record_key(Kind,File,Key),
    recordz(Key,Record).
    
cache_test:-
	findall(File,pdt_file_annotation(File,_),Files),
	forall(member(File,Files),pdt_write_cache(File)),
	forall(member(File,Files),pdt_forget_annotation(File)),
	forall(member(File,Files),pdt_read_cache(File)).	

pdt_write_cache(FileSpec):-
    pdt_file_spec(FileSpec,File),
	new_index_entry(File),
	cache_file(File,CacheFile),
	open(CacheFile,write,Stream),
	call_cleanup(pdt_write_file_annotation(Stream,File),close(Stream)),
	update_index_entry(File).

pdt_read_cache(FileSpec):-
    pdt_file_spec(FileSpec,File),
    pdt_cache_time(File,Time),
    Time > 0,
    cache_file(File,CacheFile),
    open(CacheFile,read,Stream),
    call_cleanup(pdt_read_file_annotation(Stream),close(Stream)),
    pif_notify(file_annotation(File),update).
	
pdt_clear_cache(FileSpec):-
    pdt_file_spec(FileSpec,File),
	cache_file(File,CacheFile),
	(	exists_file(CacheFile)
	->	delete_file(CacheFile)
	;	true
	),
	remove_index_entry(File).
	
pdt_cache_time(FileSpec,Time):-
    pdt_file_spec(FileSpec,File),    
    hash_term(File,Hk),
    cache_index(Hk,File,Time,_).
		

	

% new_index_entry(+File,-Hk,-Time,-Serial)
% creates a new index entry.
% Unifies Hk, Time, and Serial with respective values.
% If an entry exists, no new entry is created.
% If no entry exists, time is set to -1.
new_index_entry(File):-
    new_index_entry(File,_,_,_).
new_index_entry(File,Hk,Time,Serial):-
    hash_term(File,Hk),
	cache_index(Hk,File,Time,Serial),
	!.
new_index_entry(File,Hk,-1,Serial):-
    hash_term(File,Hk),
    cache_file_serial(Hk,File,Serial),
	assert(cache_index(Hk,File,-1,Serial)).
    	

% assumes cache file exists    
% assznes index entry exists.
% updates index timestamp.

update_index_entry(File):-
    hash_term(File,Hk),
    cache_index(Hk,File,_,Serial),
    cache_file(File,CacheFile),
    time_file(CacheFile,Time),
    retractall(cache_index(Hk,File,_,_)),
	assert(cache_index(Hk,File,Time,Serial)).
	
	
remove_index_entry(File):-
    hash_term(File,Hk),    
    retractall(cache_index(Hk,File,_,_)).

pdt_write_cache_index:-
    cache_dir(Dir),
    atom_concat(Dir,'/cache_index',File),
    open(File,write,Stream),
    call_cleanup(
    	(    forall(
    			cache_index(Hk,CacheFile,Time,Serial),
		    	portray_clause(Stream,cache_index(Hk,CacheFile,Time,Serial))
		      )
		 ), 
		 close(Stream)
	).
pdt_read_cache_index:-
    cache_dir(Dir),
    atom_concat(Dir,'/cache_index',File),
    exists_file(File),
    retractall(cache_index(_Hk,_File,_Time,_Serial)),    
	consult(pdt_annotator_cache:File).
