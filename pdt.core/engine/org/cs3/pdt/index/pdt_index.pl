%Note: This module may be used as an annotator (see org/cs3/pdt/annotate/pdt_annotator)
%this will cause each annotated file to be indexed.

:- module(pdt_index,[
	pdt_update_index/1,
	pdt_clear_index/1
]).

:- use_module(library('org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_hashtable')).


file_post_annotation_hook([File|_],_,_,Annos,[indexed(TxTime)|Annos]):-
    pdt_update_index(File),
    time_index(File,TxTime).

pdt_update_index(FileSpec):-
    pdt_file_spec(FileSpec,File),
    time_file(File,ModTime),
    time_index(File,IxTime),
    update_index(File,ModTime,IxTime).

pdt_clear_index(FileSpec):-
    pdt_file_spec(FileSpec,File),
    clear_index(File).
    
update_index(_,ModTime,IxTime):-
	    ModTime @=< IxTime,
	    !.
	    
update_index(File,_,_):-	    
	get_time(IxTime),
	clear_index(File),
	pdt_set(index_times,File,IxTime),
	current_file_annotation(File,Annos,_),
	member(defines(Definitions),Annos),
	index_clauses(File,Definitions).
	
clear_index(File):-
	pdt_remove_all(index_times,File,	_),
	current_file_annotation(File,Annos,_),
	member(defines(Definitions),Annos),
	unindex_clauses(File,Definitions).
	

	
index_clauses(_,[]).
index_clauses(File,[Definition|Definitions]):-
    pdt_get(clause_definitions,Definition,File),
    !,
    index_clauses(File,Definitions).
index_clauses(File,[Definition|Definitions]):-
    pdt_put(clause_definitions,Definition,File),
    index_clauses(File,Definitions).


unindex_clauses(_,[]).
unindex_clauses(File,[Definition|Definitions]):-
    pdt_remove_all(clause_definitions,Definition,File),
    index_clauses(File,Definitions).
	    

time_index(File,IxTime):-
    pdt_get(index_times,File,IxTime),
    !.    
time_index(_, -1).    