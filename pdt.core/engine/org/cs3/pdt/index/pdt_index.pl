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
    time_file(File,ModTime),
    time_index(File,IxTime),
    update_index(File,Annos,ModTime,IxTime).


pdt_update_index(FileSpec):-
    pdt_file_spec(FileSpec,File),
    time_file(File,ModTime),
    time_index(File,IxTime),
    current_file_annotation(File,Annos,_),
    update_index(File,Annos,ModTime,IxTime).

pdt_clear_index(FileSpec):-
    pdt_file_spec(FileSpec,File),
    current_file_annotation(File,Annos,_),
    clear_index(File,Annos).
    
update_index(_,_,ModTime,IxTime):-
	    ModTime @=< IxTime,
	    !.
	    
update_index(File,Annos,_,_):-	    
	get_time(IxTime),
	clear_index(File,Annos),
	pdt_ht_set(index_times,File,IxTime),	
	member(defines(Definitions),Annos),
	index_clauses(File,Definitions).
	
clear_index(File,Annos):-
	pdt_ht_remove_all(index_times,File,_),
	member(defines(Definitions),Annos),
	unindex_clauses(File,Definitions).
	

	
index_clauses(_,[]).
index_clauses(File,[Definition|Definitions]):-
    pdt_ht_get(clause_definitions,Definition,File),
    !,
    index_clauses(File,Definitions).
index_clauses(File,[Definition|Definitions]):-
    pdt_ht_put(clause_definitions,Definition,File),
    index_clauses(File,Definitions).


unindex_clauses(_,[]).
unindex_clauses(File,[Definition|Definitions]):-
    pdt_ht_remove_all(clause_definitions,Definition,File),
    index_clauses(File,Definitions).
	    

time_index(File,IxTime):-
    pdt_ht_get(index_times,File,IxTime),
    !.    
time_index(_, -1).    