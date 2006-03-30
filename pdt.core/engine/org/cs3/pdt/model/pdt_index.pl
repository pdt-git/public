
:- module(pdt_index,[
	pdt_index_load/2,
	pdt_index_store/2,
	pdt_index_put/4,	
	pdt_index_remove/4,	
	pdt_index_get/3,		
	pdt_index_after/4				
]).

%Currently we only support rb trees as index data structure. Once i found a good way to implement a
%hash table with non-destructive, backtrackable behaviour, there will also be hash index tables.
%Although arbitrary terms may be used for both index key and value, it is recommended to use entity handles as value.
%see pdt_handle.pl

:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_hashtable')).
:- use_module(library('org/cs3/pdt/util/pdt_util_rbtree')).


% pdt_index_load(+IxName,-IX)
%
% load the index table that is associated with the name IxName.
% If no such association exists, a new index table is created and associated with IxName.
pdt_index_load(IxName,IX):-
    pdt_ht_get(index,IxName,IX),!.
pdt_index_load(IxName,IX):-
    pdt_rbtree_new(IX),
    pdt_index_store(IxName,IX).

% pdt_index_store(+IxName,+IX)
%
% Store the index table IX. This will associate the index table with the name IxName. 
% The index table is stored in a way that keeps it unaffected by backtracking.
% If another index table was already associated with IxName, the old table is droped and replaced 
% by the new one.
pdt_index_store(IxName,IX):-
    pdt_ht_set(index,IxName,IX).

% pdt_index_put(+IX,+Key,+Value,-NewIX)
%
% Add the entry Key,Value to the index.
% Does nothing if the Key,Value pair already exists in the index
% Note however that there can be an arbitrary number of entries with the same key, 
% provided the values differ.
% The resulting index table is unified with NewIX.
pdt_index_put(IX,Key,Val,IX):-
    pdt_rbtree_lookup(Key,Val,IX),!.
pdt_index_put(IX,Key,Val,NextIX):-    
    pdt_rbtree_insert(IX,Key,Val,NextIX).


% pdt_index_remove(+IX,+Key,?Value,-NewIX)
%
% Remove all entries matching Key and Value from the index IX.
% The resulting index table will be unified with NewIX
pdt_index_remove(IX,Key,Value,NewIX):-
    pdt_index_get(IX,Key,Value),
    !,
    pdt_rbtree_delete(IX,Key,Value,NextIX),
    pdt_index_remove(NextIX,Key,Value,NewIX).
pdt_index_remove(IX,_,_,IX).
    

% pdt_index_after(+IX, +Key,-Val)
%
% find the first matching entry for Key.
% On backtracking, this will produce all other entries for Key.
pdt_index_get(IX,Key,Val):-
    pdt_rbtree_lookup(Key,Val,IX).

% pdt_index_after(+IX,+Start,-Key,-Val)
%
% find the next neighbour, i.e. the next entry with a key greater then or equal to
% Start. On Backtracking, this will successfully produce all entries after the first one found,
% in standard term order. This can be used to implement interval searches in the index.
pdt_index_after(IX,Start,Key,Val):-
    pdt_rbtree_next(Start,Key,Val,IX).    


