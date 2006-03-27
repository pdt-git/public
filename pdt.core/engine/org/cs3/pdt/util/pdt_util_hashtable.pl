%storage abstraction: i want "something" to behave similar to a hashtable.
%assert is to expansive, recordz for itself is to limited: i want to hash terms, 
%not only functors.
%I am not completely sure how to do it, or if it will make much difference in terms
%of performance, so i regard this as an abstraction. Its current implementation is ased on the
%records database.
%
% random access is implemented straight forward by usint hash_term/2
% all operation should be O(1), assuming that collision lists stay
% reasonably short. Currently the implementation does nothing to 
% enforce this (i.e. no re-hashing, splitting, etc.) Yet todo.
%
% To enable efficient iteration of all entries, the individual buckets
% (or collision lists, i.e. the elements of the image of the hash function)
% maintain references to each other in a way that they form a linked list.
% So if pdt_get/3 is called with a variable key, it will successively create 
% all entries of the hash table.
%
%


:-module(pdt_util_hashtable,[
	pdt_put/3,
	pdt_set/3,
	pdt_get/3,
	pdt_remove/3,
	pdt_remove_all/3,
	pdt_remove_all/2
]).


pdt_get(HT,Key,Value):-
    ground(Key),
	hash_key(HT,Key,HashKey),
	recorded(HashKey,entry(Key,Value)).
pdt_get(HT,Key,Value):-
    var(Key),
    first_bucket(HT,First).
    

pdt_set(HT,Key,Value):-
    pdt_remove_all(HT,Key),
    pdt_put(HT,Key,Value).

pdt_put(HT,Key,Value):-
	hash_key(HT,Key,HashKey),
	use_bucket(HT,HashKey),
	recordz(HashKey,entry(Key,Value,Pre,Post)).
    
  

pdt_remove(HT,Key,Value):-
    hash_key(HT,Key,HashKey),
    recorded(HashKey,entry(Key,Value,Pre,Post),Ref),
    erase(Ref),
	unuse_bucket(HT,HashKey).


pdt_remove_all(HT,Key,Value):-
    hash_key(HT,Key,HashKey),
    remove_all(HT,HashKey,Key,Value),
    unuse_bucket(HT,HashKey).

pdt_remove_all(HT,Key):-
    hash_key(HT,Key,HashKey),
    remove_all(HT,HashKey,Key),
    unuse_bucket(HT,HashKey).


remove_all(HT,HashKey,Key,Value):-
    recorded(HashKey,entry(Key,Value,Pre,Post),Ref),
    erase(Ref),
    \+ recorded(HashKey,entry(Key,Value,_,_)),
    !.
remove_all(_,_,_,_).    


remove_all(HT,HashKey,Key):-    
    recorded(HashKey,entry(Key,_,Pre,Post),Ref),
    erase(Ref),
    \+ recorded(HashKey,entry(Key,_,_,_)).
remove_all(_,_,_).    

hash_key(HT,Key,HashKey):-
    hash_term(Key,HVal),
    atom_concat(HT,HVal,HashKey).
    
eraseall(K):-
    recorded(K,_,Ref),
    erase(Ref),
    \+ recorded(K,_),
    !.
eraseall(K):-
    \+ recorded(K,_).


