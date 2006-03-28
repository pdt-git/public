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
	pdt_remove_all/2,
	pdt_clear/1
]).


pdt_get(HT,Key,Value):-
    ground(Key),
	hash_key(HT,Key,HashKey),
	recorded(HashKey,entry(Key,Value)).
pdt_get(HT,Key,Value):-
    var(Key),
    first_key(HT,First),
    next_entry(First,entry(Key,Value)).

next_entry(HKey,entry(Key,Value)):-
    recorded(HKey,entry(Key,Value)).
next_entry(HKey,Next):-
	next_key(HKey,NextKey),
	next_entry(NextKey,Next).

next_key(HKey,NextKey):-
	recorded(HKey,chain(_,NextKey)),
	nonvar(NextKey).

pdt_set(HT,Key,Value):-
    pdt_remove_all(HT,Key),
    pdt_put(HT,Key,Value).

pdt_put(HT,Key,Value):-
	hash_key(HT,Key,HashKey),
	use_key(HT,HashKey),
	recordz(HashKey,entry(Key,Value)).
    
  

pdt_remove(HT,Key,Value):-
    hash_key(HT,Key,HashKey),
    recorded(HashKey,entry(Key,Value),Ref),
    erase(Ref),
	unuse_key(HT,HashKey).


pdt_remove_all(HT,Key,Value):-
    hash_key(HT,Key,HashKey),
    remove_all(HashKey,Key,Value),
    unuse_key(HT,HashKey).

pdt_remove_all(HT,Key):-
    hash_key(HT,Key,HashKey),
    remove_all(HashKey,Key),
    unuse_key(HT,HashKey).

pdt_clear(HT):-
    first_key(HT,Key),
	clear_recursive(Key),
	set_first_key(HT,_),
	set_last_key(HT,_).

clear_recursive(Key):-
    next_key(Key,Next),
    eraseall(Key),
	clear_recursive(Next).
clear_recursive(Key):-
    eraseall(Key).

remove_all(HashKey,Key,Value):-
    recorded(HashKey,entry(Key,Value),Ref),
    erase(Ref),
    \+ recorded(HashKey,entry(Key,Value)),
    !.
remove_all(_,_,_).    


remove_all(HashKey,Key):-    
    recorded(HashKey,entry(Key,_),Ref),
    erase(Ref),
    \+ recorded(HashKey,entry(Key,_)).
remove_all(_,_).    

hash_key(HT,Key,HashKey):-
    hash_term(Key,HVal),
    atom_concat(HT,HVal,HashKey).
    



use_key(_,HKey):-
    % if there are already records, nothing is todo
    recorded(HKey,_),
    !.
use_key(HT,HKey):-
    % the default case: connect last key with current,
    % update lastkey.
    last_key(HT,LastKey),!,
    recorded(LastKey,chain(LastPrev,_)),
    eraseall(LastKey,chain(_,_)),
    recorda(LastKey,chain(LastPrev,HKey)),
    recorda(HKey,chain(LastKey,_)),
    set_last_key(HT,HKey).
use_key(HT,HKey):-
    % the map is empty. the new key will be both
    % first and last key.
	set_first_key(HT,HKey),
	set_last_key(HT,HKey),
	recorda(HKey,chain(_,_)).	




unuse_key(_,HKey):-
    % if there are still records, nothing is todo
    recorded(HKey,entry(_,_)),
    !.

unuse_key(HT,HKey):-
    recorded(HKey,chain(Prev,Next)),
    unlink_prev(HT,Prev,Next),
    unlink_next(HT,Prev,Next),
    eraseall(HKey).
unuse_key(_,HKey):-
    % if there is no chain record, nothing is todo
    \+recorded(HKey,chain(_,_)),
    !.


unlink_prev(HT,Prev,Next):-
    %If Prev is not bound, the removed key was the first key.
    %So Next will be the new First key.
    var(Prev),
    !,
    set_first_key(HT,Next).
unlink_prev(_,Prev,Next):-
    %Otherwise, Prev was pointing to the removed key as its successor.
    %we need to update this reference to point to NEXT
    recorded(Prev,chain(PrevPrev,_)),
    eraseall(Prev,chain(_,_)),
    recorda(Prev,chain(PrevPrev,Next)).


unlink_next(HT,Prev,Next):-
    %If Next is not bound, the removed key was the last key.
    %So Prev will be the new last key
    var(Next),
    !,
    set_last_key(HT,Prev).
unlink_next(_,Prev,Next):-
    %Otherwise, Next was pointing to the removed key as its predecessor.
    %we need to update this reference to point to Prev
    recorded(Next,chain(_,NextNext)),
    eraseall(Next,chain(_,_)),
    recorda(Next,chain(Prev,NextNext)).


first_key(HT,First):-
    atom_concat(HT,'$first',K),
    recorded(K,First).

last_key(HT,Last):-
    atom_concat(HT,'$last',K),
    recorded(K,Last).


set_last_key(HT,Last):-
    var(Last),
    atom_concat(HT,'$last',K),
    eraseall(K).    
set_last_key(HT,Last):-
    nonvar(Last),
    atom_concat(HT,'$last',K),
    eraseall(K),
    recorda(K,Last).

set_first_key(HT,First):-
    var(First),
    atom_concat(HT,'$first',K),
    eraseall(K).    
set_first_key(HT,First):-
    nonvar(First),
    atom_concat(HT,'$first',K),
    eraseall(K),
    recorda(K,First).

	
eraseall(K):-
    recorded(K,_,Ref),
    erase(Ref),
    \+ recorded(K,_),
    !.
eraseall(K):-
    \+ recorded(K,_).

eraseall(K,V):-
    recorded(K,V,Ref),
    erase(Ref),
    \+ recorded(K,V),
    !.
eraseall(K,V):-
    \+ recorded(K,V).	