%storage abstraction: i want "something" to behave similar to a hashtable.
%assert is to expansive, recordz for itself is to limited: i want to hash terms, 
%not only functors.
%I am not completely sure how to do it, or if it will make much difference in terms
%of performance, so i regard this as an abstraction. Its current implementation is ased on the
%records database.

:-module(pdt_util_hashtable,[
	pdt_put/3,
	pdt_set/3,
	pdt_get/3,
	pdt_remove/3,
	pdt_remove_all/3,
	pdt_remove_all/2
]).

pdt_set(HT,Key,Value):-
    pdt_remove_all(HT,Key),
    pdt_put(HT,Key,Value).

pdt_put(HT,Key,Value):-
	hash_key(HT,Key,HashKey),
	recordz(HashKey,entry(Key,Value,Pre,Post)),
	link_entry(HT,entry(Key,Value,Pre,Post)).

pdt_get(HT,Key,Value):-
    ground(Key),
	hash_key(HT,Key,HashKey),
	recorded(HashKey,entry(Key,Value,_,_)).
pdt_get(HT,Key,Value):-
    var(Key),
    current_head(HT,Head),
    next_entry(HT,Head,entry(Key,Value,_,_)).
    

next_entry(_,Entry,Entry):-
	nonvar(Entry).
next_entry(HT,entry(_,_,_,Post),Next):-
    next_entry(HT,Post,Next).
    
  

pdt_remove(HT,Key,Value):-
    hash_key(HT,Key,HashKey),
    recorded(HashKey,entry(Key,Value,Pre,Post),Ref),
    erase(Ref),
    unlink_entry(HT,entry(Key,Value,Pre,Post)).


pdt_remove_all(HT,Key,Value):-
    hash_key(HT,Key,HashKey),
    remove_all(HT,HashKey,Key,Value).

remove_all(HT,HashKey,Key,Value):-
    recorded(HashKey,entry(Key,Value,Pre,Post),Ref),
    erase(Ref),
    unlink_entry(HT,entry(Key,Value,Pre,Post)),
    \+ recorded(HashKey,entry(Key,Value,_,_)),
    !.
remove_all(_,_,_,_).    

pdt_remove_all(HT,Key):-
    hash_key(HT,Key,HashKey),
    remove_all(HT,HashKey,Key).

remove_all(HT,HashKey,Key):-    
    recorded(HashKey,entry(Key,_,Pre,Post),Ref),
    erase(Ref),
    unlinke_entry(HT,entry(Key,_,Pre,Post)),
    \+ recorded(HashKey,entry(Key,_,_,_)).
remove_all(_,_,_).    

hash_key(HT,Key,HashKey):-
    hash_term(Key,HVal),
    atom_concat(HT,HVal,HashKey).
    
link_entry(HT,entry(Key,Value,OldTail,_)):-
    current_tail(HT,OldTail),
    link_pre(HT,entry(Key,Value,OldTail,_)),
    set_tail(HT,entry(Key,Value,OldTail,_)).

link_pre(HT,entry(Key,Value,Pre,Post)):-
    var(Pre),!,
    set_head(HT,entry(Key,Value,Pre,Post)).
link_pre(_,entry(Key,Value,Pre,Post)):-
    nb_setarg(4,Pre,entry(Key,Value,Pre,Post)).
    
ensure_head_exists(HT,_):-
    current_head(HT,_),!.
ensure_head_exists(HT,Init):-
    set_head(HT,Init).

ensure_tail_exists(HT):-
    current_tail(HT,_),!.
ensure_tail_exists(HT):-
    set_tail(HT,_).

    
unlink_entry(HT,entry(_,_,Pre,Post)):-
	unlink_pre(HT,Pre,Post),
	unlink_post(HT,Pre,Post).

unlink_pre(HT,Pre,Post):-
	current_head(HT,Pre),
	set_head(Post).
unlink_pre(_,Pre,Post):-
	nb_setarg(	4,Pre,Post).

unlink_post(_,_,Post):-
	var(Post).
unlink_post(_,Pre,Post):-
	nb_setarg(	3,Post,Pre).	

current_head(HT,Head):-
    atom_concat(HT,'$head',K),
    recorded(K,Head),!.
current_head(_,_).

current_tail(HT,Tail):-
    atom_concat(HT,'$tail',K),
    recorded(K,Tail),!.
current_tail(_,_).    

set_head(HT,Head):-
    atom_concat(HT,'$head',K),
	eraseall(K),
    recordz(K,Head).

eraseall(K):-
    recorded(K,_,Ref),
    erase(Ref),
    \+ recorded(K,_),
    !.
eraseall(K):-
    \+ recorded(K,_).


set_tail(HT,Tail):-
    atom_concat(HT,'$tail',K),
	eraseall(K),
    recordz(K,Tail).
    