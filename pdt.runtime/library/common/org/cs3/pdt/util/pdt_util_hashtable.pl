%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
% 
% Author: Lukas Degener (among others) 
% E-mail: degenerl@cs.uni-bonn.de
% WWW: http://roots.iai.uni-bonn.de/research/pdt 
% Copyright (C): 2004-2006, CS Dept. III, University of Bonn
% 
% All rights reserved. This program is  made available under the terms 
% of the Eclipse Public License v1.0 which accompanies this distribution, 
% and is available at http://www.eclipse.org/legal/epl-v10.html
% 
% In addition, you may at your option use, modify and redistribute any
% part of this program under the terms of the GNU Lesser General Public
% License (LGPL), version 2.1 or, at your option, any later version of the
% same license, as long as
% 
% 1) The program part in question does not depend, either directly or
%   indirectly, on parts of the Eclipse framework and
%   
% 2) the program part in question does not include files that contain or
%   are derived from third-party work and are therefor covered by special
%   license agreements.
%   
% You should have received a copy of the GNU Lesser General Public License
% along with this program; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
%   
% ad 1: A program part is said to "depend, either directly or indirectly,
%   on parts of the Eclipse framework", if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the "pdt" package
%   fragment in their name fall into this category.
%   
% ad 2: "Third-party code" means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% NOTE: this library should be considered public api.
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
% NOTE: this library is deprecated!!!.
% !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
	pdt_ht_put/3,
	pdt_ht_set/3,
	pdt_ht_get/3,
	pdt_ht_remove/3,
	pdt_ht_remove_all/3,
	pdt_ht_remove_all/2,
	pdt_ht_clear/1
]).


pdt_ht_get(HT,Key,Value):-
    ground(Key),
	hash_key(HT,Key,HashKey),
	recorded(HashKey,entry(Key,Value)).
pdt_ht_get(HT,Key,Value):-
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

pdt_ht_set(HT,Key,Value):-
    pdt_ht_remove_all(HT,Key),
    pdt_ht_put(HT,Key,Value).

pdt_ht_put(HT,Key,Value):-
	hash_key(HT,Key,HashKey),
	use_key(HT,HashKey),
	recordz(HashKey,entry(Key,Value)).
    
  

pdt_ht_remove(HT,Key,Value):-
    hash_key(HT,Key,HashKey),
    recorded(HashKey,entry(Key,Value),Ref),
    erase(Ref),
	unuse_key(HT,HashKey).


pdt_ht_remove_all(HT,Key,Value):-
    hash_key(HT,Key,HashKey),
    remove_all(HashKey,Key,Value),
    unuse_key(HT,HashKey).

pdt_ht_remove_all(HT,Key):-
    hash_key(HT,Key,HashKey),
    remove_all(HashKey,Key),
    unuse_key(HT,HashKey).

pdt_ht_clear(HT):-
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