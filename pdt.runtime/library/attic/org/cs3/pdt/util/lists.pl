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

/**
 *
 * @author Tobias Rho <rho@bonn.edu>
 * @author Bruno Harbulot <bruno@cs.man.ac.uk>
 */


/**
 * Flatten List of Lists/Elements.
 * e.g [[a,b],c,[d,e,f]] -> [a,b,c,d,e,f].
 * concat(+Lists,?List)
 */
concat_lists([L],[]):-
    is_list(L),
    L = [],
    !.
concat_lists([List],[Head|Tail]):-
	is_list(List),
	List = [Head|Tail],
	!.
concat_lists([Elem],[Elem]) :- !.

concat_lists([[HeadHead|HeadTail]|Tail],FlatList) :-
    !,
	concat_lists(Tail,TailFlat),
    append([HeadHead|HeadTail],TailFlat,FlatList).
    
concat_lists([[]|Tail],TailFlat):-
	concat_lists(Tail,TailFlat).

concat_lists([Head|Tail],[Head|TailFlat]):-
	not(is_list(Head)),
	concat_lists(Tail,TailFlat).    
    
test(concat_list_2):-
    assert_true('[a,b],[d,e,f]',concat_lists([[a,b],[d,e,f]],
    	[a,b,d,e,f])),
    assert_true('[a,b],[d,e,f]',(concat_lists([[a,b],[d,e,f]],
    	Flat),!,Flat=[a,b,d,e,f])),

    assert_true('[a,b],c,[d,e,f]', concat_lists([[a,b],c,[d,e,f]],
    	[a,b,c,d,e,f])),
    assert_true('[a,B],C,[d,e,f]',concat_lists([[a,B],C,[d,e,f]],
    	[a,B,C,d,e,f])),
   assert_true('[a,B],C,[d,e,f]',(concat_lists([[a,B],C,[d,e,f]],
    	Flat2), !,Flat2=[a,B,C,d,e,f])),
   assert_true('[],[],[d,e,f]',(concat_lists([[],[],[d,e,f]],Flat3),
    !,Flat3=[d,e,f])),
   assert_true('[A],[A]',(concat_lists([V1],[V2]),
    !,V1 == V2)).


    
/**
        append(?List1, ?List2, ?List3, ?List4)
        
        Succeeds  when List4 unifies with  the concatenation of List1, List2 and
    List3. The  predicate can be used with any instantiation pattern
    (even four variables).
*/

append(_first, _second, _third, _Ret) :-
    append(_first, _second, _dummyList),
    append(_dummyList, _third, _Ret).

/**
        append(?List1, ?List2, ?List3, ?List4, ?List5)
        
        Succeeds  when List5 unifies with  the concatenation of List1, List2, List3 and
    List4. The  predicate can be used with any instantiation pattern
    (even five variables).
*/

append(_first, _second, _third, _fourth, _Ret) :-
    append(_first, _second, _third, _dummyList),
    append(_dummyList, _fourth, _Ret).


/*
        prepend(+List, +Elem, -NewList)
*/

prepend(_list, _elem, _return) :-
    append([_elem],_list,_return).

/*
    insertBefore(+List, +TargetMember, +NewMember, -NewList)
    
    Inserts NewMember before TargetMember.
*/

insertBefore([_Head | _Tail], _elem, _newElem, _RetList) :-
    equals(_Head, _elem),
    append([_Head], [_newElem], _Tail, _RetList).

insertBefore([_Head | _Tail], _elem, _newElem, _RetList) :-
    not(equals(_Head, _elem)),
    insertBefore(_Tail, _elem, _newElem, _NewTail),
    append([_Head], _NewTail, _RetList).

insertBefore([], _elem, _newElem, _RetList) :- equals(_RetList, []).
    
    
%replaceItemWithListInList(Item, OldList, SubList, NewList).
replaceItemWithListInList(_, [], _, []).
replaceItemWithListInList(S, [X | OldList], [], [X |  NewList]) :-
	X\=S,
    replaceItemWithListInList(S, OldList, [], NewList).
replaceItemWithListInList(S, [S | OldList], [], OldList).
replaceItemWithListInList(S, [X | OldList], SubList, [X | NewList]) :-
    X\=S,
    replaceItemWithListInList(S, OldList, SubList, NewList).
replaceItemWithListInList(S, [S | OldList], [X | SubList], [X | NewList]) :-
    replaceItemWithListInList(S, [S | OldList], SubList, NewList).


%insertListBeforeItemInList(InsertionPoint, OldList, ListToInsert, NewList).
insertListBeforeItemInList(_, [], _, []).
insertListBeforeItemInList(_, List, [], List).
insertListBeforeItemInList(InsertionPoint, [X | OldList], ListToInsert, [X | NewList]) :-
    X\=InsertionPoint,
    insertListBeforeItemInList(InsertionPoint, OldList, ListToInsert, NewList).
insertListBeforeItemInList(InsertionPoint, [InsertionPoint | OldList], [], [InsertionPoint | NewList]) :-
    insertListBeforeItemInList(InsertionPoint, OldList, [], NewList).
insertListBeforeItemInList(InsertionPoint, [InsertionPoint | OldList], [X | T], [X | NewList]) :-
    insertListBeforeItemInList(InsertionPoint, [InsertionPoint | OldList], T, NewList).


%substractListFromList(LongList, ElementsToSubstract, ShortenedList)
substractListFromList([], _, []).
substractListFromList(List, [], List).
substractListFromList([ A | LongList ], [ A | ElementsToSubstract], ShortenedList) :-
    substractListFromList(LongList,  [A | ElementsToSubstract], TempList),
    substractListFromList(TempList, ElementsToSubstract, ShortenedList).
substractListFromList([ A | LongList ], [ B | ElementsToSubstract], ShortenedList) :-
    A \= B,
    substractListFromList(LongList,  [B | ElementsToSubstract], TempList),
    substractListFromList([ A | TempList ], ElementsToSubstract, ShortenedList).


replaceItemInList(_, _, [], [], 0).
replaceItemInList(OldItem, NewItem, [OldItem | OldList], [NewItem | NewList], Count) :-
    replaceItemInList(OldItem, NewItem, OldList, NewList, OldCount), Count is OldCount+1.
replaceItemInList(OldItem, NewItem, [Item | OldList], [Item | NewList], Count) :-
    Item\=OldItem,
    replaceItemInList(OldItem, NewItem, OldList, NewList, Count).

replaceItemInList(OldItem, NewItem, OldList, NewList) :-
    replaceItemInList(OldItem, NewItem, OldList, NewList, Count),
    Count>0.
    
    
mapGoalAlwaysSucceed(_, []).
mapGoalAlwaysSucceed(Goal, [X | T]) :-
    (call(Goal, X) ; true),
    mapGoalAlwaysSucceed(Goal, T).
    
mapGoalMayFail(_, []).
mapGoalMayFail(Goal, [X | T]) :-
    call(Goal, X),
    mapGoalMayFail(Goal, T).



revertList([], AccList, AccList).
revertList([X | L1], AccList, NewAccList) :-
    revertList(L1, [ X | AccList], NewAccList).
revertList([], []).
revertList(L1, L2) :-
    revertList(L1, [], L2).


lastElement([], Previous, Previous).
lastElement([X | T], _, LastElement) :-
    !, lastElement(T, X, LastElement).
lastElement([], _) :- !, fail.
lastElement(List, LastElement) :-
    !, lastElement(List, _, LastElement).

print_list([]).
print_list([_head|[]]) :-
    !,
    write(_head).

print_list([_head|_tail]) :-
    format('~w, ',_head),
    print_list(_tail).

listOrEmptyListIfNull(_elem, []) :-
    equals(_elem, 'null').
listOrEmptyListIfNull(_elem, [_elem]) :-
    not(equals(_elem, 'null')).

emptyListIfNull(_elem, []) :-
    equals(_elem, 'null').
emptyListIfNull(_elem, _elem) :-
    not(equals(_elem, 'null')).

/**
 * atom_to_list(+Atom,+Sep,-List)
 * 
 * Parses Atom into a sub atom list 
 * which are separated by Sep.
 */
 

atom_to_list(Name,Sep, [First|RestNames]):-
    atom_concat(First,Sep,RestName, Name),
    !,
        atom_to_list(RestName,Sep,RestNames).
        
atom_to_list(Name,_, [Name]).  

test(atom_to_list):-
    atom_to_list( 'Asdf,fdsa,2', ',',['Asdf',fdsa,'2']).
