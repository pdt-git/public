
%
% @author Bruno Harbulot <bruno@cs.man.ac.uk>
%




% TODO Make the order of parameters for replaceItemWithList and replaceItem consistent.



concatList([], L, L).
concatList([X|L1], L2, [X|L3]) :- 
    concatList(L1, L2, L3).
    


    
sizeOfList([], Size, Size).
sizeOfList([_ | T], OldSize, NewSize) :-
    TempSize is OldSize+1,
    sizeOfList(T, TempSize, NewSize).

sizeOfList(List, Size) :-
    sizeOfList(List, 0, Size).
   
%sizeOfList([], 0).
%sizeOfList([_ | T], Size) :-
%    sizeOfList(T, OldSize),
%    Size is OldSize+1.




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






isInList(_, []) :- fail.
isInList(Item, [Item | _]).
isInList(Item, [X | T]) :- X\=Item, isInList(Item, T).



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
