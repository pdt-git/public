circle(Set, CircleSortedSet) :-
    path(Set, First, Last, [_h1|Path1], _, all),
    path(Set, Last, First, [_h2|Path2], _, all),
    collect(Path1, Path2, CircleSortedSet).

conflict(Set, CircleSortedSet) :-
    path(Set, First, Last, [_h1|Path1], _, neg),
    path(Set, Last, First, [_h2|Path2], _, neg),
    collect(Path1, Path2, CircleSortedSet).
%    length(CircleSortedSet, _l), _l > 1.

iteration(Set, CircleSortedSet) :-
    path(Set, First, Last, [_h1|Path1], _, 'pos_mon+'),
    path(Set, Last, First, [_h2|Path2], _, 'pos_mon+'),
    collect(Path1, Path2, CircleSortedSet).
%    length(CircleSortedSet, _l), _l > 1.
iteration(Set, CircleSortedSet) :-
    path(Set, First, Last, [_h1|Path1], _, 'pos_mon-'),
    path(Set, Last, First, [_h2|Path2], _, 'pos_mon-'),
    collect(Path1, Path2, CircleSortedSet).
%    length(CircleSortedSet, _l), _l > 1.

transDepend(Set, _ct1, _ct2) :-   path(Set, _ct1, _ct2, _, _,_).

/**************************************************************************/

check_in_set(_, Set) :-
    var(Set), !.
check_in_set(X, Set) :-
    member(X, Set).

/* path(?Set, ?First, ?Last, ?Path, ?Length) is true if Path is a path of length     */
/*   Length from the First vertex to the Last vertex in the graph defined  */
/*   by e/3.                                                               */
path(Set, First, Last, Path, Length, _type):-
  path_1(Set, First, Last, [], 0, Path, Length, _type).

path_1(Set, Last, Last, Path, Length, [Last|Path], Length, _type) :-
  nonvar(Last),
  check_in_set(Last, Set).
path_1(Set, First, Last, Path0, Length0, Path, Length, _type):-
  pos_neg_depend(NextToLast, Last, _, _type),
  \+member(NextToLast, Path0),
  check_in_set(Last, Set),
  check_in_set(NextToLast, Set),
  Length2 is Length0 + 1,
  path_1(Set, First, NextToLast, [Last|Path0], Length2, Path, Length, _type).

pos_neg_depend(_ct1, _ct2, _e, neg)        :-  negDepend(_ct1, _ct2, _e).
pos_neg_depend(_ct1, _ct2, _e, 'pos_mon+') :-  posDepend(_ct1, _ct2, _e), _e \= not(_).
pos_neg_depend(_ct1, _ct2, _e, 'pos_mon-') :-  posDepend(_ct1, _ct2, not(_e)).
pos_neg_depend(_ct1, _ct2, _e, all)        :-  posDepend(_ct1, _ct2, _e).
pos_neg_depend(_ct1, _ct2, _e, all)        :-  negDepend(_ct1, _ct2, _e).

collect(Path1, Path2, CircleSortedSet) :-
    append(Path1, Path2, CircleUnsorted),
    sort(CircleUnsorted, CircleSorted),
    list_to_set(CircleSorted, CircleSortedSet).

