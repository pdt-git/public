/**
 * Public, called from dep_graph.pl::gen_order/2:
 *  - circle(+Set, CircleSortedSet)
 *  - conflict(+Set, CircleSortedSet)
 */


% Look within the elements of arg1 for a cycle  
% and return it in sorted, duplicate-free form in arg2: 
circle(Set, CircleSortedSet) :-
    % assert(cycle_analysis( ----------------------------------------
    path(Set, First, Last, [_h1|Path1], _, all),
    path(Set, Last, First, [_h2|Path2], _, all),
    collect(Path1, Path2, CircleSortedSet).


% Look within the elements of arg1 for a cycle consisting only of negative edges 
% and return it in sorted, duplicate-free form in arg2: 
conflict(Set, CircleSortedSet) :-
    path(Set, First, Last, [_h1|Path1], _, neg),
    path(Set, Last, First, [_h2|Path2], _, neg),
    collect(Path1, Path2, CircleSortedSet).
%    length(CircleSortedSet, _l), _l > 1.


% From 2 paths forming a cycle produce a single one in normalized form
% (suitable for comparisons) by eliminating the duplicates and sorting it.
collect(Path1, Path2, CircleSorted) :-
    append(Path1, Path2, CircleUnsorted),
    sort(CircleUnsorted, CircleSorted).            % sort and remove duplicates
    % list_to_set(CircleSorted, CircleSortedSet).  % order preseving duplicate removal
    

/**
 * path(+Set, ?First, ?Last, ?Path, ?Length, ?Type) 
 *   
 * Set is a list of nodes in the dependency graph  defined by depend/3.
 * The predicate succeeds if Path (arg4) is a path of length Length (arg5)
 * containing only arcs of type Type (arg6) from the First node (arg2) 
 * to the Last node (arg3). 
 * All nodes on the path, including First and Last, must be from Set (arg1). 
 */
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


check_in_set(_, Set) :-
    var(Set), !.
check_in_set(X, Set) :-
    member(X, Set).
    
    
/**************************************************************************/
% The rest of this file is dead code. At least it is not used by the
% dependency analysis
/**************************************************************************/

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

