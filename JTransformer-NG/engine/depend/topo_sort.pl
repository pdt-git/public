/**************************************************************************\
  * Toplogical sorting:
  *
  *   Declare a binary relation "precedes"
  *
  *   Then do the query   :- topo_sort(_x).
  *
  * Example code how to use this:
  *
  *   %Some example precedes declarations. This is how you declare dependencies.
  *
  *   precedes(foundation,walls).
  *   precedes(foundation,floor).
  *   precedes(walls,roof).
  *   precedes(walls,windows).
  *
  *   %Example 1: find all elements that where declared in "precedes"
  *   :- topo_element(_x).
  *
  *   %Example 2: same as example 1, but finds them in a list.
  *   :- topo_elements(_x).
  * 
  *   %Example 3: topological sort the elements. (will find all possible
  *   %           sortings that satisfy the precedes relationship)
  *   :- topo_sort(_s).
  * 
  *   %Example 4: only the first sorting
  *   :- FIRST(topo_sort(_s)).
  *
*/
precedes(_x,_y) :- depend(_y, _x).


/***** Topo sorting stuff *****/


/* predicates for findin all the elements to be sorted*/

topo_element(_x) :-
       precedes(_x,_) ; precedes(_,_x).

topo_elements(_all) :-
       setof(_x,topo_element(_x),_all).


/* predicate to divide a list of elements into a minimal element and remaining elements. */
/*
topo_start(_x,_from,_rest) :-
       use_cache_results_only,
       !,
       my_member(_x,_from,_rest),
%       not((precedes(_y,_x),member(_y,_from))).
%        findall(_x,(precedes(_y,_x),member(_y,_from)),_l),_l = []. % compability to bin prolog
        findall(_x,(member(_y,_from),term_to_atom(_y,_ya),term_to_atom(_x,_xa), precedes(_ya,_xa)),_l),_l = []. % compability to bin prolog
*/

/* predicate to divide a list of elements into a minimal element and remaining elements. */
topo_start(_x,_from,_rest) :-
       my_member(_x,_from,_rest),
%       not((precedes(_y,_x),member(_y,_from))).
%        findall(_x,(precedes(_y,_x),member(_y,_from)),_l),_l = []. % compability to bin prolog
        findall(_x,(member(_y,_from),precedes(_y,_x)),_l),_l = []. % compability to bin prolog

/* topo sort a list of elements */
topo_sort(_list,[_start|_sortedrest]) :-
       topo_start(_start,_list,_rest),
       topo_sort(_rest,_sortedrest).
       topo_sort([_x],[_x]).

topo_sort(_sorted) :-
       topo_elements(_all),
       topo_sort(_all,_sorted).


/******** Auxiliary functions *****/

/* like my_member but also binds the other elements to _rest */
my_member(_x,_from,_rest) :-
       append(_beg,[_x|_end],_from),
       append(_beg,_end,_rest).
