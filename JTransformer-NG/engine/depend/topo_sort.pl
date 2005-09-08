/**
 * Public, called from dep_graph.pl::gen_order/2:
 *  - topo_sort/2
 */


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
  
   setof(dep(X,Y),depend(X,Y),List), member(dep(_x,_y),List).
    
  *
*/

% Precedes relation is the duplicate-free dependency relation:


% precedes(_x,_y) :- depend(_y,_x).
precedes(_x,_y) :- 
   ct_edge(_,_y,_x,_DepElem,Type),
     % Negative self-cycles are not considered harmful. 
     % Ignore them for the purpose of topological sorting:
   ( (_y = _x) -> not(Type = negative) ; true ). 

/*    setof(dep(Y,X),depend(X,Y),List),
%    sort(List,Sorted),                  % eliminate duplicates !!!
    member(dep(_x,_y),List).
*/

/*precedes(_x,_y) :-
    setof(dep(Y,X),ct_edge(_,X,Y,_,_),List),  % eliminate duplicates !!!
    member(dep(_x,_y),List).
*/

/* ********** Topo sorting stuff unused by the dependency analysis:  */

% Find all the elements to be sorted, that is
% all elements related by the precedes realtion:

topo_element(_x) :-
       precedes(_x,_) ; precedes(_,_x).

topo_elements(_all) :-
       setof(_x,topo_element(_x),_all).

% Sort all elements of the precedes relation:

topo_sort(_sorted) :-
       topo_elements(_all),
       topo_sort(_all,_sorted).
       
/* ********** End of unused by the dependency analysis **********  */

/**
 * topo_sort(+list, ?sortedList)
 *
 * Topologically sort a list of elements.
 * Succeeds if arg2 is a topological sort of arg1.
 */
topo_sort(_list,[_start|_sortedrest]) :-
       topo_start(_start,_list,_rest),
       topo_sort(_rest,_sortedrest).
topo_sort([_x],[_x]).


/**
 * topo_start( ?_x, +_from, ?_rest)
 *
 * Predicate to divide a list of elements into a minimal element and
 * remaining elements. Suceeds if arg1 is a minimal element of arg2.
 * Arg3 is arg2 without arg1.
 */

topo_start(_x,_from,_rest) :-
       extract_element(_x,_from,_rest),         % Pick out any element of _from
%        findall(_x,(precedes(_y,_x),member(_y,_from)),_l),_l = []. % compability to bin prolog
        findall(_x,
                (member(_y,_from), precedes(_y,_x)),  % Make sure it is minimal.
                _l),
        _l = [].                                      % Backtrack if it isn't.

/*
topo_start(_x,_from,_rest) :-
       use_cache_results_only,
       !,
       extract_element(_x,_from,_rest),
%       not((precedes(_y,_x),member(_y,_from))).
%        findall(_x,(precedes(_y,_x),member(_y,_from)),_l),_l = []. % compability to bin prolog
        findall(_x,(member(_y,_from),term_to_atom(_y,_ya),term_to_atom(_x,_xa), precedes(_ya,_xa)),_l),_l = []. % compability to bin prolog
*/

/******** Auxiliary functions *****/

/**
 * extract_element(?_x, +_from, ?_rest)
 *
 * Succeds if arg3 is the same list as arg2 but without the element arg1.
 */
extract_element(_x,_from,_rest) :-
       append(_beg,[_x|_end],_from),
       append(_beg,_end,_rest).
