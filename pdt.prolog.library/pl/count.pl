% Author: Günter Kniesel
% Date:   12.06.2004

/* ***********************************************************
 * Different semantic and implementation variants of counting:
 *  - Semantic: Counting derivations (success) versus results.
 *  - Implementation: Based on nb_setargs/3, flag/3 or findall/3.
 * *********************************************************** */
 
count_facts(Goal, Nr) :-
  predicate_property(Goal, number_of_clauses(Nr)).

:- meta_predicate count_success(0, -).     
                                    
count_success(Goal, Times) :-    
        Counter = counter(0),       
        (   catch(Goal,_,fail),                   
                 arg(1, Counter, N0),    
                 N is N0 + 1,            
                 nb_setarg(1, Counter, N),   
            fail                    
        ;   arg(1, Counter, Times)  
        ).        
  
:- module_transparent count/2.

count(Goal, _) :-
  flag(successcounter,_,0),
  catch(Goal,_Any,fail),     % turn exceptions into failures
    flag(successcounter,N,N+1),
  fail.
count(_, N) :-
  flag(successcounter,N,N).


:- module_transparent count_and_print/2.

count_and_print(Goal, _) :-
  flag(successcounter,_,0),
  catch(Goal,_Any,fail),     % turn exceptions into failures
    flag(successcounter,N,N+1),
    format('~w.~n', [Goal]),
  fail.
count_and_print(_, N) :-
  flag(successcounter,N,N).
  
  
:- module_transparent count_unique/2.

count_unique(Goal,Nall,Nunique) :-
  findall(Goal, catch(Goal,_Any,fail), All),    % turn exceptions into failures
  sort(All,Unique),
  length(All,Nall),
  length(Unique,Nunique).

/*
?- count(create_generic_edges:parent(Id,EdgeVal,NodeType,TargetType),N).
N = 70894 ;

?- count( (ast_node_type_dummy(Id,Type), not( create_generic_edges:parent(Id, EdgeVal, Type, TargetType) )),N).
N = 29742 ;

?- X is 29742 + 70894.

X = 100636 

?- count( ast_node_type_dummy(_,_) , N).
N = 100636 ;
*/


/* ***************************************************************
   Findall-based counting. 
   ***************************************************************
   Inappropriate for large factbases but useful for counting
   but useful for counting without duplicates -- which is 
   currently not supported by count/2, count_facts/2 above.
*/

/* *
 * count_all_and_unique(+Goal,Nall,Nunique)
 *   Find all results and all unique results as lists and count
 *   them using count_list_elements.
 */
% Count exceptions as failures:
:- module_transparent count_unique/2.

count_all_and_unique(Goal,Nall,Nunique) :-
  findall(Goal, catch(Goal,_Any,fail), All),  
  count_list_elements(All,_SortedUnique,Nall,Nunique).


/**
 * count_list_elements(+All,?SortedUnique,?LengthAll,?LengthSortedUnique)
 *   Return sorted, duplicate-free list and lenght of both lists.
 */   
count_list_elements(All,SortedUnique,LengthAll,LengthSortedUnique) :-
  sort(All,SortedUnique),
  length(All,LengthAll),
  length(SortedUnique,LengthSortedUnique).
  
/* ********************************************************** 
   Experimental stuff, for grouping before counting. Useful
   for instance, for determining singular groups,  
*/
   

/* *
 * group_by(+Other, +Goal, ?OtherVals)
 *   Other is a term containing variables of Goal whose 
 *   values we want to determine. OtherVals will contain
 *   one instance of Other for each group of same values
 *   for the remaining variables og Goal.
 */ 
group_by(Other, Goal, OtherVals) :-
    bagof(Other, call(Goal), OtherVals ).

non_singular_groups(Other, Goal, Groups) :-
   findall( OtherVals, ( group_by(Other, Goal, OtherVals), 
                         length(OtherVals,N),
                         N>1
                       ),
            Groups ).

non_singular_groups__(Groups) :-
   findall( OtherVals, 
            ( bagof(packCls(Pkg,Id), 
                    class_in_package(_PkgN,_N,Pkg,Id), 
                    OtherVals ), 
              length(OtherVals,L), 
              L>1
             ),
             Groups ).             

all_external_groups :- 
   non_singular_groups__(Groups),
   forall( ( member(List,Groups),
             member(packCls(_Pkg,Class),List)
            ),
            externT(Class)
   ).
   
