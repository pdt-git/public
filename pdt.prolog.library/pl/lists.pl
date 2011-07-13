% Author:  Günter Kniesel
% Date: 21.11.2005

:- module( ctc_lists, [
    nth1_non_unifying/3,      % (Index, +List, Elem) ?+? is nondet, ??? is infinite
    union_and_intersection/4, % (+Set1,+Set2,?Union,?Intersection)! <- identity-based equality
    intersection/3,           % (+Set1,+Set2,       ?Intersection)! <- unification-based equality   
    union_sorted/3,           % (+Set1,+Set2,?Union)! 
    union_order_preserving/3, % (+Set1,+Set2,?Union)!
    remove_duplicates_sorted/2,% (+List,?DuplicateFree)!
    remove_duplicates/2,      % (+List,?DuplicateFree)!
    list_sum/2,               % (+Numbers,?Total)!
    traverseList/3,           % (+List,+Stop,+Pred) is nondet
    list_to_disjunction/2,    % (+List,?Disjunction) is det.
    list_to_conjunction/2,    % (+List,?Conjunction) is det.
    pretty_print_list/1,      % (+List)!io 
    pretty_print_list/2,      % (+List,+Indent)!io 
    pretty_print_list_body/2  % (+List,+Indent)
] ).


/**
 * Check list membership without unifying.
 * Succeed only upon *identical* terms.
 * Suceed multiply if member multiply in list!!!!
 */
nth1_non_unifying(Index, List, Elem) :-
    nth1_non_unifying__(List, Elem, Index, 1).
    
nth1_non_unifying__([H|_], Elem, Index, Depth) :-
     Elem == H,
     Index = Depth.
nth1_non_unifying__([_|T], Elem, Index, Depth) :-
     Deeper is Depth+1,
     nth1_non_unifying__(T, Elem, Index, Deeper).



/**
 * union_and_intersection(+Set1,+Set2,?Union,?Intersection) is det
 *
 * Determines the union and intersection of two sets represented as
 * dupliate-free lists.
 * Has no side-effects on free variables (free variables are not unified).
 * Arg1 and arg2 are the input sets.
 * Arg3 is their union.
 * Arg4 is their intersection.
 */

union_and_intersection(S1,S2,U,I) :-
   length(S1,L1),
   length(S2,L2),
   ( L1 < L2
   -> union_inters__(S1,S2,U,I)      % start with shorter list
   ;  union_inters__(S2,S1,U,I)      % start with shorter list
   ).

union_inters__([],Set2,Set2,[]).
union_inters__(Set1,[],Set1,[]) :-
   Set1 \= [].
union_inters__([X|T1],[Y|T2],NewU, NewI):-
   ( (X==Y, union_inters__(T1,T2,U,I),     NewU=[X|U], NewI=[X|I])
   ; (X@<Y, union_inters__(T1,[Y|T2],U,I), NewU=[X|U], NewI=I    )
   ; (X@>Y, union_inters__([X|T1],T2,U,I), NewU=[Y|U], NewI=I    )
   ).


intersection(S1,S2,I) :-
   % prevent propagating side-effects of unification to the call site
   copy_term(S2,S2C), 
   % start with shorter list (for better performance)    
   length(S1,L1),
   length(S2,L2),
   ( L1 < L2
   -> inters__unification_based(S1,S2C,I)      
   ;  inters__unification_based(S2C,S1,I)      
   ).

% Caution: This version unifies terms when compring them.
% If this is not desired, the call site is responsible to
% do pass copies of the relevant terms.
inters__unification_based([],_,[]).
inters__unification_based(Set1,[],[]) :-
   Set1 \= [].
inters__unification_based([X|T1],[Y|T2], NewI):-
   ( (X=Y, !,  inters__unification_based(T1,    T2,I), NewI=[X|I])
   ; (X@<Y,!,  inters__unification_based(T1,[Y|T2],I), NewI=I    )
   ; (X@>Y,    inters__unification_based([X|T1],T2,I), NewI=I    )
   ).

inters__identity_based([],_,[]).
inters__identity_based(Set1,[],[]) :-
   Set1 \= [].
inters__identity_based([X|T1],[Y|T2], NewI):-
   ( (X==Y, inters__identity_based(T1,    T2,I), NewI=[X|I])
   ; (X@<Y, inters__identity_based(T1,[Y|T2],I), NewI=I    )
   ; (X@>Y, inters__identity_based([X|T1],T2,I), NewI=I    )
   ).
   
% Test:
% 
% try_uai(C,F,G,H) :-
%    C = [_G548, _G554, _G556, _G557, _G558, _G559] ,
%    F = [_G564, _G556, _G570, _G571, _G572],
%    union_inters__(C,F,G,H).
 

/**
 * union_sorted(+Set1,+Set2,?Union) is det
 *
 * Determines the union of two sets represented as dupliate-free lists.
 * Has no side-effects on free variables (free variables are not unified).
 * The result is sorted according to the standard order of terms.
 * Arg1 and arg2 are the input sets. 
 * Arg3 is their union.
 * Arg4 is their intersection.
 */
 
union_sorted(Set1,Set2,Set12):- 
    once(union_sorted__(Set1,Set2,Set12)).
 
union_sorted__([],Set2,Set2).
union_sorted__(Set1,[],Set1) :-
   Set1 \= [].
union_sorted__([X|T1],[Y|T2],NewU):-
   ( (X==Y, union_sorted__(T1,T2,U),     NewU=[X|U])
   ; (X@<Y, union_sorted__(T1,[Y|T2],U), NewU=[X|U])
   ; (X@>Y, union_sorted__([X|T1],T2,U), NewU=[Y|U])
   ).


/**
 * union_order_preserving(+Set1,+Set2,?Union) is det
 *
 * Determines the union of two sets represented as dupliate-free lists.
 * Has no side-effects on free variables (free variables are not unified).
 * Does not change the relative order of terms. If duplicates are
 * encountered, the first occurence is retained.
 * Arg1 and arg2 are the input sets. 
 * Arg3 is their union. 
 * Arg4 is their intersection.
 */
union_order_preserving(S1,S2,Res) :-
   append(S1,S2,S3),
   list_to_set(S3,Res), !.

/*
remove_duplicates([First|Rest],NoDup) :-
   member(First,Rest),
   !,
   remove_duplicates(Rest,NoDup).
remove_duplicates([First|Rest],[First|NoDup]) :-
   remove_duplicates(Rest,NoDup).
remove_duplicates([],[]).
*/


/**
 * remove_duplicates_sorted(+List, ?DuplicateFree) is det
 *
 * Arg2 is the duplicate-free version of Arg1. The first occurence
 * of any element of Arg1 is preserved, later ones are deleted.
 * It is assumed that the list in Arg1 is sorted, so any duplicates
 * occur consecutively.
 * Two terms are considered duplicates if they unify. The unification
 * is performed before the removal. So any list of free variables will
 * be collapsed to just one element that is unified with each removed
 * element!
 */
remove_duplicates_sorted([], []) .
remove_duplicates_sorted([First|Rest], [First|UniqueRest]) :-
  remove_duplicates_sorted__(Rest, First, UniqueRest).  

remove_duplicates_sorted__([],_,[]).
remove_duplicates_sorted__([First|Rest], Previous, Result ) :-
   ( First = Previous 
     -> ( Result = RestNoDup,
          remove_duplicates_sorted__(Rest, Previous, RestNoDup)
        )
      ; ( Result = [First|RestNoDup],
          remove_duplicates_sorted__(Rest, First, RestNoDup)
        )
   ).


/**
 * remove_duplicates(+List, ?DuplicateFree) is det
 *
 * Arg2 is the duplicate-free version of Arg1. The first occurence
 * of any element of Arg1 is preserved, later ones are deleted.
 */
remove_duplicates([First|Rest],[First|NoDup]) :-
   split_unique(Rest,First,Before,After),
   !,
   append(Before,After,BA),
   remove_duplicates(BA,NoDup).
remove_duplicates([First|Rest],[First|NoDup]) :-
   remove_duplicates(Rest,NoDup).
remove_duplicates([],[]).

/**
 * split_unique(+List,+Elem,?BeforeNoDups,?AfterNoDups)
 *
 * Arg2 is an element from the list Arg1.
 * Arg3 is the duplicate-free part of Arg1 before Arg2.
 * Arg4 is the duplicate-free part of Arg1 after  Arg2.
 */
split_unique([E|T],E,[],TwithoutE) :-
  remove_duplicates([E|T],[E|TwithoutE]).
split_unique([H|T],E,[H|Before],After) :-
   split_unique(T,E,Before,After).


/*
flatten_one_level([],[]) .
flatten_one_level([H|T],Res) :-
  flatten_one_level(T,Flat),
  append(H,Flat,Res).
*/


/**
 * list_sum(+Numbers, ?Total)
 *  - Arg1 = List of numbers (integer or real)
 *  - Arg2 = Sum of the elements of Arg1.
 * 
 * Sum up a list of numbers.
 */ 
list_sum(Numbers,Total) :- list_sum(Numbers,0,Total).
list_sum([],X,X).
list_sum([H|T],Temp,Res) :-
  NewTemp is Temp+H,
  list_sum(T,NewTemp,Res).

%list_sum([],0).
%list_sum([1],1).
%list_sum([1,2],3).
%list_sum([1,2,3,4,0,3,2,1],16).

/**
 *  Generic list visitor. Traverses any binary encoded list and
 *  executes Pred(X) on all its elements. The functor of the
 *  terms representing binary lists is irrelevant as long as the
 *  head is the first argument and the tail is the second. Stop
 *  is the term representing the end of the sequence.
 */
traverseList(List,Stop,_Pred):-
    List = Stop,
    !.
traverseList(List,Stop,Pred):-
    List =.. [_F,Head,Tail],
    Pred =.. [_P,Head],
    call(Pred),
    traverseList(Tail,Stop,Pred).

/**
 * list_to_disjunction(+List,?Disjunction) is det.
 *
 * Arg1 is a List and Arg2 is its representation as a disjunction.
 */
list_to_disjunction([ ],true) :-!.
list_to_disjunction([A],A   ) :-!.
list_to_disjunction([A|B],(A;Disj)) :-
  list_to_disjunction(B,Disj).

/**
 * list_to_conjunction(+List,?Conjunction) is det.
 *
 * Arg1 is a List and Arg2 is its representation as a conjunction.
 */
list_to_conjunction([ ],true) :-!.
list_to_conjunction([A],A   ) :-!.
list_to_conjunction([A|B],(A,Conj)) :-
  list_to_conjunction(B,Conj).

/**
 * pretty_print_list(+List)
 *  Pretty-print a list, with each element starting on a new line
 *  and fixed two character indentation of each element.
 */
pretty_print_list([]) :-
   format('[]~n').
pretty_print_list([A|B]) :-
   format('[~n'),
   pretty_print_list_body__([A|B]),
   format(']~n').

pretty_print_list_body__([A|B]) :-
    term_to_atom(A,Aterm),
    format('  ~a,~n',[Aterm]),
    pretty_print_list_body__(B).
pretty_print_list_body__([]) .
     

/**
 * pretty_print_list(+List, +Indent)
 *  Pretty-print List, with each element starting on a new line.
 *  Indent is the indentation of each element. It is an atom, 
 *  e.g. '   ' for 3 character indentation.
 */
pretty_print_list(List, Indent) :-
    concat_atom(['~t~',Indent,'|'],Formatstring),  % e.g. '~t~8|'  
    pp_list(List, Formatstring).

pretty_print_list_body(List, Indent) :-
    concat_atom(['~t~',Indent,'|'],Formatstring),  % e.g. '~t~8|'  
    pp_list_body(List, Formatstring).
    
    
pp_list([], Indent) :-
    atom_concat(Indent,'[]~n',Emptylist), 
    format(Emptylist).
pp_list([A|B], Indent) :-
    concat_atom([Indent,'[~n '], Liststart), % 
    format(Liststart),
    pp_list_body([A|B], Indent),
    atom_concat(Indent,']~n',Listend), 
    format(Listend).   
          
pp_list_body([A|B], Indent) :-
    term_to_atom(A,Aterm),
    atom_concat(Indent,'~a,~n',Format), 
    format(Format,[Aterm]),
    pp_list_body(B, Indent).
pp_list_body([], _Indent) .


test_PPL :- pretty_print_list([1,2,3,a,b,c,X,Y,Z,f(a),g(b,c),h(X,Y,Z)]) .  
test_PPL :- pretty_print_list([1,2,3,a,b,c,X,Y,Z,f(a),g(b,c),h(X,Y,Z)], 8) . 
