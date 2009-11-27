/**
 * Very basic database and I/O utilities.
 *
 * Know uses: In crossref module.
 * Author: Günter Kniesel
 * Date: 13.06.2005
 *
 */
 :- module(basicUtilities, [
       assert_unique_fact/1         % assert_unique_fact(+Head)
%       assert_all/1,                 % assert_all(+List)
%       print_linewise/1              % print_linewise(+List)
    ]).


  /**
   * Assert the fact in arg1 only if it isn't there already. Succeed otherwise.
   */
assert_unique_fact( Head ) :-
    ( call(Head), !
    ; assert(Head)
    ).
%
%  /**
%   * Assert each element of the list in arg1 as one fact.
%   */
%assert_all([]).
%assert_all([H|T]) :-
%   assert(H),
%   assert_all(T).
%
% /**
%  * Print each element of the list in arg1 on one line of the standard output.
%  */
%print_linewise([]) .
%print_linewise([H|T]) :-
%    print(H),
%    nl,
%    print_linewise(T).
