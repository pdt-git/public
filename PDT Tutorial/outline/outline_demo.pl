% Exported predicates have a green icon in the outline.
% Non-exported predicates have a yellow icon in the outline and can be filtered with the 'Hide private predicates' filter.
:- module(outline_demo, [
    likes/2,
    pair/2,
    single/1
]).

% Multifile predicate defined in this module.
% 
% The outline first lists all clauses of likes/2 defined in this file,
% below the outline lists the files which contribute to likes/2.
% Each listed file shows all its clauses of likes/2 .
:- multifile likes/2.
:- dynamic likes/2.

likes(jack, kate).
likes(sawyer, kate).
likes(kate, jack).
likes(kate, sawyer).

pair(X, Y) :-
    likes(X, Y),
    likes(Y, X).

single(X) :-
    \+ pair(X, _).

add_likes(X, Y) :-
    assertz(likes(X, Y)).

likes(john_locke, the_island).
