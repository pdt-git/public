:- module(search_demo, [abc/1, p/1]).

% Select abc and call "Find all Declarations and Definitions" from the context menu.
% You should find results in the following modules:
% - search_demo (local): the clause below is found and this result is "local" because it is started from here
% - search_demo_sub (sub): this module imports search_demo and redefines abc/1, therefore this result is marked as sub
% - other_module (invisible): this module also defines a predicate abc/1, but this is not related to search_demo:abc/1
abc(0).

% Select abc and call "Find References" from the context menu.
% You should find both clauses of p/1 as result because both clauses contain a term p(_).
p(X) :-
	abc(X).

p(Y) :-
	search_demo_sub:abc(Y).
	