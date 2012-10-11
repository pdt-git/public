:- module(exact_search, [
	exact_search_predicate/0,
	exact_search_predicate/1,
	exact_search_predicate/2
]).

exact_search_predicate.

exact_search_predicate(1).

exact_search_predicate(1, 2).

references_in_other_file :-
	not_exact_search:at_the_end_search_string(1,2,3,4,5).
	
