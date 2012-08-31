:- module(not_exact_search, []).

search_string(1,2,3).

search_string_at_the_front(1).

in_the_search_string_middle.

at_the_end_search_string(1,2,3,4,5).

references_in_same_file :-
	search_string_at_the_front(1).
	
