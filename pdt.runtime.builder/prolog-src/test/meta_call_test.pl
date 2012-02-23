:- module(meta_call_test,[
		
	]).


:- use_module(meta_call_test_data).
:- use_module('../analyzer/metapred_finder').

%:- find_all_meta_predicates.

:- begin_tests(meta_call_inference).

% :- use_module(...).


test(direct_meta_call) :-
        true.

:- end_tests(meta_call_inference).