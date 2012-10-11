:- module(completion_demo, []).

:- use_module(library(pldoc)).

:- doc_collect(true).

%% my_predicate_with_documentation(+Value) is det.
% 
% True if Value can is an atom.
my_predicate_with_documentation(Value) :-
	atom(Value).

my_predicate_without_documentation(Value) :-
	atomic(Value).

% Use the auto-completion of the Prolog Editor by typing a prefix and pressing CTRL+Space.
% The auto-completion lists all matching predicates and modules.
% If the documentation of a selected predicate has been parsed it is shown right to the list of predicates
% and the variable names used in the documentation will be inserted as arguments of the predicate.
% E.g. type the prefix "my_pre" and press CTRL+Space.

