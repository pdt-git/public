:- module(other_module, []).

% Start a global search for references to the module "lists" (in the menu Search->Search... or CTRL+H from the editor).
% You should find lots of results, including two results from this file:
% - the use_module/1 directive below and
% - the dummy/1 predicate below since it contains the call to lists:member/2.
% The search is performed on all consulted code. Therefore it finds lots of results contained in SWI Prolog code.
:- use_module(library(lists)).

abc(2).

dummy :-
	member(_, [_,_]).