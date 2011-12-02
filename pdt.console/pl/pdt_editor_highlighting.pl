% Utility used by pdt/src/org/cs3/pdt/internal/editors/PLScanner.java

:- module( pdt_editor_highlighting,
         [ predicates_with_property/3  
         ]).


               /************************************************
                * PREDICATE PROPERTIES FOR SYNTAX HIGHLIGHTING *
                ************************************************/
                

%% predicates_with_property(+Property,-Predicates) is det.
%
% Look up all Predicates with property Property, including atomic
% properties (e.g. dynamic, built_in) AND properties that are 
% functions (e.g. meta_predicate(Head)).

% GK, 5. April 2011: Extended the implementation to deal with unary
% functors. The combination of findall and setof is essentail for 
% this added functionality. The findall/3 call finds all results
%   (even if the arguments are free variables -- note that setof/3
%   would return results one by one in such a case, not a full list!). 
% Then the setof/3 call eliminates the duplicates from the results
% of findall/3. 
% DO NOT CHANGE, unless you consider yourself a Prolog expert.

% Property = undefined | built_in | dynamic | transparent | meta_predicate(_)    

% Look for undefined predicates only in the local context 
% (of the file whose editor has just been opened):
%predicates_with_property(undefined, FileName, Predicates) :-
%    !,
%    module_of_file(FileName,Module), 
%	findall(Name, predicate_name_with_property_(Module,Name,undefined), AllPredicateNames),
%	make_duplicate_free_string(AllPredicateNames,Predicates).

predicates_with_property(Property, _, Predicates) :-
    findall(Name, predicate_name_with_property_(_,Name,Property), AllPredicateNames),
	make_duplicate_free_string(AllPredicateNames,Predicates).


    	
predicate_name_with_property_(Module,Name,Property) :-
    current_module(Module),
    current_predicate(Module:Name/Arity),
	Name \= '[]',
	functor(Head,Name,Arity),
	predicate_property(Module:Head,Property).
	
make_duplicate_free_string(AllPredicateNames,Predicates) :-
    setof(Name, member(Name,AllPredicateNames), UniqueNames),
	sformat(S,'~w',[UniqueNames]),
	string_to_atom(S,Predicates).


% Below this line is apparently dead code. 
% TODO: 
% Check whether it is better than the one above.
% If yes use it, otherwise delete it. 

	
%% predicates_with_unary_property(+Property,?Predicates,?PropertyParams) is det.
%
% Look up all Predicates with the unary property Property, e.g. meta_predicate(Head) 
% The element at position i in Predicates is the name of a predicate that has  
% the property Property with the parameter at position i in PropertyParams.
%
% Author: GK, 5 April 2011
% TODO: Integrate into the editor the ability to show the params as tool tips,
% e.g. show the metaargument specifications of a metapredicate on mouse over.
predicates_with_unary_property(Property,Predicates,PropertyArguments) :-
	setof((Name,Arg),
	   predicate_name_with_unary_property_(Name,Property,Arg),
	   PredArgList),
	findall(Pred, member((Pred,_),PredArgList), AllProps),
	findall(Arg,  member((_,Arg), PredArgList), AllArgs),
	sformat(S1,'~w',[AllProps]),
	sformat(S2,'~w',[AllArgs]),
	string_to_atom(S1,Predicates),
	string_to_atom(S2,PropertyArguments).
	   	  
% helper
predicate_name_with_unary_property_(Name,Property,Arg) :-
    Property =.. [__F,Arg],
	predicate_property(_M:Head,Property),
	functor(Head,Name,_),
	Name \= '[]'.
