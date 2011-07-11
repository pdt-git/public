:- consult(pdt_runtime_builder_analyzer('meta_pred_toplevel.pl')).

smell_marker_pdt(Name, Description, QuickfixDescription, QuickfixAction, File, Start, Length) :-
	smell_description(Name, Description, QuickfixDescription),
	smell(Name, File, Start, Length, QuickfixAction).
	
smell_description('MissingMetaPredicateDeclaration', SmellDescription, QuickfixDescription):-
	SmellDescription = 'Missing meta-predicate declation', 
	QuickfixDescription = 'Add missing meta-predicate declaration'.
	
%    
%QuickfixAction ist der Text, der unmittelbar vor dem Smell eingetragen werden muss (Zeilenumbruch muss mit angegeben werden)
smell('MissdeclaredMetaPredicateDeclaration', File, Offset, 0, QuickfixAction) :-
    find_undeclared_meta_predicates_position(File, Offset, Spec),
    format(atom(QuickfixAction),':- meta_predicate(~w).~n', [Spec]).
    
    
    
    
% Dummy: Just for testing
%smell_description('hallo', 'beschreibung', 'fix beschreibung').
%smell('hallo', 'l:/work/noth/workspaces/runtime generics/dummdidumm/pl/heididdeliho.pl', 0, 5, '% fix itself\n').