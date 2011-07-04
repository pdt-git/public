smell_marker(Name, Description, QuickfixDescription, QuickfixAction, File, Start, Length) :-
	smell_description(Name, Description, QuickfixDescription),
	smell(Name, File, Start, Length, QuickfixAction).
	
%smell_description('SmellName', 'SmellDescription', 'QuickfixDescription').
%    
%QuickfixAction ist der Text, der unmittelbar vor dem Smell eingetragen werden muss (Zeilenumbruch muss mit angegeben werden)
%smell('SmellName', File, Offset, Length, QuickfixAction) :-
    
% Dummy: Just for testing
%smell_description('hallo', 'beschreibung', 'fix beschreibung').
%smell('hallo', 'l:/work/noth/workspaces/runtime generics/dummdidumm/pl/heididdeliho.pl', 0, 5, '% fix itself\n').