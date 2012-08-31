:- begin_tests(outline).

:- end_tests(outline).





:- begin_tests(search).

% TODO: fails (unique filtering)
test(search_global_pred_defs_exact_arity) :-
	findall(
	OrigFile,
	pdt_search:find_definitions_categorized(OrigFile,0,'abc'/1,Functor,Arity,Module,DeclOrDef,DefiningModule,File,Line,PropertyList,Visibility,true),
	ResultList),
	% there must only be 1 result
	length(ResultList,1).
	% TODO: check for type of result
	
% TODO: fails (unique filtering)
test(search_global_pred_defs_exact_any) :-
	findall(
		(Arity),
		pdt_search:find_definitions_categorized(OrigFile,0,'exact_search_predicate'/(-1),Functor,Arity,Module,DeclOrDef,DefiningModule,File,Line,PropertyList,Visibility,true),
		ResultList),
	% there must be 3 results
	length(ResultList, 3),
	member(0,ResultList),
	member(1,ResultList),
	member(2,ResultList).
		
	
test(search_global_pred_defs_substring) :- 
	findall(
		(Functor, Arity, Module),
		pdt_search:find_definitions_categorized(_OrigFile,0,'search_string'/(-1),Functor,Arity,Module,_DeclOrDef,_DefiningModule,_File,_Line,_PropertyList,_Visibility,false),
		ResultList),
	% there must be 4 results
	length(ResultList, 4),
	member((search_string, 3), ResultList),
	member((search_string_at_the_front, 1), ResultList),
	member((in_the_search_string_middle, 0), ResultList),
	member((at_the_end_search_string, 5), ResultList).
	
test(search_global_pred_refs_exact) :-
	findall(
	(RefFile, RefLine),
	pdt_search:find_reference_to('abc',1,_File,_Module,true,_RefModule,_RefName,_RefArity,RefFile,RefLine,_Nth,_Kind,_PropertyList),
	ResultList
	),
	length(ResultList,2),
	member((RefFile,18),ResultList),
	atom_concat(_,'search/search_demo.pl',RefFile),
	member((RefFile,21),ResultList),
	atom_concat(_,'search/search_demo.pl',RefFile).

% TODO: write test
test(search_global_pred_refs_substring) :- fail.

test(search_global_module_defs_exact) :- 
	findall(
		(File, Line, Module),
		pdt_search:find_module_definition('search_demo_sub',true,File,Line,Module),
		ResultList),
	ResultList = [(File, 4, search_demo_sub)],
	atom_concat(_, 'search/search_demo_sub.pl', File).
	
test(search_global_module_defs_substring) :- 
	findall(
		(File, Line, Module),
		pdt_search:find_module_definition('search_demo',false,File,Line,Module),
		ResultList),
	length(ResultList, 2),
	member((File1, 4, search_demo_sub), ResultList),
	atom_concat(_, 'search/search_demo_sub.pl', File1),
	member((File2, 1, search_demo), ResultList),
	atom_concat(_, 'search/search_demo.pl', File2).

% TODO: write test
test(search_global_module_refs_exact) :- fail.

% TODO: write test
test(search_global_module_refs_substring) :- fail.
	
:- end_tests(search).
 