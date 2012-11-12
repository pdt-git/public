:- begin_tests(outline).

get_outline_data(host, ResultList) :-
	module_property(outline_demo, file(File)),
	findall(
		[Entity, Name, Arity, Line, PropertyList],
		pdt_search:find_definition_contained_in(File, Entity, _, _, Name, Arity, _, Line, PropertyList),
		ResultList
	).

get_outline_data(contributor, ResultList) :-
	module_property(outline_demo_multifile_contribution, file(File)),
	findall(
		[Entity, Name, Arity, Line, PropertyList],
		pdt_search:find_definition_contained_in(File, Entity, _, _, Name, Arity, _, Line, PropertyList),
		ResultList
	).

get_outline_data(user, ResultList) :-
	source_file(File),
	atom_concat(_, 'user_file.pl', File),
	findall(
		[Entity, Name, Arity, Line, PropertyList],
		pdt_search:find_definition_contained_in(File, Entity, _, _, Name, Arity, _, Line, PropertyList),
		ResultList
	).

test(all_clauses_found_host) :-
	get_outline_data(host, ResultList),
	member([outline_demo, likes,     2, 17, _], ResultList),
	member([outline_demo, likes,     2, 18, _], ResultList),
	member([outline_demo, likes,     2, 19, _], ResultList),
	member([outline_demo, likes,     2, 20, _], ResultList),
	member([outline_demo, likes,     2, 32, _], ResultList),
	member([outline_demo, likes,     2,  8, _], ResultList),
	member([outline_demo, pair,      2, 22, _], ResultList),
	member([outline_demo, single,    1, 26, _], ResultList),
	member([outline_demo, add_likes, 2, 29, _], ResultList),
	!.

test(all_clauses_found_contributor) :-
	get_outline_data(contributor, ResultList),
	member([outline_demo,                        likes,         2,  8, _], ResultList),
	member([outline_demo,                        likes,         2, 17, _], ResultList),
	member([outline_demo,                        likes,         2, 18, _], ResultList),
	member([outline_demo,                        likes,         2, 19, _], ResultList),
	member([outline_demo,                        likes,         2, 20, _], ResultList),
	member([outline_demo,                        likes,         2, 32, _], ResultList),
	member([outline_demo_multifile_contribution, own_predicate, 0, 10, _], ResultList),
	!.

test(predicate_properties) :-
	get_outline_data(host, ResultList),
	forall((
		member([outline_demo, likes, 2, _, PropertyList], ResultList)
	),(
		member(dynamic, PropertyList),
		member(multifile, PropertyList),
		member(exported, PropertyList),
		member(number_of_clauses(6), PropertyList)
	)).

test(multifile_from_other) :-
	get_outline_data(host, ResultList),
	
	member([outline_demo, likes, 2, 8, PropertyList], ResultList),
	member(defining_file(File), PropertyList),
	atom_concat(_, 'outline_demo_multifile_contribution.pl', File),
	!.

test(multifile_for_other) :-
	get_outline_data(contributor, ResultList),
	
	member([outline_demo, likes, 2, 8, PropertyList], ResultList),
	member(for(outline_demo), PropertyList),
	
	!. 

test(user_module) :-
	get_outline_data(user, ResultList),
	member([user, predicate_one, 0, 1, _], ResultList),
	member([user, message_hook,  3, 5, P], ResultList),
	member(number_of_clauses(N), P),
	N > 1,
	!.

:- end_tests(outline).





:- begin_tests(search).

get_editor_data(from_super, ResultList) :-
	module_property(search_demo, file(File)),
	findall(
		[Functor,Arity,DefiningModule,Visibility],
		pdt_search:find_definitions_categorized(File,0,abc(_),Functor,Arity,_Module,_DeclOrDef,DefiningModule,_File,_Line,_PropertyList,Visibility,true),
		ResultList
	).

get_editor_data(from_sub, ResultList) :-
	module_property(search_demo_sub, file(File)),
	findall(
		[Functor,Arity,DefiningModule,Visibility],
		pdt_search:find_definitions_categorized(File,0,abc(_),Functor,Arity,_Module,_DeclOrDef,DefiningModule,_File,_Line,_PropertyList,Visibility,true),
		ResultList
	).

test(from_editor_local) :-
	get_editor_data(from_super, ResultList1),
	member([abc, 1, search_demo, local], ResultList1),
	get_editor_data(from_sub, ResultList2),
	member([abc, 1, search_demo_sub, local], ResultList2),
	!.

test(from_editor_super) :-
	get_editor_data(from_sub, ResultList),
	member([abc, 1, search_demo, super], ResultList),
	!.

test(from_editor_sub) :-
	get_editor_data(from_super, ResultList),
	member([abc, 1, search_demo_sub, sub], ResultList),
	!.

test(from_editor_invisible) :-
	get_editor_data(from_super, ResultList1),
	member([abc, 1, other_module, invisible], ResultList1),
	get_editor_data(from_sub, ResultList2),
	member([abc, 1, other_module, invisible], ResultList2),
	!.

test(search_global_pred_defs_exact_arity) :-
	findall(
		[Functor,Arity,DefiningModule],
		pdt_search:find_definitions_categorized('abc'/1,true, DefiningModule, Functor,Arity,_DeclOrDef,_File,_Line,_PropertyList),
		ResultList
	),
	% the search gives lots of duplicate results
	forall(
		member(Result, ResultList),
		member(Result, [
			[abc, 1, search_demo],
			[abc, 1, search_demo_sub],
			[abc, 1, other_module]
		])
	).
	
test(search_global_pred_defs_exact_any) :-
	findall(
		[Functor,Arity,DefiningModule],
		pdt_search:find_definitions_categorized('exact_search_predicate',true,DefiningModule, Functor,Arity,_DeclOrDef,_File,_Line,_PropertyList),
		ResultList
	),
	% the search gives lots of duplicate results
	forall(
		member(Result, ResultList),
		member(Result, [
			[exact_search_predicate, 0, exact_search],
			[exact_search_predicate, 1, exact_search],
			[exact_search_predicate, 2, exact_search]
		])
	).

test(search_global_pred_defs_substring) :- 
	findall(
		[Functor, Arity, Module],
		pdt_search:find_definitions_categorized('search_string',false,Module,Functor,Arity,_DeclOrDef,_File,_Line,_PropertyList),
		ResultList
	),
	% there must be 4 results
	length(ResultList, 4),
	member([search_string,               3, not_exact_search], ResultList),
	member([search_string_at_the_front,  1, not_exact_search], ResultList),
	member([in_the_search_string_middle, 0, not_exact_search], ResultList),
	member([at_the_end_search_string,    5, not_exact_search], ResultList),
	!.
	
test(search_global_pred_refs_exact) :-
	findall(
		[RefModule,RefName,RefArity,Nth],
		pdt_search:find_reference_to('abc',1,_File,_Module,true,RefModule,RefName,RefArity,_RefFile,_RefLine,Nth,_Kind,_PropertyList),
		ResultList
	),
	member([search_demo, p, 1, 1],ResultList),
	member([search_demo, p, 1, 2],ResultList),
	!.

% TODO: write test
%test(search_global_pred_refs_substring) :- fail.

test(search_global_module_defs_exact) :- 
	findall(
		[File, Line, Module],
		pdt_search:find_module_definition('search_demo_sub',true,File,Line,Module),
		ResultList
	),
	ResultList = [[File, 4, search_demo_sub]],
	atom_concat(_, 'search/search_demo_sub.pl', File).
	
test(search_global_module_defs_substring) :- 
	findall(
		[File, Line, Module],
		pdt_search:find_module_definition('search_demo',false,File,Line,Module),
		ResultList
	),
	length(ResultList, 2),
	member([File1, 4, search_demo_sub], ResultList),
	atom_concat(_, 'search/search_demo_sub.pl', File1),
	member([File2, 1, search_demo], ResultList),
	atom_concat(_, 'search/search_demo.pl', File2),
	!.

test(search_global_module_refs_exact) :-
	findall(
		[File, Line, ReferencingModule, RefName, RefArity],
		pdt_search:find_module_reference(search_demo, true, File, Line, ReferencingModule, RefName, RefArity, _PropertyList),
		ResultList
	),
	member([File1, 1, system, load_files, 2], ResultList),
	atom_concat(_, 'search/load.pl', File1),
	member([File2, 6, system, load_files, 2], ResultList),
	atom_concat(_, 'search/search_demo_sub.pl', File2),
	member([_, _, search_demo_sub, q, 0], ResultList),
	!.
	

% TODO: write test
%test(search_global_module_refs_substring) :- fail.

test(primary_definition_unique) :-
	module_property(other_module, file(TestFile)),
	module_property(lists, file(ListsFile)),
	pdt_search:find_primary_definition_visible_in(TestFile,_SelectedLine,'member(_, [_,_])',_ReferencedModule,MainFile,_FirstLine,ResultKind),
	MainFile == ListsFile,
	ResultKind == single,
	!.

test(primary_definition_ambiguous) :-
	module_property(outline_demo_multifile_contribution, file(TestFile)),
	module_property(outline_demo, file(OutlineDemoFile)),
	pdt_search:find_primary_definition_visible_in(TestFile,_SelectedLine,'outline_demo:likes(jack_torrance, the_overlook_hotel)',_ReferencedModule,MainFile,FirstLine,ResultKind),
	ResultKind == (multifile),
	(	MainFile == TestFile,
		FirstLine == 8
	;	MainFile == OutlineDemoFile,
		FirstLine == 17
	),
	!.

test(primary_definition_loading_directive_complete) :-
	module_property(other_module, file(TestFile)),
	module_property(lists, file(ListsFile)),
	pdt_search:find_primary_definition_visible_in(TestFile,_SelectedLine,'use_module(library(lists))',_ReferencedModule,MainFile,_FirstLine,_MultifileResult),
	ListsFile == MainFile,
	!.

test(primary_definition_loading_directive_only_alias) :-
	module_property(other_module, file(TestFile)),
	module_property(lists, file(ListsFile)),
	pdt_search:find_primary_definition_visible_in(TestFile,_SelectedLine,'library(lists)',_ReferencedModule,MainFile,_FirstLine,_MultifileResult),
	ListsFile == MainFile,
	!.


test(completion_predicate_with_doc) :-
	module_property(completion_demo, file(TestFile)),
	pdt_search:find_pred(TestFile,'my_pre',_,Name,Arity,_Exported,_Builtin,Help),
	Name == my_predicate_with_documentation,
	Arity == 1,
	sub_atom(Help, _, _, _, 'True if'),
	sub_atom(Help, _, _, _, 'can is an atom'),
	!.

test(completion_predicate_without_doc) :-
	module_property(completion_demo, file(TestFile)),
	pdt_search:find_pred(TestFile,'my_pre',_,Name,Arity,_Exported,_Builtin,Help),
	Name == my_predicate_without_documentation,
	Arity == 1,
	Help == nodoc,
	!.

test(completion_predicate_with_module) :-
	module_property(completion_demo, file(TestFile)),
	findall(
		[Name, Arity],
		pdt_search:find_pred(TestFile,'s','not_exact_search',Name,Arity,_Exported,_Builtin,_Help),
		ResultList
	),
	member([search_string, 3], ResultList),
	member([search_string_at_the_front, 1], ResultList),
	!.

test(completion_module) :-
	module_property(completion_demo, file(TestFile)),
	findall(
		[Name, Kind],
		pdt_search:find_pred_for_editor_completion(TestFile,'search_d',_,Name,_Arity,_Exported,_Builtin,_Help,Kind),
		ResultList
	),
	member([search_demo, module], ResultList),
	member([search_demo_sub, module], ResultList),
	!.
	
:- end_tests(search).
 