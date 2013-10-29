:- module(dependency_matrix,[
	write_wiki/0,
	write_wiki/1
]).

:- use_module(dependencies).
:- use_module(library(lists)).

category(outline, 'Outline', [
	pdt_search:find_definition_contained_in/10,
	pdt_search:loaded_file/1
]).

category(definition_search, 'Search definitions', [
	pdt_search:find_definitions_categorized/13,
	pdt_search:find_definitions_categorized/9,
	pdt_search:find_entity_definition/5,
	pdt_search:find_primary_definition_visible_in/7,
	pdt_search:find_alternative_predicates/7
]).

category(reference_search, 'Search references', [
	pdt_search:find_reference_to/13,
	pdt_search:find_module_reference/8
]).

category(metapredicate_search, 'Search meta predicates', [
	pdt_call_analysis:find_undeclared_meta_predicate/10
]).

category(completion, 'Search completions', [
	pdt_search:find_completion/12
]).

category(graph_creation, 'Create graphs', [
	pl_to_graphML:write_focus_to_graphML/3,
	pl_to_graphML:write_global_to_graphML/2,
	pl_to_graphML:write_dependencies_to_graphML/3
]).

category(consult_state, 'File consult state', [
	source_files:pdt_source_file/2
]).

category(console, 'Console', [
	pdt_console_server:pdt_start_console_server/2,
	pdt_console_server:pdt_stop_console_server/0
]).

write_wiki :-
	(	exists_directory('c:/users/user/documents/neuer ordner/workspaces/runtime-w1-1/matrix')
	->	write_wiki('c:/users/user/documents/neuer ordner/workspaces/runtime-w1-1/matrix/deps.txt')
	;	throw(error('Specifiy a valid file!'))
	).

write_wiki(File) :-
	with_mutex(write_wiki, setup_call_cleanup(open(File, write, S), write_wiki_(S), close(S))).

:- dynamic(dep_cache/2).

write_wiki_(S) :-
	retractall(dep_cache(_, _)),
	% create table head and derive & store dependencies
	write(S, '^  Predicate  ^'),
	category(Category, Label, Predicates),
	format(S, '  ~w  ((~w))^', [Label, Predicates]),
	setof(Ref, Predicate^(
		member(Predicate, Predicates),
		dep(Predicate, Ref)
		), Refs),
	assertz(dep_cache(Category, Refs)),
	fail.
write_wiki_(S) :-
	nl(S),
	% collect all predicates
	setof(Ref, Category^CategoryRefs^(dep_cache(Category, CategoryRefs), member(Ref, CategoryRefs)), AllRefs),
	assertz(dep_cache(all, AllRefs)),
	fail.
write_wiki_(S) :-
	% write line for each predicate
	dep_cache(all, AllRefs),
	member(Ref, AllRefs),
	write_wiki_line(S, Ref),
	fail.
write_wiki_(_S).

write_wiki_line(S, Ref) :-
	format(S, '| ~w  |', [Ref]),
	category(Category, _, _),
	dep_cache(Category, CategoryRefs),
	(	memberchk(Ref, CategoryRefs)
	->	write(S, '  x  |')
	;	write(S, '     |')
	),
	fail.
write_wiki_line(S, _Ref) :-
	nl(S).