:- module(import_organizer,[
	imports_for_file/2
]).

:- use_module(pdt_common_pl('callgraph/pdt_call_graph')).
:- use_module(library(lists)).
:- use_module(library(filesex)).

imports_for_file(File, Imports) :-
	ensure_call_graph_generated,
	module_property(Module, file(File)),
	with_output_to(atom(Imports), write_imports(File, Module)).

write_imports(File, Module) :-
	referenced_module(File, Module, ReferencedModule, Predicates),
	path_to_module(File, ReferencedModule, Path),
	format(':- use_module(~W, [~n', [Path, [quotes(true)]]),
	write_predicates(Predicates),
	fail.
write_imports(_File, _Module).

write_predicates(Predicates) :-
	nth1(Pos, Predicates, Predicate),
	(	Pos == 1
	->	format('\t~w', [Predicate])
	;	format(',~n\t~w', [Predicate])
	),
	fail.
write_predicates(_Predicates) :-
	format('~n]).\n', []).



referenced_module(File, Module, ReferencedModule, Predicates) :-
	setof(N/A, referenced_predicate(File, Module, ReferencedModule, N, A), Predicates).

referenced_predicate(File, Module, ReferencedModule, N, A) :-
	calls(ReferencedModule, N0, A0, Module, CN, CA, _),
	(	functor(CH, CN, CA),
		predicate_property(Module:CH, multifile)
	->	calls_multifile(ReferencedModule, N, A, Module, CN, CA, File, _)
	;	N = N0,
		A = A0
	),
	Module \== ReferencedModule,
	functor(H, N, A),
	\+ predicate_property(ReferencedModule:H, built_in).

path_to_module(FileX, ReferencedModule, Path) :-
	module_property(ReferencedModule, file(File0)),
	file_name_extension(File, _, File0),
	(	directory_file_path(Directory, FileName, File),
		directory_file_path(Directory, _, FileX)
	->	Path = FileName
	;	file_search_path(library, LibPath),
		atomic(LibPath),
		atom_concat(LibPath, Rest, File)
	->	remove_leading_slash(Rest, Arg),
		Path = library(Arg)
	;	file_search_path(LibName, LibPath),
		atomic(LibPath),
		once(sub_atom(LibPath, _, _, _, pdt)),
		atom_concat(LibPath, Rest, File)
	->	remove_leading_slash(Rest, Arg),
		Path =.. [LibName, Arg]
	;	Path = File
	).

remove_leading_slash(Atom, AtomWithoutSlash) :-
	(	atom_concat('/', Rest, Atom)
	->	AtomWithoutSlash = Rest
	;	AtomWithoutSlash = Atom
	).