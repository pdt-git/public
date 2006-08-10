lala:-
    findall(A, current_file_annotation(A,_),Files),
    profile(build(Files)).

build([]).
build([File|Files]):-
	forget_file_annotation(File),
	ensure_annotated(File),
	build(Files).

lulu:-
	profile(
		build(
	['z:/eclipse/runtime-new_configuration/work/tester/pefs.pl']
		)
	).	