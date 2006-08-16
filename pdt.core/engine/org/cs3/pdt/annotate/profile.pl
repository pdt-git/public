lala:-
    findall(A, pdt_file_annotation(A,_),Files),
    profile(build(Files)).

lele:-
    findall(A, pdt_file_annotation(A,_),Files),
    build(Files).


build([]).
build([File|Files]):-
	pdt_forget_annotation(File),
	pdt_ensure_annotated(File), 
	build(Files).

lulu:-
	profile( 
		build(
	['z:/eclipse/runtime-new_configuration/work/tester/pefs.pl']
		)
	). 	 