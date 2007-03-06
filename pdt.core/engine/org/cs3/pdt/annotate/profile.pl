:- use_module(pdt_annotator).
:- use_module(op_annotator).
:- use_module(fileref_annotator).
:- pdt_annotator_cache:pdt_set_preference_value(cache_dir,'c:/temp').


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

the_file('z:/jhotdraw6.1-pefs.pl').

redirect(File, Goal):-
    open(File,write,Stream),
    with_output_to(Stream,Goal),
    close(Stream).
    	

lulu:-
	profile( 
		build(
	['z:/eclipse/runtime-new_configuration/work/tester/pefs.pl']
		)
	). 	 