:- module(parser,
	[	pdt_parse/1,
		pdt_forget/1
	]
).
:- use_module(library('prolog_source')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('spike/pef_base')).

:- pdt_define_context(parse_cx(file_ref,toplevel_ref,term,expanded)).




pdt_parse(Spec):-
    my_read(Spec).

pdt_forget(Spec):-
    my_forget(Spec).


my_forget(Spec):-
    pdt_file_ref(Spec,FileRef),
    
	forall(
		pef_module_definition_query([id=Id,file_ref=FileRef]),
		pef_property_retractall([id=Id])
	),
	pef_module_definition_retractall([file_ref=FileRef]),	
    
    forall(
		pef_op_definition_query([id=Id,file_ref=FileRef]),
		pef_property_retractall([id=Id])
	),
	pef_op_definition_retractall([file_ref=FileRef]),	
	
	forall(
		pef_file_dependency_query([id=Id,file_ref=FileRef]),
		pef_property_retractall([id=Id])
	),
	pef_file_dependency_retractall([file_ref=FileRef]),
	
	forall(
		pef_problem_query([id=Id,type=parser,file_ref=FileRef]),
		pef_property_retractall([id=Id])
	),
	pef_problem_retractall([file_ref=FileRef]),
    
    atom_concat(terms_,FileRef,Key),
    forall(
    	recorded(Key,_,Ref),
    	erase(Ref)
    ).

my_read(Spec):-
    pdt_file_spec(Spec,F),
    %TODO: check timestamp
    do_read(F).

do_read(F):-
    prolog_open_source(F,In),
    call_cleanup(
    	do_read(F,In),
    	prolog_close_source(In)
    ).

do_read(F,In):-
    parse_cx_new(Cx),
    pdt_file_ref(F,Ref),
    parse_cx_file_ref(Cx,Ref),    
    atom_concat(terms_,Ref,Key),
    repeat,
    	catch(
    		prolog_read_source_term(In,Term,Expanded,[]),    	
    		Error,
    		writeln(Error) %TODO: add syntax error pef
    	),
    	var(Error),
    	pef_toplevel_recordz(Key,[file_ref=Ref,term=Term,expanded=Expanded],TlRef),
    	parse_cx_get(Cx,[term=Term,expanded=Expanded,toplevel_ref=TlRef]),
    	preprocess(Expanded,Cx),
    	Term==end_of_file,
    !.

	    
preprocess((:-module(Name,_Exports)), Cx):-
    !,
    process_module_definition(Name,Cx).
preprocess((:-op(Priority,Type,Op)), Cx):-
    !,
    process_op(Priority,Type,Op, Cx).
preprocess((:-Term), Cx):-
    find_file_refs(Term,Files),
    !,
    forall(member(F,Files),process_inclusion(F,Cx)).
preprocess(_Term, _Cx).


process_module_definition(Name,Cx):-
    pef_reserve_id(pef_module_definition,Id),
    parse_cx_get(Cx,[file_ref=FileRef,toplevel_ref=TLRef]),
    pef_module_definition_assert([id=Id,name=Name,file_ref=FileRef,toplevel_ref=TLRef]).


process_inclusion(F,Cx):-
	format("resolving ~w~n", [F]),
	parse_cx_file_ref(Cx,MyRef),
	pdt_file_spec(file_ref(MyRef),MyFile),
	file_directory_name(MyFile,MyDir),
	pdt_file_spec(F,MyDir,File),
	!,
	format("including ~w~n", [File]),
	pdt_parse(File),
	pdt_file_ref(File,Ref),

	(	pef_module_definition_query([file_ref=Ref],Module)
	->  process_module_inclusion(Module,Cx)
	;	process_file_inclusion(Ref,Cx)
	).
process_inclusion(F,_Cx):-
    format("could not resolve: ~w~n",[F]). %TODO: add file not found error.

process_module_inclusion(Module,Cx):-
    pef_module_definition_toplevel_ref(Module,Ref),
    pef_toplevel_recorded(_,[expanded=(:- module(_,Exports))],Ref),
    forall(	
    	member(op(Pr,Tp,Nm),Exports),
    	my_push_op(Pr,Tp,Nm,Cx)
    ).

process_file_inclusion(FileRef,Cx):-
	forall(
		pef_op_definition_query([file_ref=FileRef,priority=Pr,type=Tp,name=Nm]),
		my_push_op(Pr,Tp,Nm,Cx)
	).

process_op(Priority,Type,Op, Cx):-
    pef_reserve_id(pef_op_definition,Id),
    parse_cx_get(Cx,[file_ref=FileRef,toplevel_ref=TLRef]),
    pef_op_definition_assert([id=Id,priority=Priority,type=Type,name=Op,file_ref=FileRef,toplevel_ref=TLRef]).
    

find_file_refs([H|T],[H|T]).
find_file_refs(load_files([H|T],_),[H|T]).
find_file_refs(load_files(H,_),[H]):-
    \+ is_list(H).
find_file_refs(consult([H|T]),[H|T]).
find_file_refs(consult(H),[H]):-
    \+ is_list(H).
find_file_refs(ensure_loaded([H|T]),[H|T]).
find_file_refs(ensure_loaded(H),[H]):-
    \+ is_list(H).
find_file_refs(include(H),[H]):-
    \+ is_list(H).    
find_file_refs(use_module([H|T]),[H|T]).
find_file_refs(use_module(H),[H]):-
    \+ is_list(H).
find_file_refs(use_module([H|T],_),[H|T]).
find_file_refs(use_module(H),_,[H]):-
    \+ is_list(H).


my_push_op(Pr,Tp,Nm,_Cx):-
    '$set_source_module'(SM, SM),
     push_op(Pr,Tp,SM:Nm).
	    