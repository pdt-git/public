:- module(parser,
	[]
).
:- use_module(library('prolog_source')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).


:- pdt_define_context(parse_cx(file,toplevel,term,expanded)).

pdt_builder:build_hook(parse(AbsFile)):-
    parser:my_build_hook(AbsFile).
my_build_hook(AbsFile):-    
	pdt_forget(AbsFile),
	(	exists_file(AbsFile)
   	->	pdt_with_targets([file(AbsFile)],pdt_parse(AbsFile))
   	;	true
   	).
pdt_builder:invalidate_hook(file(AbsFile)):-
    pdt_invalidate_target(parse(AbsFile)).
pdt_builder:invalidate_hook(parse(AbsFile)):-
    parser:
    (	get_pef_file(AbsFile,DepRef),
	    forall(
	    	pef_file_dependency_query([dependency=DepRef,depending=FileRef] ),
	    	(	get_pef_file(File,FileRef),
	    		pdt_invalidate_target(parse(File))
	    	)
	    )
	).

pdt_parse(Spec):-
    my_read(Spec).

pdt_forget(Spec):-
    my_forget(Spec).


my_forget(Spec):-
	pdt_file_spec(Spec,AbsFile),
    get_pef_file(AbsFile,FileRef),
    
	forall(
		pef_module_definition_query([id=Id,file=FileRef]),
		(	pef_property_retractall([pef=Id]),
			pef_exports_retractall([module=Id])
		)
	),
	pef_module_definition_retractall([file=FileRef]),	
    
    forall(
		pef_op_definition_query([id=Id,file=FileRef]),
		pef_property_retractall([pef=Id])
	),
	pef_op_definition_retractall([file=FileRef]),	
	
	forall(
		pef_file_dependency_query([id=Id,depending=FileRef]),
		pef_property_retractall([pef=Id])
	),
	pef_file_dependency_retractall([depending=FileRef]),
	
	/*forall(
		pef_problem_query([id=Id,type=parser,file_ref=FileRef]),
		pef_property_retractall([pef=Id])
	),
	pef_problem_retractall([file_ref=FileRef]),
    */
    pef_toplevel_retractall([file=FileRef]).

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
    get_pef_file(F,Ref),
    parse_cx_file(Cx,Ref),    

    repeat,
    	catch(
    		prolog_read_source_term(In,Term,Expanded,[variable_names(VarNames),singletons(Singletons),subterm_positions(Positions)]),    	
    		Error,
    		debug(parser(todo),"TODO: add an error marker for ~w.~n",[Error])
    	),
    	var(Error),
    	pef_reserve_id(pef_toplevel,TID),
    	pef_toplevel_assert([id=TID,file=Ref,term=Term,expanded=Expanded,varnames=VarNames,singletons=Singletons,positions=Positions]),
    	parse_cx_get(Cx,[term=Term,expanded=Expanded,toplevel=TID]),
    	preprocess(Expanded,Cx),
    	Term==end_of_file,
    !.

	    
preprocess((:-module(Name,Exports)), Cx):-
    !,
    process_module_definition(Name,Exports,Cx).
preprocess((:-op(Priority,Type,Op)), Cx):-
    !,
    process_op(Priority,Type,Op, Cx).
preprocess((:-Term), Cx):-
    find_file_refs(Term,Files),
    !,
    forall(member(F,Files),process_inclusion(F,Cx)).
preprocess(_Term, _Cx).


process_module_definition(Name,Exports,Cx):-
    pef_reserve_id(pef_module_definition,Id),
    parse_cx_get(Cx,[file=FileRef,toplevel=TLRef]),
    pef_module_definition_assert([id=Id,name=Name,file=FileRef,toplevel=TLRef]),
    forall(
    	member(Export,Exports),
    	pef_exports_assert([module=Id,signature=Export])
    ).


process_inclusion(F,Cx):-
	debug(parser(debug),"resolving ~w~n", [F]),
	parse_cx_file(Cx,MyRef),
	parse_cx_toplevel(Cx,TlRef),
%	pdt_file_spec(file_ref(MyRef),MyFile),
	get_pef_file(MyFile,MyRef),
	file_directory_name(MyFile,MyDir),
	pdt_file_spec(F,MyDir,File),
	!,
	get_pef_file(File,Ref),
	pef_reserve_id(pef_file_dependency,Id),
	pef_file_dependency_assert([id=Id,depending=MyRef,toplevel=TlRef,dependency=Ref]),
	catch(
		do_inclusion(File,Cx),
		error(cycle(T)),
		debug(parser(todo),"TODO: add a warning about dependency cycle: ~w~n",[T])
	).
process_inclusion(F,_Cx):-
    debug(parser(todo),"TODO add error marker - could not resolve: ~w~n",[F]). 




do_inclusion(File,Cx):-	
	pdt_with_targets([parse(File)],
		(	get_pef_file(File,Ref),
			(	pef_module_definition_query([file=Ref],Module)
			->  process_module_inclusion(Module,Cx)
			;	process_file_inclusion(Ref,Cx)
			)
		)		
	).


process_module_inclusion(Module,Cx):-
    forall(	
    	pef_exports_query([module=Module,signature=op(Pr,Tp,Nm)]),
    	my_push_op(Pr,Tp,Nm,Cx)
    ).

process_file_inclusion(FileRef,Cx):-
	forall(
		pef_op_definition_query([file=FileRef,priority=Pr,type=Tp,name=Nm]),
		my_push_op(Pr,Tp,Nm,Cx)
	).

process_op(Priority,Type,Op, Cx):-
    pef_reserve_id(pef_op_definition,Id),
    parse_cx_get(Cx,[file=FileRef,toplevel=TLRef]),
    pef_op_definition_assert([id=Id,priority=Priority,type=Type,name=Op,file=FileRef,toplevel=TLRef]).
    

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
	    