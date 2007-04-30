:- module(pdt_parser,
	[	pdt_parse/1,
		pdt_forget/1
	]
).
:- use_module(library('prolog_source')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('spike/pef_base')).

:- pdt_define_context(parse_cx(file_ref,toplevel_ref,term,expanded)).
:- pdt_define_context(toplevel_record(file_ref,term,expanded)).



pdt_parse(Spec):-
    my_read(Spec).

pdt_forget(Spec):-
    my_forget(Spec).


my_forget(_Spec):-
    true. %TODO: forget module and op definitions, erase toplevel records, retract error pefs.

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
    	parse_cx_get(Cx,[term=Term,expanded=Expanded]),
    	record_toplevel(Key,Cx),    	
    	preprocess(Expanded,Cx),
    	Term==end_of_file,
    !.


record_toplevel(Key,Cx):-
    toplevel_record_new(Record),
    parse_cx_get(Cx,[file_ref=FileRef,toplevel_ref=TlRef,term=Term,expanded=Expanded]),
    toplevel_record_get(Record,[file_ref=FileRef,term=Term,expanded=Expanded]),
    recordz(Key,Record,TlRef).


	    
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
	pdt_file_spec(F,File),
	!,
	pdt_file_ref(File,Ref),
	format("including ~w~n", [File]),
	(	pef_module_definition_query([file_ref=Ref],Module)
	->  process_module_inclusion(Module,Cx)
	;	process_file_inclusion(Ref,Cx)
	).
process_inclusion(F,_Cx):-
    writeln(blablabla),
    format("could not resolve: ~w~n",[F]). %TODO: add file not found error.

process_module_inclusion(Module,_Cx):-
    pef_module_toplevel_ref(Module,Ref),
    recorded(_,(:- module(_,Exports)),Ref),
    forall(	
    	member(op(Pr,Tp,Nm),Exports),
    	push_op(Pr,Tp,Nm)
    ).

process_file_inclusion(FileRef,_Cx):-
	forall(
		pef_op_definition_query([file_ref=FileRef,priority=Pr,type=Tp,name=Nm]),
		push_op(Pr,Tp,Nm)
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
    