:- module(parse,[]).
:- use_module(library('prolog_source')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).
:- use_module(library('facade/pdt_workspace')).


:- pdt_define_context(parse_cx(file,toplevel,term)).



pdt_builder:build_hook(parse(Resource)):-
    pdt_request_target(Resource),
	(	Resource=file(AbsFile)    
	->  parse:my_build_hook(AbsFile)    
	;	forall(pdt_contains(Resource,Element),pdt_request_target(parse(Element)))
	).

   
pdt_builder:target_container(parse(Resource),parse(Container)):-
    pdt_builder:target_container(Resource,Container).
pdt_builder:target_file(parse(file(F)),F).
pdt_builder:target_file(parse(directory(F,_,_)),F).
pdt_builder:target_mutable(parse(workspace),true).
pdt_builder:target_mutable(parse(project(_)),true).


my_build_hook(AbsFile):-    
	%pdt_forget(AbsFile),
	(	exists_file(AbsFile)
   	->	pdt_request_target(file(AbsFile)),
   		pdt_parse(AbsFile)
   	;	true
   	).

pdt_parse(Spec):-
    my_read(Spec).
/*
pdt_forget(Spec):-
   % my_forget(Spec).
	pdt_file_spec(Spec,AbsFile),
    get_pef_file(AbsFile,FID),
    pef_toplevel_cleanupall([file=FID]),
    pef_syntax_error_cleanupall([file=FID]).

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
*/
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
    		prolog_read_source_term(In,Original,Expanded,
    			[	variable_names(VarNames),
    				singletons(Singletons),
    				subterm_positions(Positions),
    				comments(Comments),
    				double_quotes(string)/*,
    				module(parse)*/
    			]
    		),    	
    		Error,
    		add_error_marker(Error,Cx)
    		%debug(parse(todo),"TODO: add an error marker for ~w.~n",[Error])
    	),
    	var(Error),
    	
    	pef_reserve_id(pef_toplevel,OrigID),   
    	(	pef_toplevel_assert([id=OrigID,file=Ref,term=Original,varnames=VarNames,singletons=Singletons,positions=Positions]),    	
    		parse_cx_get(Cx,[term=Original,toplevel=OrigID]),
    		Term=Original,
    		add_comments(Comments,Cx)
    	;	Expanded\==Original,
    		(	is_list(Expanded)
    		->	Terms=Expanded
    		;	Terms=[Expanded]
    		),
    		member(Term,Terms),
    		pef_reserve_id(pef_toplevel,ExpID),  
    		gen_varnames(Term,GenVarnames),  		
    		pef_toplevel_assert([id=ExpID,file=Ref,term=Term,varnames=GenVarnames,singletons=[],positions=none]),
    		pef_term_expansion_assert([source=OrigID,expanded=ExpID]),    	
    		parse_cx_get(Cx,[term=Term,toplevel=ExpID])
    	), 
    	preprocess(Term,Cx),
    	
    	Term==end_of_file,
    !.

gen_varnames(Term,VarNames):-
    copy_term(Term,Term2),
    numbervars(Term2,0,_),
    unifiable(Term,Term2,VarNames0),
    reverse_varnames(VarNames0,VarNames).

reverse_varnames([],[]).
reverse_varnames([X=Y|VarNames0],[Y=X|VarNames]):-
    reverse_varnames(VarNames0,VarNames).

add_comments([],_Cx).
add_comments([StreamPos-Text|Comments],Cx):-
    parse_cx_file(Cx,File),
    parse_cx_toplevel(Cx,Toplevel),    
    pef_reserve_id(pef_comment,Id),
    pef_comment_assert([id=Id,toplevel=Toplevel,file=File,text=Text]),
    pef_property_assert([pef=Id,key=position,value=StreamPos]),
    add_comments(Comments,Cx).
    
add_error_marker(Error,Cx):-
    (	\+ \+ Error = error(syntax_error(_),_)
    ->	parse_cx_file(Cx,File),
    	pef_reserve_id(pef_syntax_error,ID),
    	pef_syntax_error_assert([id=ID,file=File,error=Error])
    ;	debug(parse(warning),"not a syntax error??: ~w",[Error])
    ).
	    
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
	debug(parse(debug),"resolving ~w~n", [F]),
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
		error(cycle(Toplevel)),
		pef_parser_dependency_cycle_assert([action=parse(file(File)),toplevel=Toplevel])
	).
process_inclusion(F,Cx):-
    pef_reserve_id(pef_file_not_found,Id),
    parse_cx_toplevel(Cx,Tl),
    pef_file_not_found_assert([id=Id,file_spec=F,toplevel=Tl]).
     




do_inclusion(File,Cx):-	
	pdt_request_target(parse(file(File))),
	get_pef_file(File,Ref),
	(	pef_module_definition_query([file=Ref],Module)
	->  process_module_inclusion(Module,Cx)
	;	process_file_inclusion(Ref,Cx)
	).


process_module_inclusion(Module,Cx):-
	pef_module_definition_id(Module,MID),
    forall(	
    	pef_exports_query([module=MID,signature=op(Pr,Tp,Nm)]),
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
	    
