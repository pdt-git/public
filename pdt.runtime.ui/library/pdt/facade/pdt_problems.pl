:- module(pdt_problems,
	[	pdt_problem/6,
		pdt_problem/7,
		pdt_problem_count/1,
		pdt_problem_count/2,
		pdt_request_problems/1,
		pdt_request_problems/0
	]
).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/parse')).
:-use_module(library('builder/targets/singletons')).
:-use_module(library('builder/targets/interprete')).
:-use_module(library('builder/targets/literals')).
:-use_module(library('facade/pdt_workspace')).
:-use_module(library('util/progress')).
:-use_module(library(pif_observe2)).


syntax_error_position(error(_, stream(_, _, _, Offset)),Offset,Offset).
syntax_error_position(error(_, file(_, _, _, Offset)),Offset,Offset).
    



problem_target(cheap,global(Resource),parse(Resource)).
problem_target(cheap,global(Resource),singletons(Resource)).
problem_target(expensive,entry_point(Path),interprete(file(Path))).
problem_target(expensive,entry_point(Path),literals(first,file(Path))).

progress:report_start_hook(Group,Units):-
    problem_target(Group,_,_),
    !,
    debug(pdt_problems,"Started: ~w, units: ~w",[Group,Units]),
    Work is Units,
    pif_notify(progress(Group),start(Work)).

progress:report_done_hook(Group):-
    problem_target(Group,_,_),
    !,
    debug(pdt_problems,"Done: ~w",[Group]),    
    pif_notify(progress(Group),done).
    
progress:report_progress_hook(Task,Subtask,Units):-
	problem_target(Task,_,_),
    !,  
    debug(pdt_problems,"Worked: Task ~w, Subtask ~w, units: ~w",[Task,Subtask,Units]),
    Work is Units,
    pif_notify(progress(Task),worked(Work)).
    

progress:estimate_hook(Group,Key,1):-
    problem_target(Group,global(Resource),Target),
    pdt_request_target(workspace),
    pdt_contains_star(workspace,Resource),
    pdt_builder:target_key(Target,Key),
    \+ pdt_builder:available(Key).
progress:estimate_hook(Group,Key,1):-
    problem_target(Group,entry_point(Path),Target),
    pdt_request_target(parse(workspace)),
    pef_entry_point_query([path=EntryPointPath]),
    get_pef_file(EntryPointPath,File),
    file_depends_star(File,DependsFile),
    get_pef_file(Path,DependsFile),    
	pdt_builder:target_key(Target,Key),
    \+ pdt_builder:available(Key).

problem_targets(Group,Targets):-
    findall(Target,
    	(	problem_target(Group,Scope,Target),
    		(	Scope=entry_point(Path)
    		->	pef_entry_point_query([path=Path])
    		;	Scope=global(workspace)
    		)
    	), 
    	Targets
    ).
    
pdt_request_problems(Group):-
    problem_targets(Group,Targets),
    pdt_request_targets(Targets).
pdt_request_problems:-
    problem_targets(_,Targets),
    pdt_request_targets(Targets).    

sum(Times,Brutto):-
	sum(Times,0,Brutto).

sum([],Sum,Sum).
sum([Time|Times],Sum0,Sum):-
    Sum1 is Time + Sum0,
    sum(Times,Sum1,Sum).


pdt_problem_count(C):-
    pdt_request_problems,
    pef_count(problem,C).    

pdt_problem_count(Tag,Sum):-
    pdt_request_problems(Tag),
    findall(C,
    	(	pef_type_is_a(Type,problem),
    		pef_type_tag(Type,Tag),
    		pef_count(Type,C)
    	),
    	Cs
    ),
    sum(Cs,Sum).

%% pdt_problem(File,Tag,Start,End,Severity,Msg)
% successively finds all problems found by build targets.
pdt_problem(File,Tag,Start,End,Severity,Msg):-
    pdt_with_progress(Tag,pdt_request_problems(Tag)),
    %pdt_request_problems(Tag),
    problem(_,File,Tag,Start,End,Severity,Msg).
pdt_problem(Id,File,Tag,Start,End,Severity,Msg):-
    pdt_with_progress(Tag,pdt_request_problems(Tag)),
    %pdt_request_problems(Tag),
    problem(Id,File,Tag,Start,End,Severity,Msg).
    

problem(Id,File,expensive,Start,End,error,Message):-%module name clash
	pef_module_name_clash_query([id=Id,toplevel=TLID,first=MID]),
	toplevel_source_position(TLID,FID,Start,End),
	module_name(MID,MName),
	module_file(MID,FirstFID),
	get_pef_file(FirstFile,FirstFID),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("A module named ~w was already loaded from ~w.(~w)",[MName,FirstFile,Id])).

problem(Id,File,expensive,Start,End,error,Message):-%predicate already imported
	pef_predicate_already_imported_query([id=Id,toplevel=TLID,predicate=PRID]),
	pef_predicate_query([id=PRID,name=PName,arity=Arity,module=MID]),
	toplevel_source_position(TLID,FID,Start,End),
	module_name(MID,MName),	
	get_pef_file(File,FID),
	with_output_to(string(Message),format("A predicate ~w was already imported from module ~w.(~w)",[PName/Arity,MName,Id])).

problem(Id,File,expensive,Start,End,warning,Message):-%predicate redefinition
	pef_predicate_redefinition_query([id=Id,toplevel=TLID,first=PRID]),
	predicate_file(PRID,FirstFID),
	pef_predicate_query([id=PRID,name=PName,arity=Arity,module=MID]),
	module_name(MID,MName),
	toplevel_source_position(TLID,FID,Start,End),
	get_pef_file(FirstFile,FirstFID),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("Redefinition of predicate ~w originally defined in ~w.(~w)",[MName:PName/Arity,FirstFile,Id])).
	
problem(Id,File,expensive,Start,End,warning,Message):-%predicate abolished
	pef_predicate_abolished_query([id=Id,toplevel=TLID,module=MID,predicate=PRID]),
	predicate_file(PRID,FirstFID),
	pef_predicate_query([id=PRID,name=PName,arity=Arity]),
	module_name(MID,MName),
	toplevel_source_position(TLID,FID,Start,End),
	get_pef_file(FirstFile,FirstFID),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("Loading module ~w abolishes predicate ~w originally defined in ~w.(~w)",[MName,MName:PName/Arity,FirstFile,Id])).

problem(Id,File,expensive,Start,End,warning,Message):-%malformed signature
	pef_malformed_signature_query([id=Id,toplevel=TLID,signature=Signature]),
	toplevel_source_position(TLID,FID,Start,End),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("Malformed predicate signature: ~w.(~w)",[Signature,Id])).
	
problem(Id,File,cheap,Start,End,warning,SMessage):-%file not found
    pef_file_not_found_query([id=Id,file_spec=Spec,toplevel=Tl]),        
    toplevel_source_position(Tl,FID,Start,End),
    with_output_to(string(SMessage),format("Cannot resolve file spec: ~w (~w)",[Spec,Id])),  
    get_pef_file(File,FID).
problem(Id,File,cheap,Start,End,warning,SMessage):-%Parser dependency cycle
    pef_parser_dependency_cycle_query([id=Id,action=Action,toplevel=Tl]),
    toplevel_source_position(Tl,FID,Start,End),
    with_output_to(string(SMessage),format("Parser ignored cyclic dependency: ~w (~w)",[Action,Id])),  
    get_pef_file(File,FID).    
    
    
    
problem(Id,File,cheap,Start,End,error,SMessage):-%syntax error
    pef_syntax_error_query([id=Id,file=FID,error=Error]),
    message_to_string(Error,Message),
    with_output_to(string(SMessage),format("~s (~w)",[Message,Id])),
    syntax_error_position(Error,Start,End),
    get_pef_file(File,FID).

problem(Id,File,cheap,Start,End,warning,Message):-%singleton
    pef_singleton_query([id=Id,variable=VID]),
    pef_variable_query([id=VID,name=Name,ast=Ast]),
    pef_ast_query([id=Ast,toplevel=TlID]),
    %pef_toplevel_query([id=TlID,file=FID]),
    toplevel_source_position(TlID,FID,_,_),
    pef_variable_occurance_query([variable=VID,id=OccID]),
    pef_property_query([pef=OccID,key=start,value=Start]),
	pef_property_query([pef=OccID,key=end,value=End]),
    get_pef_file(File,FID),
    with_output_to(string(Message),format("Variable ~w only apears once in this clause.(~w)",[Name,Id])).    
problem(Id,File,cheap,Start,End,warning,Message):-%no singleton
    pef_no_singleton_query([id=Id,variable=VID]),
    pef_variable_query([id=VID,name=Name,ast=Ast]),
    pef_ast_query([id=Ast,toplevel=TlID]),
    %pef_toplevel_query([id=TlID,file=FID]),
    toplevel_source_position(TlID,FID,_,_),
    pef_variable_occurance_query([variable=VID,id=OccID]),
    pef_property_query([pef=OccID,key=start,value=Start]),
	pef_property_query([pef=OccID,key=end,value=End]),
    get_pef_file(File,FID),
    with_output_to(string(Message),format("Variable ~w apears more than once in this clause.(~w)",[Name,Id])).
problem(Id,File,expensive,Start,End,warning,Message):-%unresolved predicate symbol
    pef_unresolved_predicate_symbol_query([id=Id,goal=Term,cx=Cx]),
    ast_toplevel(Term,TlID),
    %pef_toplevel_query([id=TlID,file=FID]),
    toplevel_source_position(TlID,FID,_,_),
    pef_term_query([id=Term,name=Name,arity=Arity]),
    pef_property_query([pef=Term,key=start,value=Start]),
	pef_property_query([pef=Term,key=end,value=End]),
    get_pef_file(File,FID),
    literals:cx_predicate(Cx,Pred),
    predicate_owner(Pred,Program),
    pef_program_query([id=Program,file=PFile]),
    get_pef_file(PPath,PFile),
    literals:cx_context(Cx,Context),
    with_output_to(string(Message),format("Cannot resolve ~w:~w/~w (entry point: ~w) .(~w)",[Context,Name,Arity,PPath,Id])).    
problem(Id,File,expensive,Start,End,warning,Message):-%cannot infer rule
    pef_cannot_infer_rule_query([id=Id,goal=Term,cx=Cx]),
    ast_toplevel(Term,TlID),
    %pef_toplevel_query([id=TlID,file=FID]),
    toplevel_source_position(TlID,FID,_,_),    
    pef_property_query([pef=Term,key=start,value=Start]),
	pef_property_query([pef=Term,key=end,value=End]),
    get_pef_file(File,FID),
    literals:cx_predicate(Cx,Pred),
    predicate_owner(Pred,Program),
    pef_program_query([id=Program,file=PFile]),
    get_pef_file(PPath,PFile),    
    with_output_to(string(Message),format("Cannot infer rule  (entry point: ~w) .(~w)",[PPath,Id])).    


            
