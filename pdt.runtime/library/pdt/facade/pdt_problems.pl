:- module(pdt_problems,
	[	pdt_problem/5,
		pdt_problem/6,
		pdt_problem_count/1
	]
).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/problems')).
:-use_module(library('org/cs3/pdt/util/pdt_util_term_position')).

syntax_error_position(error(_, stream(_, _, _, Offset)),Offset,Offset).
syntax_error_position(error(_, file(_, _, _, Offset)),Offset,Offset).
    


pdt_problem_count(C):-
    pef_count(problem,C).    

%% pdt_problem(File,Start,End,Severity,Msg)
% successively finds all problems found by build targets.
pdt_problem(File,Start,End,Severity,Msg):-
    pdt_with_targets([problems(workspace)],
    	problem(_,File,Start,End,Severity,Msg)
    ).
pdt_problem(Id,File,Start,End,Severity,Msg):-
    pdt_with_targets([problems(workspace)],
    	problem(Id,File,Start,End,Severity,Msg)
    ).

problem(Id,File,Start,End,error,Message):-%module name clash
	pef_module_name_clash_query([id=Id,toplevel=TLID,first=MID]),
	pef_toplevel_query([id=TLID,file=FID,positions=Positions]),
	top_position(Positions,Start,End),
	module_name(MID,MName),
	module_file(MID,FirstFID),
	get_pef_file(FirstFile,FirstFID),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("A module named ~w was already loaded from ~w.(~w)",[MName,FirstFile,Id])).

problem(Id,File,Start,End,error,Message):-%predicate name clash
	pef_predicate_name_clash_query([id=Id,toplevel=TLID,first=PRID]),
	pef_predicate_query([id=PRID,name=PName,arity=Arity,module=MID]),
	pef_toplevel_query([id=TLID,file=FID,positions=Positions]),
	top_position(Positions,Start,End),
	module_name(MID,MName),	
	get_pef_file(File,FID),
	with_output_to(string(Message),format("A predicate ~w was already imported from module ~w.(~w)",[PName/Arity,MName,Id])).

problem(Id,File,Start,End,warning,Message):-%predicate redefinition
	pef_predicate_name_clash_query([id=Id,toplevel=TLID,first=PRID]),
	predicate_file(PRID,FirstFID),
	pef_predicate_query([id=PRID,name=PName,arity=Arity,module=MID]),
	module_name(MID,MName),
	pef_toplevel_query([id=TLID,file=FID,positions=Positions]),
	top_position(Positions,Start,End),
	get_pef_file(FirstFile,FirstFID),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("Redefinition of predicate ~w originally defined in ~w.(~w)",[MName:PName/Arity,FirstFile,Id])).
	
problem(Id,File,Start,End,warning,Message):-%predicate abolished
	pef_predicate_abolished_query([id=Id,toplevel=TLID,module=MID,predicate=PRID]),
	predicate_file(PRID,FirstFID),
	pef_predicate_query([id=PRID,name=PName,arity=Arity]),
	module_name(MID,MName),
	pef_toplevel_query([id=TLID,file=FID,positions=Positions]),
	top_position(Positions,Start,End),
	get_pef_file(FirstFile,FirstFID),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("Loading module ~w abolishes predicate ~w originally defined in ~w.(~w)",[MName,MName:PName/Arity,FirstFile,Id])).

	
problem(Id,File,Start,End,error,SMessage):-%syntax error
    pef_syntax_error_query([id=Id,file=FID,error=Error]),
    message_to_string(Error,Message),
    with_output_to(string(SMessage),format("~s (~w)",[Message,Id])),
    syntax_error_position(Error,Start,End),
    get_pef_file(File,FID).
problem(Id,File,Start,End,warning,Message):-%singleton
    pef_singleton_query([id=Id,variable=VID]),
    pef_variable_query([id=VID,name=Name,ast=Ast]),
    pef_ast_query([id=Ast,toplevel=TlID]),
    pef_toplevel_query([id=TlID,file=FID]),
    pef_variable_occurance_query([variable=VID,id=OccID]),
    pef_property_query([pef=OccID,key=start,value=Start]),
	pef_property_query([pef=OccID,key=end,value=End]),
    get_pef_file(File,FID),
    with_output_to(string(Message),format("Variable ~w only apears once in this clause.(~w)",[Name,Id])).    
problem(Id,File,Start,End,warning,Message):-%no singleton
    pef_no_singleton_query([id=Id,variable=VID]),
    pef_variable_query([id=VID,name=Name,ast=Ast]),
    pef_ast_query([id=Ast,toplevel=TlID]),
    pef_toplevel_query([id=TlID,file=FID]),
    pef_variable_occurance_query([variable=VID,id=OccID]),
    pef_property_query([pef=OccID,key=start,value=Start]),
	pef_property_query([pef=OccID,key=end,value=End]),
    get_pef_file(File,FID),
    with_output_to(string(Message),format("Variable ~w apears more than once in this clause.(~w)",[Name,Id])).        