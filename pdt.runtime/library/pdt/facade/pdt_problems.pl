:- module(pdt_problems,[pdt_problem/5]).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/parse')).
:-use_module(library('builder/targets/singletons')).
:-use_module(library('builder/targets/interprete')).
:-use_module(library('org/cs3/pdt/util/pdt_util_term_position')).



pdt_builder:target_group(file(F),problems):-
    pef_file_query([path=F]).
pdt_builder:target_group(parse(F),problems):-
    pef_file_query([path=F]).
pdt_builder:target_group(interprete(F),problems):-
    pef_file_query([path=F]).
pdt_builder:target_group(singletons(F),problems):-
    pef_file_query([path=F]).

    
syntax_error_position(error(_, stream(_, _, _, Offset)),Offset,Offset).
syntax_error_position(error(_, file(_, _, _, Offset)),Offset,Offset).
    


%% pdt_problem(File,Start,End,Severity,Msg)
% successively finds all problems found by build targets.

pdt_problem(File,Start,End,Severity,Msg):-
    pdt_with_targets([problems],
    	problem(File,Start,End,Severity,Msg)
    ).

problem(File,Start,End,error,Message):-%module name clash
	pef_module_name_clash_query([toplevel=TLID,first=MID]),
	pef_toplevel_query([id=TLID,file=FID,positions=Positions]),
	top_position(Positions,Start,End),
	module_name(MID,MName),
	module_file(MID,FirstFID),
	get_pef_file(FirstFile,FirstFID),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("A module named ~w was already loaded from ~w.",[MName,FirstFile])).

problem(File,Start,End,error,Message):-%predicate name clash
	pef_predicate_name_clash_query([toplevel=TLID,first=PRID]),
	pef_predicate_query([id=PRID,name=PName,arity=Arity,module=MID]),
	pef_toplevel_query([id=TLID,file=FID,positions=Positions]),
	top_position(Positions,Start,End),
	module_name(MID,MName),	
	get_pef_file(File,FID),
	with_output_to(string(Message),format("A predicate ~w was already importet from module ~w.",[PName/Arity,MName])).

problem(File,Start,End,warning,Message):-%predicate redefinition
	pef_predicate_name_clash_query([toplevel=TLID,first=PRID]),
	predicate_file(PRID,FirstFID),
	pef_predicate_query([id=PRID,name=PName,arity=Arity,module=MID]),
	module_name(MID,MName),
	pef_toplevel_query([id=TLID,file=FID,positions=Positions]),
	top_position(Positions,Start,End),
	get_pef_file(FirstFile,FirstFID),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("Redefinition of predicate ~w originally defined in ~w.",[MName:PName/Arity,FirstFile])).
	
problem(File,Start,End,warning,Message):-%predicate abolished
	pef_predicate_abolished_query([toplevel=TLID,module=MID,predicate=PRID]),
	predicate_file(PRID,FirstFID),
	pef_predicate_query([id=PRID,name=PName,arity=Arity]),
	module_name(MID,MName),
	pef_toplevel_query([id=TLID,file=FID,positions=Positions]),
	top_position(Positions,Start,End),
	get_pef_file(FirstFile,FirstFID),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("Loading module ~w abolishes predicate ~w originally defined in ~w.",[MName,MName:PName/Arity,FirstFile])).

	
problem(File,Start,End,error,Message):-%syntax error
    pef_syntax_error_query([file=FID,error=Error]),
    message_to_string(Error,Message),
    syntax_error_position(Error,Start,End),
    get_pef_file(File,FID).
problem(File,Start,End,warning,Message):-%singleton
    pef_singleton_query([variable=VID]),
    pef_variable_query([id=VID,name=Name,ast=Ast]),
    pef_ast_query([id=Ast,toplevel=TlID]),
    pef_toplevel_query([id=TlID,file=FID]),
    pef_variable_occurance_query([variable=VID,id=OccID]),
    pef_property_query([pef=OccID,key=start,value=Start]),
	pef_property_query([pef=OccID,key=end,value=End]),
    get_pef_file(File,FID),
    with_output_to(string(Message),format("Variable ~w only apears once in this clause.",[Name])).    
problem(File,Start,End,warning,Message):-%no singleton
    pef_no_singleton_query([variable=VID]),
    pef_variable_query([id=VID,name=Name,ast=Ast]),
    pef_ast_query([id=Ast,toplevel=TlID]),
    pef_toplevel_query([id=TlID,file=FID]),
    pef_variable_occurance_query([variable=VID,id=OccID]),
    pef_property_query([pef=OccID,key=start,value=Start]),
	pef_property_query([pef=OccID,key=end,value=End]),
    get_pef_file(File,FID),
    with_output_to(string(Message),format("Variable ~w apears more than once in this clause.",[Name])).        