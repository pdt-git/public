:- module(pdt_problems,[pdt_problem/5]).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/parse')).
:-use_module(library('builder/targets/singletons')).
:-use_module(library('builder/targets/interprete')).



pdt_builder:target_group(file(F),problems):-
    pef_file_query([path=F]).
pdt_builder:target_group(parse(F),problems):-
    pef_file_query([path=F]).
pdt_builder:target_group(interprete(F),problems):-
    pef_file_query([path=F]).
pdt_builder:target_group(singletons(F),problems):-
    pef_file_query([path=F]).

    



%% pdt_problem(File,Start,End,Severity,Msg)
% successively finds all problems found by build targets.

pdt_problem(File,Start,End,Severity,Msg):-
    pdt_with_targets([problems],
    	problem(File,Start,End,Severity,Msg)
    ).

problem(File,Start,End,error,Message):-%syntax error
    pef_syntax_error_query([file=FID,start=Start,end=End,message=Msg]),
    message_to_string(Msg,Message),
    get_pef_file(File,FID).
problem(File,Start,End,error,Message):-%singleton
    pef_singleton_query([variable=VID]),
    pef_variable_query([id=VID,name=Name,ast=Ast]),
    pef_ast_query([id=Ast,toplevel=TlID]),
    pef_toplevel_query([id=TlID,file=FID]),
    pef_variable_occurance_query([variable=VID,id=OccID]),
    pef_property_query([pef=OccID,key=start,value=Start]),
	pef_property_query([pef=OccID,key=end,value=End]),
    get_pef_file(File,FID),
    with_output_to(string(Message),format("Variable ~w only apears once in this clause.",[Name])).    
problem(File,Start,End,error,Message):-%no singleton
    pef_no_singleton_query([variable=VID]),
    pef_variable_query([id=VID,name=Name,ast=Ast]),
    pef_ast_query([id=Ast,toplevel=TlID]),
    pef_toplevel_query([id=TlID,file=FID]),
    pef_variable_occurance_query([variable=VID,id=OccID]),
    pef_property_query([pef=OccID,key=start,value=Start]),
	pef_property_query([pef=OccID,key=end,value=End]),
    get_pef_file(File,FID),
    with_output_to(string(Message),format("Variable ~w apears more than once in this clause.",[Name])).        