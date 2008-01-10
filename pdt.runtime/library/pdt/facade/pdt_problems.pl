:- module(pdt_problems,
	[	pdt_problem/6,
		pdt_problem/7,
		pdt_problem_count/1,
		pdt_problem_count/2
	]
).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/problems')).


syntax_error_position(error(_, stream(_, _, _, Offset)),Offset,Offset).
syntax_error_position(error(_, file(_, _, _, Offset)),Offset,Offset).
    
problems:kind(cheap,Path,parse(file(Path))).
problems:kind(cheap,Path,singletons(Path)).
problems:kind(expensive,Path,interprete(Path)).
problems:kind(expensive,Path,literals(first,file(Path))).

sum(Times,Brutto):-
	sum(Times,0,Brutto).

sum([],Sum,Sum).
sum([Time|Times],Sum0,Sum):-
    Sum1 is Time + Sum0,
    sum(Times,Sum1,Sum).


pdt_problem_count(C):-
    pef_count(problem,C).    

pdt_problem_count(Tag,Sum):-
    findall(C,
    	(	pef_type_is_a(Type,problem),
    		pef_type_tag(Type,Tag),
    		pef_count(Type,C)
    	),
    	Cs
    ),
    sum(Cs,Sum).

%% pdt_problem(File,Start,End,Severity,Msg)
% successively finds all problems found by build targets.
pdt_problem(File,Tag,Start,End,Severity,Msg):-
    pdt_with_targets([problems(workspace,[Tag])],
    	problem(_,File,Tag,Start,End,Severity,Msg)
    ).
pdt_problem(Id,File,Tag,Start,End,Severity,Msg):-
    pdt_with_targets([problems(workspace,[Tag])],
    	problem(Id,File,Tag,Start,End,Severity,Msg)
    ).

problem(Id,File,expensive,Start,End,error,Message):-%module name clash
	pef_module_name_clash_query([id=Id,toplevel=TLID,first=MID]),
	toplevel_source_position(TLID,FID,Start,End),
	module_name(MID,MName),
	module_file(MID,FirstFID),
	get_pef_file(FirstFile,FirstFID),
	get_pef_file(File,FID),
	with_output_to(string(Message),format("A module named ~w was already loaded from ~w.(~w)",[MName,FirstFile,Id])).

problem(Id,File,expensive,Start,End,error,Message):-%predicate name clash
	pef_predicate_name_clash_query([id=Id,toplevel=TLID,first=PRID]),
	pef_predicate_query([id=PRID,name=PName,arity=Arity,module=MID]),
	toplevel_source_position(TLID,FID,Start,End),
	module_name(MID,MName),	
	get_pef_file(File,FID),
	with_output_to(string(Message),format("A predicate ~w was already imported from module ~w.(~w)",[PName/Arity,MName,Id])).

problem(Id,File,expensive,Start,End,warning,Message):-%predicate redefinition
	pef_predicate_name_clash_query([id=Id,toplevel=TLID,first=PRID]),
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

	
problem(Id,File,cheap,Start,End,warning,SMessage):-%file not found
    pef_file_not_found_query([id=Id,file_spec=Spec,toplevel=Tl]),        
    toplevel_source_position(Tl,FID,Start,End),
    with_output_to(string(SMessage),format("Cannot resolve file spec: ~w (~w)",[Spec,Id])),  
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


            