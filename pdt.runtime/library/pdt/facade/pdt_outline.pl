:- module(pdt_outline,
	[	pdt_print_outline/1,
		pdt_outline_child/5,
		pdt_outline_label/4,
		pdt_outline_tag/4,		
		pdt_outline_position/5,
		pdt_outline/7,
		pdt_outline/9
	]
).


:-use_module(library('/org/cs3/pdt/util/pdt_util')).
:-use_module(library('/org/cs3/pdt/util/pdt_util_term_position')).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).
:-use_module(library('util/pdt_render_term')).
:-use_module(library('builder/builder')).
:-use_module(library('builder/targets/parser')).
:-use_module(library('builder/targets/program_interpreter')).

%define a pseudo build target for the outline to refer to.

pdt_builder:build_hook(outline(AbsFile)):-
    pdt_with_targets([interprete(AbsFile)],true).
pdt_builder:invalidate_hook(interprete(AbsFile)):-
	pdt_invalidate_target(outline(AbsFile)).

%%
% pdt_outline_child(+File,+ParentType,+ParentID,-ChildType,-ChildID).
% find children of a node in the outline tree view.
% @param File the absolute path to the file to be outlined.
% @param ParentType PEF type of the parent node
% @param ParentId PEF id of the parent node
% @param ChildType PEF type of the child node
% @param ChildtId PEF id of the chld node
pdt_outline_child(FID,ParT,ParID,ChT,ChID):-
    integer(FID),
    !,
    get_pef_file(File,FID),
	pdt_with_targets([outline(File)],
		outline_child(ParT,ParID,ChT,FID,ChID)		
	).
pdt_outline_child(File,ParT,ParID,ChT,ChID):-
    get_pef_file(File,FID),
	pdt_with_targets([outline(File)],
		outline_child(ParT,ParID,ChT,FID,ChID)		
	).



outline_child(pef_file,FID,pef_toplevel,FID,TlID):-
    pef_toplevel_query([id=TlID,file=FID,term=(:-_)]).
outline_child(pef_file,FID,pef_predicate,FID,Pred):-
    pef_program_query([file=FID,id=PID]),
	pef_toplevel_query([id=TlID,file=FID]),
	pef_clause_query([predicate=Pred,toplevel=TlID]),
	% make sure we report each predicate only once per file:
	% the following only succeeds if TlID is the first clause
	% of this predicate within this file.
	once( 
		(	pef_clause_query([predicate=Pred,toplevel=FirstTlID]),
			pef_toplevel_query([id=FirstTlID,file=FID])	
		)
	),
	TlID == FirstTlID,
    pef_predicate_query([id=Pred,module=MID]),
    pef_program_module_query([program=PID,module=MID]).
outline_child(pef_predicate,Pred,pef_toplevel,FID,Tl):-
    pef_clause_query([predicate=Pred,toplevel=Tl]),
    pef_toplevel_query([id=Tl,file=FID]).

%% 
% pdt_outline_label(+Type,+Node,-Label,-Tags).
% Find out how a node should be displayed in the outline.
% @param File the absolute path to the file to be outlined.
% @param Type  the pef type of the node.
% @param Node the pef id of the node.
% @param Label will be unified with the text to display
pdt_outline_label(FID,Type,ID,Label):-
    integer(FID),
    !,
    get_pef_file(File,FID),	
	pdt_with_targets([outline(File)],
		outline_label(Type,ID,FID,Label)		
	).
pdt_outline_label(File,Type,ID,Label):-
    get_pef_file(File,FID),	
	pdt_with_targets([outline(File)],
		outline_label(Type,ID,FID,Label)		
	).
    
    
outline_label(pef_file,FID,FID,Label):-
    pef_file_query([id=FID,path=Label]).
outline_label(pef_predicate,Pred,FID,Label):-
    pef_predicate_query([id=Pred,name=Name,arity=Arity,module=MID]),
    concat_atom([Name,Arity],'/',Signature),
	(   local_module(FID,MID)
	->	Label=Signature
	;	module_name(MID,MName),
		concat_atom([MName,Signature],':',Label)
	).
outline_label(pef_toplevel,Tl,_FID,Label):-
    pef_toplevel_query([id=Tl,varnames=VarNames,term=Term]),
    subterm_to_render(Term,SubTerm),
    pdt_visible_ops(Tl,Ops),
    pdt_render_term(SubTerm,VarNames,Ops,3,Label).

    
% succeeds if module is defined by file or if module is user and 
% file is not associated with any module.    
local_module(FID,MID):-
	(	pef_module_definition_query([file=FID])
	->	module_file(MID,FID)
	;	module_name(MID,user)
	).

	
    
subterm_to_render(:-Body,Body):-
    !.
subterm_to_render(Head:-_,Head):-
    !.    
subterm_to_render(Fact,Fact).

pdt_outline_tag(FID,Type,ID, Tag):-
    integer(FID),
    !,
    get_pef_file(File,FID),
    pdt_with_targets([outline(File)],
		outline_tag(Type,ID,FID,Tag)		
	).
pdt_outline_tag(File,Type,ID, Tag):-
    get_pef_file(File,FID),
    pdt_with_targets([outline(File)],
		outline_tag(Type,ID,FID,Tag)		
	).
    

outline_tag(pef_predicate,ID,FID,public):-
    pef_predicate_query([id=ID,module=MID,name=Name,arity=Arity]),
    module_name(MID,Module),
    public_predicate(Module,Name,Arity,FID).
outline_tag(pef_predicate,ID,_FID,Prop):-
    pef_predicate_property_definition_query([predicate=ID,property=Prop]).
outline_tag(pef_toplevel,ID,_FID,directive):-
    pef_toplevel_query([id=ID,expanded=(:-_)]).

public_predicate(user,_Name,_Arity,_FID):-!.
public_predicate(system,_Name,_Arity,_FID):-!.
public_predicate(Module,Name,Arity,FID):-
    pef_program_query([file=FID,id=PID]),
    resolve_module(PID,Module,MID),
    module_exports(MID,Name/Arity).
    
pdt_outline_position(FID,Type,ID, From,To):-
    integer(FID),
    !,
    get_pef_file(File,FID),    
	pdt_with_targets([outline(File)],
		outline_position(Type,ID,FID,From,To)	
	).
pdt_outline_position(File,Type,ID, From,To):-
    get_pef_file(File,FID),    
	pdt_with_targets([outline(File)],
		outline_position(Type,ID,FID,From,To)	
	).

outline_position(pef_predicate,ID,FID,From,To):-
    outline_child(pef_predicate,ID,pef_toplevel,FID,TLID),
    !,
    outline_position(pef_toplevel,TLID,FID,From,To).
/*	pef_program_query([file=FID,id=PID]),
	first_clause(FID,PID,ID,Clause),
	pef_clause_toplevel(Clause,TLID),
	pef_toplevel_query([id=TLID,positions=Positions]),
	top_position(Positions,From,To).*/

outline_position(pef_toplevel,ID,_FID,From,To):-
    pef_toplevel_query([id=ID,positions=Positions]),
	top_position(Positions,From,To).    
	
	
pdt_print_outline(File):-
	get_pef_file(File,FID),
	print_outline_X(pef_file, FID, FID, '').
	
print_outline_X(Type, ID, FID, Prefix):-
    pdt_outline_label(FID,Type,ID,Label),
    atom_concat(Prefix,Label,Output),
    writeln(Output),
    forall(
    	pdt_outline_child(FID,Type,ID,CType,CID),
    	(	atom_concat('\t',Prefix,CPrefix),
    		print_outline_X(CType,CID,FID,CPrefix)
    	)
    ).

pdt_outline(Abs,ChildT,Child,Label,Tags,Start,End):-    
	get_pef_file(Abs,FID),
	pdt_outline_child(FID,pef_file,FID,ChildT,Child),
	pdt_outline_label(FID,ChildT,Child,Label),
	findall(Tag,pdt_outline_tag(FID,ChildT,Child,Tag),Tags),
	(	pdt_outline_position(FID,ChildT,Child,Start,End)
	->	true
	;	Start= -1,
		End= -1
	).    
	
pdt_outline(Abs,ParT,Par,ChildT,Child,Label,Tags,Start,End):-    
	get_pef_file(Abs,FID),
	pdt_outline_child(FID,ParT,Par,ChildT,Child),
	pdt_outline_label(FID,ChildT,Child,Label),
	findall(Tag,pdt_outline_tag(FID,ChildT,Child,Tag),Tags),
	(	pdt_outline_position(FID,ChildT,Child,Start,End)
	->	true
	;	Start= -1,
		End= -1
	).    	