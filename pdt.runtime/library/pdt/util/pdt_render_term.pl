:- module(pdt_render_term,[	
	pdt_render_term/5,
	pdt_visible_op/2,
	pdt_visible_ops/2
]).
:-use_module(library('/org/cs3/pdt/util/pdt_util_term_position')).
:-use_module(library('pef/pef_base')).
:-use_module(library('pef/pef_api')).


pdt_visible_ops(Tl,Ops):-
    find_visible_operators(Tl,Ops).

pdt_visible_op(Tl,Op):-
    find_visible_operators(Tl,Op).

/*find operator definitions that affect a particular toplevel*/
find_visible_operators(TlRef,Ops):-
    findall(Op,visible_op(TlRef,Op),Ops).


%% visible_op(+ToplevelRef, -Op).
%true if the toplevel "can see" the operator definition.
%case 1: op definition in the same file preceeding the term.    
%case 2: toplevel is in a file that defines a module exporting the op
%case 3: toplevel is in a file that imports the op from another module, and this import
% 		  takes place before the toplevel is parsed.
visible_op(TlRef,op(Pr,Tp,Nm)):-
	pef_toplevel_query([id=TlRef,file=FileRef,positions=Positions]),
	pef_op_definition_query([file=FileRef,toplevel=DefRef,priority=Pr,type=Tp,name=Nm]),
	\+ TlRef= DefRef, % op definition do not affect the toplevel they are defined in.
	pef_toplevel_query([id=DefRef,positions=DefPositions]),	
	top_position(Positions,TermStart,_),
	top_position(DefPositions,_,DefEnd),
	DefEnd =< TermStart.% end pos is exclusive
visible_op(TlRef,op(Pr,Tp,Nm)):-
    pef_toplevel_query([id=TlRef,file=FileRef]),
    pef_module_definition_query([file=FileRef,toplevel=DefRef]),
    pef_toplevel_query([id=DefRef,expanded=(:-module(_,Exports))]),
    member(op(Pr,Tp,Nm),Exports).
visible_op(TlRef,op(Pr,Tp,Nm)):-
	pef_toplevel_query([id=TlRef,file=FileRef,positions=Positions]),
	pef_file_dependency_query([depending=FileRef, toplevel=IncRef, dependency=DepRef]),
	pef_module_definition_query([file=DepRef,toplevel=DefRef]),
    pef_toplevel_query([id=DefRef,expanded=(:-module(_,Exports))]),
    member(op(Pr,Tp,Nm),Exports),    
    pef_toplevel_query([id=IncRef,positions=IncPositions]),	
	top_position(Positions,TermStart,_),
	top_position(IncPositions,_,IncEnd),
	IncEnd =< TermStart.% end pos is exclusive



pdt_render_term(Term,VarNames,Ops,Depth,Label):-
    render_term(Term,VarNames,Ops,Depth,Label).

render_term(Term,VarNames,Ops,Depth,Label):-
	execute_elms(VarNames),
	term_variables(Term,Vars),
	unify_with_underscores(Vars),
	push_operators(Ops),	
	new_memory_file(MemFile),	
	open_memory_file(MemFile,write,Stream),	
	call_cleanup(
		write_term(Stream,Term,[max_depth(Depth),portray(true),module(pdt_render_term)]),
		(	close(Stream),
			pop_operators			
		)
	),
	memory_file_to_atom(MemFile,Label),
	free_memory_file(MemFile).

execute_elms([]).
execute_elms([Goal|Goals]):-
	Goal,
	execute_elms(Goals).

unify_with_underscores([]).
unify_with_underscores([Var|Vars]):-
	Var='_',
	unify_with_underscores(Vars).