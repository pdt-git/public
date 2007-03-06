% creates pefs from prolog source code.
% the functionality is very limited, I mainly wrote this
% to have some test data for other experiments.
%

:- module(pef_parser,
	[	
		pef_term/2,
		pef_parse_file/1,
		pef_forget_file/1,
		pef_file_toplevel/2,
		pef_term_toplevel/2,
		pef_term_parent/2,
		pef_term_file/2,		
		pef_functor/3,
		pef_arg/3,
		pef_var/1,
		pef_add_property/3,
		pef_remove_property/3,
		pef_set_property/3,
		pef_property/3
	]).


:- use_module(library('org/cs3/pdt/util/pdt_util')).		
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).	
:- use_module(library('org/cs3/pdt/util/pdt_util_term_position')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('prolog_source')).

:- pdt_define_context(term(id,parent,order,toplevel,name,arity)).
:- pdt_define_context(var_info(name)).

:- index(term(1,1,0,1,1,0)).
:- dynamic term/6.	
:- dynamic term_property/3.
:- dynamic file_term/2.
 
pef_property(ID,Prop,Val):-
    term_property(ID,Prop,Val).
    
pef_add_property(ID,Prop,Val):-
	assert(term_property(ID,Prop,Val)).

pef_remove_property(ID,Prop,Val):-	
	retract_all(term_property(ID,Prop,Val)).

pef_set_property(ID,Prop,Val):-
    forall(term_property(ID,Prop,O),retract(term_property(ID,Prop,O))),
	assert(term_property(ID,Prop,Val)).

%
%---------------------------------------------------------------
% 
	
	
pef_file_toplevel(FileSpec,Term):-
    nonvar(FileSpec),
    !,
    pdt_file_ref(FileSpec,Ref),
    gen_id(Ref,0,FileID),
    pef_term_parent(Term,FileID).
pef_file_toplevel(FileSpec,Term):-
    pef_term_toplevel(Term,Term),
    pef_term_parent(Term,Ref),
    pdt_file_ref(FileSpec,Ref).
    
pef_term_file(ID,File):-
    nonvar(ID),
    !,
    gen_id(Ref,_,ID),
    pdt_file_ref(File,Ref).
pef_term_file(ID,File):-    
    pef_file_toplevel(File,TL),
	pef_term_toplevel(ID,TL).
    
    
pef_term_toplevel(ID,Toplevel):-
    term_new(T),
	term_id(T,ID),
    term_toplevel(T,Toplevel),
    call(T).


pef_term_parent(ID,Parent):-
    term_new(T),
    term_id(T,ID),
	term_parent(T,Parent),
    call(T).

    

pef_functor(ID,Name,Arity):-
    term_new(T),
	term_id(T,ID),
	term_name(T,Name),
	term_arity(T,Arity),
    call(T).

pef_arg(Order,ID,Value):-
    term_new(T),
	term_order(T,Order),
    term_parent(T,ID),
    term_id(T,Value),
    call(T).

pef_var(ID):-
    pef_functor(ID,'$var'(_),0).
    
%
%---------------------------------------------------------------
% 

    
pef_parse_file(Spec):-
    pdt_file_spec(Spec,File),
    pdt_file_ref(File,FileRef),
    prolog_canonical_source(File,Src),
	prolog_open_source(Src,Stream),
	call_cleanup(parse(Stream,FileRef), prolog_close_source(Stream)).
	
parse(Stream,FileRef):-
    flag(naive_parser_toplevel_num,OldToplevelNum,1),
    flag(naive_parser_term_id,OldTermId,1),
    repeat,
        flag(naive_parser_toplevel_num,Order,Order+1),
		prolog_read_source_term(Stream, Term, _ , 
			[double_quotes(string), subterm_positions(Positions),variable_names(Vars)]),
		process_variables(Vars),
		flag(naive_parser_term_id,TId,TId),		
		gen_id(FileRef,TId,ID),
		gen_id(FileRef,0,Parent),		
		create_term_pefs(Term,Parent,ID,Positions,Order),
		Term==end_of_file,
	!,
	flag(naive_parser_toplevel_num,_,OldToplevelNum),
    flag(naive_parser_term_id,_,OldTermId).
	


attr_unify_hook(_AttValue, _VarValue).

process_variables([]).
process_variables([Name=Var|Vars]):-
    var_info_new(VarInfo),
    var_info_name(VarInfo,Name),
    put_attr(Var,pef_parser,VarInfo),
    process_variables(Vars).

create_term_pefs(Term,Parent,Toplevel,Positions,Order):-
    gen_id(FileRef,_,Parent),
	flag(naive_parser_term_id,TID,TID+1),
	gen_id(FileRef,TID,ID),
	(	var(Term)
	->	get_attr(Term,pef_parser,VarInfo),
		var_info_name(VarInfo,VarName),
		Name='$var'(VarName),
		Arity=0
	;	functor(Term,Name,Arity)
	),
	term_new(T0),
	term_set(T0,[
		id=ID,
		parent=Parent,
		toplevel=Toplevel,
		order=Order,
		name=Name,
		arity=Arity		
	],T),
	assert(T),
	top_position(Positions,From,To),
    pef_set_property(ID,position,From-To),
    create_arg_pefs(Term,ID,Toplevel,Positions,1,Arity).

create_arg_pefs(_Term,_Parent,_Toplevel,_Positions,Order,Arity):-
    Order>Arity,
    !.
create_arg_pefs(Term,Parent,Toplevel,Positions,Order,Arity):-    
    arg(Order,Term,Arg),
    sub_position(Positions,Order,ArgPositions),
    NewOrder is Order + 1,
    create_term_pefs(Arg,Parent,Toplevel,ArgPositions,Order),
    create_arg_pefs(Term,Parent,Toplevel,Positions,NewOrder,Arity).
%
%---------------------------------------------------------------
% 
    
pef_forget_file(Spec):-
	pdt_file_ref(Spec,Ref),
	gen_id(Ref,0,FileID),
	forget_children(FileID).
	
gen_id(FileRef,Local,ID):-
    (	var(ID)
    ->	ID is (FileRef << 16) \/ Local
    ;	FileRef is ID >> 16,
    	Local is ((1<<16)-1) /\ ID
    ).
    
forget_children(PID):-
	forall(pef_term_parent(CID,PID),forget_term(CID)).    
	
forget_term(ID):-
    forget_children(ID),
    retractall(term_property(ID,_,_)),
    term_new(T),
    term_id(T,ID),
    retract(T).

%
%---------------------------------------------------------------
% 

pef_term(ID,Term):-
    pdt_map_empty(InVars),
	pef_term(InVars,ID,Term,_OutVars).
	
pef_term(InVars,ID,Term,OutVars):-
    pef_functor(ID,Name,Arity),
    pef_term(Name,Arity,InVars,ID,Term,OutVars).
    
pef_term('$var'(Name),0,InVars,_ID,Term,OutVars):-
	!,
	use_variable(InVars,Name,Term,OutVars).
pef_term(Name,Arity,InVars,ID,Term,OutVars):-
    functor(Term,Name,Arity),
    bind_args(Term,1,Arity,InVars,ID,OutVars).
    
bind_args(_Term,N,Arity,Vars,_,Vars):-
	N>Arity,
	!.
bind_args(Term,N,Arity,InVars,ID,OutVars):-	
	pef_arg(N,ID,ArgID),
	arg(N,Term,Arg),
	pef_term(InVars,ArgID,Arg,NextVars),
	M is N + 1,
	bind_args(Term,M,Arity,NextVars,ID,OutVars).

use_variable(InVars,Name,Var,OutVars):-
	(	pdt_map_get(InVars,Name,Var)
	->  OutVars=InVars
	;	pdt_map_put(InVars,Name,Var,OutVars)
	).