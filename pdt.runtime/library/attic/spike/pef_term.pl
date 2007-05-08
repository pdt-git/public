:- use_module(library('spike/pef_base')).
:- use_module(library('spike/parser')).
:- use_module(library('spike/ast_generator')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).

:- use_module(library('org/cs3/pdt/util/pdt_util_map')).

:- pdt_define_context(peft_term_cx(subst,invars,outvars)).

setup_test_data:-
	pdt_file_ref('z:/workspace/pdt.runtime/library/attic/spike/b.pl',Ref),
	pdt_parse(file_ref(Ref)),
	atom_concat(terms_,Ref,Key),
	forall(pef_toplevel_recorded(Key,_,TL),
		(	pdt_generate_ast(TL,Id),
			assert(testdata_root(Id))
		)
	).
	

apply_subst(Id,Subst,RealId):-
    pdt_map_get(Subst,Id,NextId),
    !,
    apply_subst(NextId,Subst,RealId).
apply_subst(Id,_,Id).

peft_term(Id,Term):-
    pdt_map_empty(Subst),
    pdt_map_empty(Vars),
    peft_term_cx_new(Cx),
    peft_term_cx_subst(Cx,Subst),
    peft_term_cx_invars(Cx,Vars),    
    peft_term(Id,Cx,Term).
    
peft_term(Id,Cx,Var):-
    peft_term_cx_get(Cx,[subst=Subst,invars=Vars,outvars=OutVars]),    
    apply_subst(Id,Subst,RealId),
	pef_variable_occurance_query([id=RealId,variable_ref=VarRef]),
	!,
	(	pdt_map_get(Vars,VarRef,Var)
	->	InVars=OutVars
	;	pdt_map_put(InVars,VarRef,Var,OutVars)
	).
peft_term(Id,Cx,Term):-
    peft_term_cx_subst(Cx,Subst),    
    apply_subst(Id,Subst,RealId),
	pef_term_query([id=RealId,name=Name,arity=Arity]),
	functor(Term,Name,Arity),
	peft_term_args(1,Arity,RealId,Cx,Term).  

peft_term_args(I,N,_Id,Cx,_Term):-
    I>N,
    !,
    peft_term_cx_get(Cx,[invars=Vars,outvars=Vars]).
peft_term_args(I,N,Id,Cx,Term):-        
    peft_arg(I,Id,ArgId),
    peft_term_cx_set(Cx,[outvars=ArgOutVars],ArgCx),
    peft_term(ArgId,ArgCx,Arg),
    arg(I,Term,Arg),
    J is I + 1,
    peft_term_cx_set(Cx,[invars=ArgOutVars],NextCx),
    peft_term_args(J,N,Id,NextCx,Term).
    
peft_arg(I,Id,Subst,ArgId):-    
	apply_subst(Id,Subst,RealId),
    pef_arg_query([num=I,parent=RealId,child=ArgId]).
peft_arg(I,Id,ArgId):-    
    pef_arg_query([num=I,parent=Id,child=ArgId]).


peft_var(Id):-
    pef_variable_occurance_query(Id).

peft_var(Id,Subst):-
	apply_subst(Id,Subst,RealId),
	pef_variable_occurance_query(RealId).

peft_functor(Id,Name,Arity):-
    pef_term_query([id=Id,name=Name,arity=Arity]).

peft_functor(Id,Subst,Name,Arity):-
    apply_subst(Id,Subst,RealId),
    pef_term_query([id=RealId,name=Name,arity=Arity]).
	
peft_unifiable(A,B,Unifier):-    
    pdt_map_empty(Unifier0),
    peft_unifiable(A,B,Unifier0,Unifier).
    
    
peft_unifiable(A,B,Unifier,MergedUnifier):-
	peft_var(A,Unifier),
	!,
	unifier_append(Unifier,A=B,MergedUnifier).
peft_unifiable(A,B,Unifier,MergedUnifier):-
	peft_var(B,Unifier),
	!,
	unifier_append(Unifier,B=A,MergedUnifier).
peft_unifiable(A,B,Unifier,MergedUnifier):-
    apply_subst(A,Unifier,RealA),
    apply_subst(B,Unifier,RealB),    
	peft_functor(RealA,Name,Arity),
	peft_functor(RealB,Name,Arity),
	peft_unifiable_args(1,Arity,RealA,RealB,Unifier,MergedUnifier).		

peft_unifiable_args(I,N,_A,_B,Unifier,Unifier):-
	I>N,
	!.
peft_unifiable_args(I,N,A,B,Unifier,MergedUnifier):-	
	peft_arg(I,A,ArgA),
	peft_arg(I,B,ArgB),
	peft_unifiable(ArgA,ArgB,Unifier,TmpUnifier),
	J is I + 1,
	peft_unifiable_args(J,N,A,B,TmpUnifier,MergedUnifier).
	
unifier_append(Unifier,A=B,MergedUnifier):-
	pdt_map_get(Unifier,A,BB),
	!,
	peft_unifiable(B,BB,Unifier,MergedUnifier).
unifier_append(Unifier,A=B,MergedUnifier):-
	pdt_map_put(Unifier,A,B,MergedUnifier).	