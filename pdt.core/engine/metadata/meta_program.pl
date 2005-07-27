:- module(meta_program,[]).


:- use_module(plast).
:- use_module(plast_util).
:- use_module(u_term).


% mp_unify(+Sin,+LUTerm,+RUTerm,-Sout)

mp_unify(Sin,LUTerm,RUTerm,Sout):-
    lr_u_term(l,_,_,LVars,_,_,LUTerm),
    lr_u_term(r,_,_,RVars,_,_,RUTerm),
	merge(LVars,RVars,Variables),
	LUTerm=RUTerm,
	merge_bound(Sin,Variables,Sout).

merge_bound(Sin,[],Sin).
merge_bound(Sin,[Sym=Val|Tail],Sout):-
    (	nonvar(Val)
    ->	merge(Sin, [Sym=Val],Sout)
    ;	Sout=Sin
    ),
    merge_bound(Sin,Tail,Sout).

    	
mp_subst(FrameId,Bindings,Node,ROutTerm):-
	u_term(r, t_term(FrameId,Node,Bindings),ROutTerm).
	
	

new_frame(FrameId):-
	gensym(fid,FrameId).	


meta_clause(Id,(mp_directive(Id):-MetaBody)):-
	toplevel_term(Id,directive,_,[],Body),
	unfold(Id,[],Body,_,MetaBody).
	
meta_clause(Id,(
	
	mp_rule(Module,LTerm_InHead,RTerm_OutHead,Id):-
    	new_frame(FrameId),
	   	mp_subst(FrameId,[],HeadNode,RTerm_HeadNode),
		mp_unify([],LTerm_InHead,RTerm_HeadNode,S0),
		mp_subst(FrameId,S0,HeadNode,RTerm_OutHead))
	)
	
	:-
	toplevel_term(Id,fact,Module,HeadNode,[]),

	plast_prop(Module,term(Module)).

meta_clause(Id,(
	
	mp_rule(Module,LTerm_InHead,RTerm_OutHead,Id):-
    	new_frame(FrameId),
	   	mp_subst(FrameId,[],HeadNode,RTerm_HeadNode),
		mp_unify([],LTerm_InHead,RTerm_HeadNode,S0),
		Body,
		mp_subst(FrameId,Sout,HeadNode,RTerm_OutHead))
	)
		
	:-
	toplevel_term(Id,rule,ModuleNode,HeadNode,BodyNode),
	plast_prop(ModuleNode,term(Module)),
	unfold(FrameId,Id,S0,BodyNode,Sout,Body).

unfold(FrameId,ClauseId,S0,Node,Sout,
(
	GoalA,GoalB
):-
    plast_prop(Node,functor((,)/2)),
    plast_prop(Node,arguments([A,B])),
    unfold(FrameId,ClauseId,S0,A,S1,GoalA),
    unfold(FrameId,ClauseId,S1,B,Sout,GoalB).
    
unfold(FrameId,ClauseId,S0,Node,Sout,
(
	GoalA;GoalB
):-
    plast_prop(Node,functor((;)/2)),
    plast_prop(Node,arguments([A,B])),
    unfold(FrameId,ClauseId,S0,A,S1,GoalA),
    unfold(FrameId,ClauseId,S1,B,Sout,GoalB).
	