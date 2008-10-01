:- module(builder__transitions,
	[	transition/4
	]
).



:- use_module(builder__graph).
:- use_module(builder__actions).

:- op(1200,xfx,~~>).
:- op(1199,xfx,~~).
:- op(1198,xfx,/).


builder__transitions:term_expansion( (State ~~ ConditionAndAction) , Output):-
	term_expansion( (State ~~ ConditionAndAction ~~> State) ,Output).

builder__transitions:term_expansion( (State0 ~~ TriggerCondition / Action ~~> NewState0) , Output):-
	!,
	pp_split_condition(TriggerCondition,Trigger,Guard),
	pp_process_state(State0,State),
	pp_process_state(NewState0,NewState),
	(	Guard==true
	->	Output0 = (transition(Trigger,This,State,NewState):-!,ignore(Action))
	;	pp_compile_condition(Guard,CompiledGuard),
		Output0 = (transition(Trigger,This,State,NewState):-CompiledGuard,!,ignore(Action))
	),
	replace_this(Output0,This,Output).
builder__transitions:term_expansion( (State0 ~~ TriggerCondition  ~~> NewState0) , Output):-
	pp_split_condition(TriggerCondition,Trigger,Guard),
	pp_process_state(State0,State),
	pp_process_state(NewState0,NewState),
	(	Guard==true
	->	Output0 = (transition(Trigger,This,State,NewState):-!)
	;	pp_compile_condition(Guard,CompiledGuard),
		Output0 = (transition(Trigger,This,State,NewState):-CompiledGuard,!)
	),
	replace_this(Output0,This,Output).

pp_split_condition((Trigger,Guard0),Trigger,Guard0):-
	!.
pp_split_condition(Trigger,Trigger,true).

pp_process_state((Activity,Status),state(Activity,Status)).

pp_add_argument_to_literals((Literal0,Literals0),Arg,(Literal,Literals)):-
	!,
	pp_add_argument(Arg,Literal0,Literal),
	pp_add_argument_to_literals(Literals0,Arg,Literals).
pp_add_argument_to_literals(Literal0,Arg,Literal):-
	pp_add_argument(Arg,Literal0,Literal).

pp_add_argument(Arg,Literal0,Literal):-
	Literal0 =.. [Fun|Args0],
	append(Args0,[Arg],Args),
	Literal =.. [Fun|Args].

pp_compile_condition((\+ More),(\+ Code)):-
	!,
	pp_compile_condition(More,Code).
pp_compile_condition((CondA , CondB),(CodeA,CodeB)):-
	!,	
	pp_compile_condition(CondA,CodeA),
	pp_compile_condition(CondB,CodeB).
pp_compile_condition((CondA ; CondB),(CodeA;CodeB)):-
	!,
	pp_compile_condition(CondA,CodeA),
	pp_compile_condition(CondB,CodeB).
pp_compile_condition((CondA -> CondB),forall(CodeA,CodeB)):-
	!,
	pp_compile_condition(CondA,CodeA),
	pp_compile_condition(CondB,CodeB).	
pp_compile_condition(Rel > 1,pp_more_than_one(Goal)):-
	!,
	pp_relation(Rel,Goal).
pp_compile_condition(Rel = 1,pp_one(Goal)):-
	!,
	pp_relation(Rel,Goal).
pp_compile_condition(Rel >= 1,once(Goal)):-	
	!,
	pp_relation(Rel,Goal).
pp_compile_condition(Rel > 0,once(Goal)):-	
	!,
	pp_relation(Rel,Goal).
pp_compile_condition(Rel = 0,( \+ Goal)):-
	!,
	pp_relation(Rel,Goal).
pp_compile_condition(Rel,once(Goal)):-
	pp_relation(Rel,Goal).



pp_relation(c(A,dep,B),target_depends(A,B)):-!.
pp_relation(c(A,C:req,B),edge_label(A,B,C,req)):-!.
pp_relation(c(A,C:bld(N),B),edge_label(A,B,C,bld(N))):-!.
pp_relation(c(A,C:bwd(N),B),edge_label(A,B,C,bwd(N))):-!.
pp_relation(c(A,C:lck,B),edge_label(A,B,C,lck)):-!.
pp_relation(c(A,C:xcl,B),edge_label(A,B,C,xcl)):-!.


pp_one(G):-
	copy_term(G,GG),
	call(G),
	!,
	forall(GG, G == GG).
		
pp_more_than_one(G):-
	copy_term(G,GG),
	call(G),
	!,
	\+ forall(GG, G == GG).

replace_this(Term,This,NewTerm):-
	(	Term == this
	->	NewTerm = This
	;	var(Term)
	->	NewTerm = Term
	;	Term =.. [Fun|Args],
		replace_this_args(Args,This, NewArgs),
		NewTerm =..[Fun|NewArgs]
	).

replace_this_args([],_,[]).
replace_this_args([Arg|Args],This, [NewArg|NewArgs]):-
	replace_this(Arg,This,NewArg),
	replace_this_args(Args,This,NewArgs).

	
%-----------------------------------------------------

idle,obsolete 		~~ req(T,C) 										/ start_build(T,C,this) 					~~> building,pending.
idle,consistent		~~ req(T,C)											/ add_lck(T,C,this)							~~> reading,consistent.
idle,consistent		~~ invalidate										/ propagate_invalid(this)					~~> idle,obsolete.
% Note that the whole magic happens in the guard condition: it says:
%  if for each of my outgoing bwd(N) edges (there can be at most one!) there is an
%  incoming bwd(N) edge, then I am the first node in the SCC and thus the build is complete.
building,pending 	~~ success, \+c(_,_:req,this), (c(this,_:bwd(N),_) -> c(_,_:bwd(N),this))/ build_done(this) 	~~> idle,consistent.
% There is one edge missing in the thesis: See PDT-307
building,pending 	~~ success, (c(this,_:bwd(N),_) -> c(_,_:bwd(N),this))/ build_done(this), grant_waiting(this)	~~> reading,consistent.
building,pending 	~~ success. %target has outgoing bwd. rerequesting will propagate. nothing to do right now.
building,pending	~~ invalidate										/ propagate_invalid(this)					~~> building,obsolete.
building,obsolete 	~~ success 											/ build_obsolete(this) 						~~> idle,obsolete.
building,_ 			~~ req(T,C), \+ c(_,C:bld(_),this)					/ add_req(T,C,this).
building,_ 			~~ req(T,C), c(_,C:bld(_),this) 					/ add_bwd(T,C,this).
building,_ 			~~ rel(T,_) 										/ put_dep(T,this).
building,_ 			~~ error(E) 										/ report_error(this,E) 						~~> idle,obsolete.
reading,obsolete	~~ rel(T,C), c(_,_:lck,this)=1, c(T2,C2:req,this) 	/ rem_lck(T,C,this),start_bld(T2,C2,this)	~~> building,pending.
reading,obsolete	~~ rel(T,C), c(_,_:lck,this)=1, \+ c(_,_:req,this) 	/ rem_lck(T,C,this)							~~> idle,obsolete.
reading,obsolete	~~ req(T,C)											/ add_req(T,C,this).
reading, consistent	~~ rel(T,C), c(_,_:lck,this)=1 						/ rem_lck(T,C,this)							~~> idle,consistent.
reading,_			~~ req(T,C) 										/ add_lck(T,C,this).
reading,_			~~ rel(T,C), c(_,_:lck,this)>1 						/ rem_lck(T,C,this).
_,obsolete			~~ invalidate.


