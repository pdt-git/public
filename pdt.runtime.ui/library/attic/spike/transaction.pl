

/*

semantics (partly implemented)
 - 

current limitations
 - only works correctly with single literal facts, no clauses, no conjunctions, etc..
   (no idea yet how to handle cut in bodies)
 - only retractall/1 can be used, not retract/1.
   ( makes things A LOT easier )
 - limited module support. clauses added to imported predicates are not visible during 
   the transaction.
   
   
todo
 - should use double-linking right away.    

*/




:- module(pdt_transaction,
		[	pdt_transaction/1,
			pdt_assert/1,
			pdt_retractall/1,
			pdt_query/1,
			pdt_bot/0,
			pdt_commit/0
		]
	).


:- use_module(library('/org/cs3/pdt/util/pdt_util_context')).

:- pdt_define_context(delta(id,action,fact,base,fwd)).
:- module_transparent 
	pdt_transaction/1,
	pdt_assert/1,
	pdt_retractall/1,
	pdt_query/1.


	
pdt_transaction(Goal):-
    context_module(M),
    pdt_transaction(M,Goal).
pdt_transaction(M,Goal):-
	bot,
	M:Goal,
	commit.


pdt_bot:-
    bot.
pdt_commit:-
	commit.    
    
pdt_assert(Fact):-
    context_module(M),
    pdt_assert(M,Fact).

pdt_assert(M,Fact):-
	resolve(M,Fact,QFact),
   	update(assert,QFact).

    
pdt_retractall(Fact):-
    context_module(M),
    pdt_retractall(M,Fact).

pdt_retractall(M,Fact):-
    resolve(M,Fact,QFact),
    update(retractall,QFact).
    
pdt_query(Fact):-
    context_module(M),
    pdt_query(M,Fact).

pdt_query(M,Fact):-
    resolve(M,Fact,QFact),
    query(QFact).
    
level(Next,Current):-
    nb_current(delta_level,Current),
    Current \== [],
    !,    
    NextLevel is Next,
    b_setval(delta_level,NextLevel).
level(Next,0):-    
    NextLevel is Next,
	b_setval(delta_level,NextLevel).

bot:-
   	level(L+1,L),
	update(bot,[]).
   
update(Action,Fact):-
	(	nb_current(delta,Base)
	;	Base=[]
	),
	!,
	delta_new(Delta),
	gensym(delta,S),
	delta_id(Delta,S),
	delta_action(Delta,Action),
	delta_fact(Delta,Fact),
	delta_base(Delta,Base),
	b_setval(delta,S),
	b_setval(S,Delta).    

%% resolve(+Cx, +Head, -DefMod:StrippedHead)
%
% resolve predicate. Find out which predicate definition a given goal is
% refering to in a given context.
resolve(Cx,Head,Module:Head2):-
    Cx:strip_module(Head,Cx2,Head2),
    %to avoid loading of auto-load predicates (slooooooow).
    % we use  the iso-complient current_predicate/1.
    functor(Head2,Name,Arity),
    Cx2:current_predicate(Name/Arity), 
    !,
    (	Cx2:predicate_property(Head2,imported_from(Module))
    ->	true
    ;	Cx=Module
    ).
    
		   
   
query(Fact):-
	Fact,
	verify(Fact).
query(Fact):-
	added(Fact).
	
%% verify(+Fact)
% succeeds if Fact was not retracted.
verify(Fact):-		
	b_getval(delta,S), 
	b_getval(S,Delta),
	!,
	\+ retracted(Delta,Fact).
verify(_).	

%% retracted(+Delta, +Fact).
% succeeds if Delta retracts a term that unifies with Fact.
retracted(Delta,Fact):-
    delta_action(Delta,retractall),
    delta_fact(Delta,Fact).
retracted(Delta,Fact):-
    \+ delta_action(Delta,bot),
    delta_base(Delta,Base),
    b_getval(Base,Delta2),
    retracted(Delta2,Fact).
    
added(Fact):-
    b_getval(delta,S), 
	b_getval(S,Delta),
	added(Delta,Fact).

added(Delta,Fact):-
    delta_action(Delta,retractall),
    delta_fact(Delta,Fact),
    !,
    fail.
added(Delta,Fact):-
    \+ delta_action(Delta,bot),
    delta_base(Delta,Base),
    b_getval(Base,Delta2),
    added(Delta2,Fact).
added(Delta,Fact):-
    delta_action(Delta,assert),
    delta_fact(Delta,Asserted),
    % need to copy to avoid further instantiation of global var.
    copy_term(Asserted,Fact).   

commit:-
    update(commit,[]),
    b_getval(delta,S), 
	b_getval(S,Delta),
	!,
	level(L-1,L),
	(	L == 1
	->	add_forward_links(Delta, BOT),
		delta_id(BOT,Id),
		delta_fwd(BOT,Fwd),
		nb_delete(delta),
		delete_and_commit_next(Id,Fwd)
	;	true
	).
    
commit.    

commit(assert,Fact,Id,Fwd):-
    assert(Fact),
    delete_and_commit_next(Id,Fwd).
commit(retractall,Fact,Id,Fwd):-
    retractall(Fact),
    delete_and_commit_next(Id,Fwd).
commit(commit,_Fact,Id,Fwd):-
    delete_and_commit_next(Id,Fwd).
    



delete_and_commit_next(Id0,Fwd0):-
    nb_delete(Id0),
    (	nonvar(Fwd0)
    ->  b_getval(Fwd0,Delta),
		delta_id(Delta,Id),
	    delta_action(Delta,Action),
		delta_fact(Delta,Fact),
		delta_fwd(Delta,Fwd),
		commit(Action,Fact,Id,Fwd)
	;	true
	).
    

add_forward_links(BOT, BOT):-
    delta_base(BOT,Base),
    Base==[],
    !.
add_forward_links(Delta,BOT):-
	delta_base(Delta,BaseId),
	delta_id(Delta,Id),
	b_getval(BaseId,BaseDelta),
	delta_fwd(BaseDelta,Id),
	add_forward_links(BaseDelta,BOT).



    