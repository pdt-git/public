:- module(literals,[test_find_literals/3,msf/7,fp_run/0]).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).
:- use_module(library('builder/targets/ast')).
:- use_module(library('builder/targets/interprete')).
:- use_module(library('builder/targets/parse')).

/*
This module tries to find all literals (i.e. atomic formulas) occuring in the program.
It does so matching a set of so called MSF-Rules (MSF= maximal sub-formula) against each
known formula, possibly revealing subformulas in within this formula.
This process is repeated until no more formulas are found. In particula any formula for which 
there is no matching MSF-Rule  is considered atomic, i.e., a literal.

A MSF-Rule is basically a tupel (Mod0,Pattern,Mod1,Subgoal) stating that
any formula encountered in the context of Mod0 and matching Pattern has a maximal 
subformula Subgoal that is to be interpreted in the context of Mod1.   



)
My approach is different from the others I examined so far in two things:
- instead of claiming to solve an unsolveable(?) problem, I explicitly compute 
  an upper and lower bound to the correct solution of this problem. 
- By employing a necessary and a sufficient criterion for the detection of UDMPs, I
  can dynamically expand the rule sets for both upper and lower bound, thus coming 
  to a much more precise result through a simple fix point iteration.
- Other than most (not all) other solutions I saw so far, my approach is aware of modules.  
*/


:- pdt_define_context(
	rule(
		pattern,
		context,
		subformula,
		subcontext,
		bound,
		id %id of the fact this rule is based on, or 'builtin'
	)
).


:- pdt_define_context(
	cx(
		program, % the examined program
		 
		 % the name of the module in the context of which the next formula should be
		 % analized.
		context, 
		
		 % The pef id of the ast node representing the head of the current clause
		head,
		
		% The pef id of the predicate beeing analized.
		predicate,
		
		 % The name of the module in the context of which the predicate is 
		 % analized. This is always a variable. It should neither be bound nore replaced. 
		initial_context,
		
		% The boundary that is currently exanded. (Either 'upper' or 'lower')
		bound,
		
		% the id of the last matched rule
		last_rule,
		
		% either the id of a predicate
		% or 'builtin', or 'unresolved'
		binding
	)
).
    
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).
:- use_module(library('builder/targets/parse')).

%% find_literal(+Goal,+Cx0,-Literal).
% discover literals in and/or derive new MSF-Rules from a given goal and set of MSF-Rules.
% 
% @param Goal the id of an ast node representing a goal.
% @param Cx0 the context to start with (see above).
% @param Literal will be unified either with literal(Id) where Id is the Id of a subtree of Goal,
%                or with msf_rule(Prog,Module0,Formula0,Module1,Formula1,Bound)
find_literal(Goal,Cx,rule(Rule)):-
	pef_variable_occurance_query([id=Goal,variable=VarId]),
	!,	
	% Goal represents a variable. 
	% We will add a new msf rule.
	make_rule(Var,Cx,[VarId=Var|_],Rule).
find_literal(Goal,Cx0,Literal):-        	
	resolve(Goal,Cx0,Cx1),	    	
	(	match(Goal,Cx1,Subst,SubGoal,Cx2)
	*-> % Soft cut, since we DO want backtracking in the condition part.
	    % If there is a match *at all*, and if it is due to a user-defined 
	    % meta-predicate, we have 
	    (	cx_last_rule(Cx2,Id),
	    	Id \== builtin,
	    	Literal=meta_call(Cx2,Goal)
	    ;	find_literal__match_found(Subst,SubGoal,Cx2,Literal)
	    )
	    	    
	;	% if there is no match, then Goal is an atomic formular
	    % i.e. a literal.
		Literal=literal(Cx2,Goal)
	).
	
find_literal__match_found(Subst,SubGoal,Cx,rule(Rule)):-    
    var(SubGoal),
    !,
    % If subgoal is unbound, this means that there was a match,
    % but that the pattern is NOT more general than the matched goal.
    % We cannot continue, (there is nothing to continue with!) 
    % but we can add a new MSF-Rule to the upper bound.
    make_rule(SubGoal,Cx,Subst,Rule).
find_literal__match_found(Subst,SubGoal,Cx1,Literal):-        
    % Subgoal is bound, we can continue.
    % Depending on whether Subst is empty or not, we are either
    % extending the lower or the upper bound.
    (	var(Subst) % (an open list is empty, if it is a variable)
    ->  Cx2 = Cx1
    ;	cx_set_bound(Cx1,upper,Cx2)
    ),
    find_literal(SubGoal,Cx2,Literal).
    
%% match(+Goal,+Cx0,-Subst,-SubGoal,-Cx1)
% Try to find and apply a matching MSF-Rule for a given Goal.
%
% @param Goal the pef id of an AST node representing a goal.
% @param Cx0 the current context
% @param Subst will be unified with a list of (variable_id=Pattern) pairs if the 
%        Pattern is NOT more general than the matched Goal.
% @param SubGoal will always be a subterm of Goal. If Goal is a pef_variable, Goal 
%        and SubGoal will be identical. 
% @param Cx1 will be unified with possibly updated context. Fields that may have changed
%        are module and bound
match(Goal,Cx,[],Goal,Cx):-
    pef_variable_occurance_query([id=Goal]),
    !.
match(Goal,Cx0,Subst,Subformula,CxOut):-
    find_rule(Goal,Cx0,Rule,Cx1),    
    rule_get(Rule,
    	[	pattern=Pattern,    		
    		subformula=Subformula2,
    		subcontext=Subcontext2,
    		bound=Bound
		]
	),    	
	ast_match(Pattern,Goal,Subst),
	(	Bound==lower
	->	cx_bound(Cx1,Bound0)
	;	Bound0=upper
	),
	(	Subformula2==any
	->	Bound1=upper,
		ast_subterm(Goal,_,Subformula),
		Subformula \== Goal
	;	nonvar(Subformula2), 
		Subformula2 = '$var'(Bindings)
	->  has_tail([],Bindings),
		Bound1=Bound0,
		member(Subformula,Bindings)
	;	Bound1=upper,
		Subformula=Subformula2
	),
	(	nonvar(Subcontext2), 
		Subcontext2 = '$var'(ModBindings)
	->  has_tail([],ModBindings),
		Bound2=Bound1,
		memberchk(ModTerm,ModBindings),
		apply(ModTerm,_,Subcontext)		
	;	Subcontext=Subcontext2,
		Bound2=upper
	),
	cx_set(Cx1,[bound=Bound2,context=Subcontext],CxOut).



find_rule(Goal,Cx0,Rule,Cx1):-
	pef_term_query([id=Goal,name=Name,arity=Arity]),
    rule_new(Rule),
    rule_get(Rule,	[	pattern=Pattern,	
						context=Context,
    					subformula=Subformula2,
    					subcontext=Subcontext,
    					bound=Bound,
    					id=RuleId
    	    		]
    ),
    functor(Pattern,Name,Arity),
    cx_binding(Cx0,Binding),
	(	Binding==unresolved
	->	throw(unresolved_predicate_reference(Goal,Cx0))
	;	Binding==builtin
	->  call(Rule)
	;	cx_binding(Cx0,Pred),
		cx_context(Cx0,Context),
    	cx_program(Cx0,Program),
    	pef_msf_rule_query([	predicate=Pred,
    							program=Program,    
    							pattern=Pattern,	
    							context=Context,
    							subformula=Subformula2,
    							subcontext=Subcontext,
    							bound=Bound					
    						])	
    ),
    cx_set_last_rule(Cx0,RuleId,Cx1).	    	




    

%% apply(+Ast,+Subst,-Term)
% renders an ast into a term, applying a given variable substitution.
%
% @param Ast the pef id of an AST node.
% @param Subst an __open__ list of VarId=Value pairs.
% @param Term will be unified with the rendered term.
apply(Ast,Subst,Term):-
    pef_variable_occurance_query([id=Ast,variable=Var]),
    !,
    memberchk(Var=Term,Subst).
apply(Ast,Subst,Term):-
    pef_term_query([id=Ast,name=Name,arity=Arity]),
    functor(Term,Name,Arity),
    apply_args(Arity,Ast,Subst,Term).

apply_args(0,_,_,_).
apply_args(I,Ast,Subst,Term):-
	pef_arg_query([num=I,parent=Ast,child=ArgAst]),
	arg(I,Term,ArgTerm),
	apply(ArgAst,Subst,ArgTerm),
	J is I -1,
	apply_args(J,Ast,Subst,Term).


%% make_rule(+Goal,+Cx,+Subst, -Rule) 
% creates a pef_msf_rule term.
% Goal is the pef id of an ast node representing the subgoal.
% Cx is the current context record
% Subst is an open list of Id=Term  pairs, as produced by ast_match/3
% Rule will be unified with the created rule term.
make_rule(Goal,Cx,Subst,  Rule):-
    cx_program(Cx,Program),
    cx_predicate(Cx,Predicate),
    cx_head(Cx,Head),    
    cx_context(Cx,Subcontext),
    cx_bound(Cx,Bound),
    cx_initial_context(Cx,InitialContext),
    apply(Head,Subst,Pattern),       
    pef_msf_rule_program(Rule,Program),
    pef_msf_rule_predicate(Rule,Predicate),
    pef_msf_rule_pattern(Rule,Pattern),
    pef_msf_rule_subformula(Rule,Goal),
    pef_msf_rule_context(Rule,InitialContext),
    pef_msf_rule_subcontext(Rule,Subcontext),
    pef_msf_rule_bound(Rule,Bound).
    %FIXME: add handling of upper bound rules ('any' subformula)


assert_rule(Rule):-
    pef_reserve_id(pef_msf_rule,Id),
    pef_msf_rule_program(Rule,Program),
    pef_msf_rule_predicate(Rule,Predicate),
    pef_msf_rule_pattern(Rule,Pattern),
    pef_msf_rule_subformula(Rule,Goal),
    pef_msf_rule_context(Rule,InitialContext),
    pef_msf_rule_subcontext(Rule,Subcontext),
    pef_msf_rule_bound(Rule,Bound),
    pef_msf_rule_assert([
    				id=Id,
    				program=Program,
    				predicate=Predicate,
    				pattern=Pattern,
    				subformula=Goal,
    				context=InitialContext,
    				subcontext=Subcontext,
    				bound=Bound
    			 ]).



resolve(Goal,Cx0,Cx1):-    
	cx_program(Cx0,Program),
	cx_context(Cx0,Context),
	cx_bound(Cx0,Bound0),
	pef_term_query([id=Goal,name=Name,arity=Arity]),
	(	resolve(Program,Context,Name,Arity,Bound0,Pred,Bound1)
	*-> cx_set(Cx0,[bound=Bound1,binding=Pred],Cx1)
	;	functor(Head,Name,Arity),
		predicate_property(Head,imported_from(system))
	->	cx_set(Cx0,[bound=Bound0,binding=builtin],Cx1)
	;	cx_set(Cx0,[bound=Bound0,binding=unresolved],Cx1)
	).    


resolve(Program,Context,Name,Arity,Bound0,Pred,Bound1):-
	(	var(Context)
	->  pef_predicate_query([name=Name,arity=Arity,id=Pred,module=Module]),
		pef_program_module_query([program=Program,module=Module]),
		Bound1=upper
	;	resolve_module(Program,Context,MID),
		resolve_predicate(Program,MID,Name,Arity,Pred),
		Bound1=Bound0
	).

msf(Program,Context,Formula,Bound0,Subcontext,Subformula,BoundOut):-
    nonvar(Formula),
    functor(Formula,Name,Arity),
	find_rule(Program,Name,Arity,Context, Bound0, Rule,Bound1),
	rule_pattern(Rule,Formula),
	rule_context(Rule,Context),
	rule_subformula(Rule,Subformula),
	rule_subcontext(Rule,Subcontext),
	rule_bound(Rule,Bound),
	(	Bound==upper
	->	BoundOut=upper
	;	BoundOut=Bound1
	).

rule( (A , _),	M, A, M,lower,builtin).
rule( (_ , A),	M, A, M,lower,builtin).
rule( (A ; _),	M, A, M,lower,builtin).
rule( (_ ; A),	M, A, M,lower,builtin).
rule( (A | _),	M, A, M,lower,builtin).
rule( (_ | A),	M, A, M,lower,builtin).
rule( (A -> _),	M, A, M,lower,builtin).
rule( (_ -> A),	M, A, M,lower,builtin).
rule( ( \+ A ),	M, A, M,lower,builtin).
rule( ( call(A) ),	M, A, M,lower,builtin).
rule( ( not(A) ),	M, A, M,lower,builtin).
rule( ( once(A) ),	M, A, M,lower,builtin).
rule( ( ignore(A) ),	M, A, M,lower,builtin).
rule( ( call_with_depth_limit(A,_,_) ),	M, A, M,lower,builtin).
rule( ( call_cleanup(A,_,_) ),	M, A, M,lower,builtin).
rule( ( call_cleanup(_,_,A) ),	M, A, M,lower,builtin).
rule( ( call_cleanup(A,_) ),	M, A, M,lower,builtin).
rule( ( call_cleanup(_,A) ),	M, A, M,lower,builtin).
rule( ( setup_and_call_cleanup(A,_,_) ),	M, A, M,lower,builtin).
rule( ( setup_and_call_cleanup(_,A,_) ),	M, A, M,lower,builtin).
rule( ( setup_and_call_cleanup(_,_,A) ),	M, A, M,lower,builtin).
rule( ( catch(A,_,_) ),	M, A, M,lower,builtin).
rule( ( catch(_,_,A) ),	M, A, M,lower,builtin).
rule( (A *-> _),	M, A, M,lower,builtin).
rule( (_ *-> A),	M, A, M,lower,builtin).
rule( (M : A),	_, A, M,lower,builtin).
rule( forall(A,_),	M, A, M,lower,builtin).
rule( forall(_,A),	M, A, M,lower,builtin).
rule( findall(_,A,_),	M, A, M,lower,builtin).
rule( findall(_,A,_,_),	M, A, M,lower,builtin).
rule( setof(_,A,_),	M, A, M,lower,builtin).
rule( bagof(_,A,_),	M, A, M,lower,builtin).




test_find_literals(Tl,Prog,Lit):-
    pef_toplevel_query([id=Tl,file=FID]),
    get_pef_file(File,FID),
    pdt_with_targets([interprete(File),ast(Tl)],
	    (	pef_clause_query([toplevel=Tl,predicate=Pred]),    
		    predicate_owner(Pred,Prog),
		    
		    cx_new(Cx),
		    cx_program(Cx,Prog),
		    cx_predicate(Cx,Pred), 
		    (	pef_predicate_property_definition_query([predicate=Pred,property=module_transparent])
		    ->	true
		    ;	pef_predicate_query([id=Pred,module=MID]),
		    	module_name(MID,Context)
		    ),
		    cx_context(Cx,Context),
		    cx_initial_context(Cx,Context),
		    pef_ast_query([toplevel=Tl,root=Root]),
		    ast_head_body(Root,_,Head,Body),
			cx_head(Cx,Head),	    		
			cx_bound(Cx,lower),
		    find_literal(Body,Cx,Lit)
	    )
    ).


fp_init_todo(Cx,Goal):-
    pef_clause_query([predicate=Pred,toplevel=Tl]),
    predicate_owner(Pred,Prog),
	(	pef_predicate_property_definition_query([predicate=Pred,property=module_transparent])
    ->	true
    ;	pef_predicate_query([id=Pred,module=MID]),
    	module_name(MID,Context)
    ),
    pef_ast_query([toplevel=Tl,root=Root]),
    ast_head_body(Root,_,Head,Goal),	
    cx_new(Cx),
	cx_program(Cx,Prog),
	cx_predicate(Cx,Pred),
	cx_initial_context(Cx,_),
	cx_context(Cx,Context),
    cx_head(Cx,Head),
    cx_bound(Cx,lower).


fp_init_todo_test(Cx,Goal):-
    get_pef_file(library('pdt/builder/targets/literals-test_input.pl'),FID),
    pef_clause_query([predicate=Pred,toplevel=Tl]),
    pef_toplevel_query([id=Tl,file=FID]),
    predicate_owner(Pred,Prog),
	(	pef_predicate_property_definition_query([predicate=Pred,property=module_transparent])
    ->	true
    ;	pef_predicate_query([id=Pred,module=MID]),
    	module_name(MID,Context)
    ),
    pef_ast_query([toplevel=Tl,root=Root]),
    ast_head_body(Root,_,Head,Goal),	
    cx_new(Cx),
	cx_program(Cx,Prog),
	cx_predicate(Cx,Pred),
	cx_initial_context(Cx,_),
	cx_context(Cx,Context),
    cx_head(Cx,Head),
    cx_bound(Cx,lower).

    
fp_init_todos:-
    forall(fp_init_todo_test(Cx,Goal),recordz(fp_todo,todo(Cx,Goal))).
    
fp_run:-
    fp_init_todos,
	forall(
		recorded(fp_todo,todo(Cx,Goal),Ref),
		(	(	erase(Ref)
			->	true
			;	throw(failed(erase(Ref)))
			),
			(	find_literal(Goal,Cx,Result)
			->	true
			;	throw(failed(find_literal(Goal,Cx,Result)))
			),
			(	fp_process_result(Result)
			->	true
			;	throw(failed(fp_process_result(Result)))
			)
		)
	).  
    

fp_process_result(rule(Rule)):-
    assert_rule(Rule),
    pef_msf_rule_predicate(Rule,Predicate),
    forall(
    	pef_call_query([predicate=Predicate,goal=Goal, cx=Cx]),
    	recordz(fp_todo,todo(Cx,Goal))
    ).
fp_process_result(literal(Cx,Goal)):-
    pef_reserve_id(pef_call,Id),
    cx_predicate(Cx,Predicate),
    pef_call_assert([id=Id,goal=Goal,cx=Cx,predicate=Predicate]).
fp_process_result(meta_call(Cx,Goal)):-
	pef_reserve_id(pef_call,Id),
    cx_predicate(Cx,Predicate),
    pef_call_assert([id=Id,goal=Goal,cx=Cx,predicate=Predicate]).

	/*
	-----------
	Initialisiere TODO = alle Prädikate im Programm
	
	while TODO nicht leer:
	  für jedes Predikat in TODO,
  	    - binde gefundene literale an aufgerufene Prädikate 
	    - sammele neue MSF-Regeln
	  Lege neue Regeln an.
	  Ersetze TODO <- alle Prädikate, die in neuen Regeln auftauchen.
	------------
	
	Fragen: 
	 - kann ich beide Phasen zusammenfassen? (würde Algo vereinfachen)
	*/	