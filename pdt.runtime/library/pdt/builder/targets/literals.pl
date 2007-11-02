:- module(literals,[fp_run/0]).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).
:- use_module(library('builder/targets/ast')).
:- use_module(library('builder/targets/interprete')).
:- use_module(library('builder/targets/parse')).
:- use_module(library('util/ast_util')).

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
		substitution,
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
		binding,
		
		% not really required. I currently use it for debugging.
		toplevel
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
    ast_variable_occurance(Goal,VarId),
	ast_apply(Goal,_,Dbg),
	format("looking at a variable: ~w~n",[Dbg]),
    
	%pef_variable_occurance_query([id=Goal,variable=VarId]),
	!,	
	% Goal represents a variable. 
	% We will add a new msf rule.
	make_rule(Var,Cx,[VarId=Var|_],Rule).
find_literal(Goal,Cx0,Literal):- 
	ast_apply(Goal,_,Dbg),
	format("looking at ~w~n",[Dbg]),       	
	resolve(Goal,Cx0,Cx1),	    	
	(	cx_binding(Cx1,unresolved)
	->	Literal=unresolved(Cx1,Goal)	
	;	match(Goal,Cx1,Subst,RuleSubst,SubGoal,Cx2)
	*-> % Soft cut, since we DO want backtracking in the condition part.
	    % If there is a match *at all*, and if it is due to a user-defined 
	    % meta-predicate, we have 
	    (	cx_last_rule(Cx2,Id),
	    	Id \== builtin,
	    	Literal=meta_call(Cx2,Goal)
	    ;	find_literal__match_found(Goal,Subst,RuleSubst,SubGoal,Cx2,Literal)
	    )
	    	    
	;	% if there is no match, then Goal is an atomic formular
	    % i.e. a literal.
		Literal=literal(Cx1,Goal)
	).
	
find_literal__match_found(Goal,Subst,RuleSubst,SubGoal,Cx,Literal):-    
    
    /* check bound
    if the subgoal contains any variable 
    */
    
    /*
    we can only continue with the found subgoal, if it is
    sufficiently instantiated.
    We say it is sufficently instantiated if each Variable is attachedto
    a physical AST node. (variable occurances are ok)
    
    ast_attach (which we need to call anyway) fails for var=var pairs. 
    We can use this effect to determine if all necessary variables 
    where bound by the match. Not elegant, but efficient.
    */
    (	ast_attach(RuleSubst)
	     
	->  % Subgoal is sufficiently instantiated, we can continue.
	    % Depending on whether Subst is empty or not, we are either
	    % extending the lower or the upper bound.
	    (	var(Subst) % (an open list is empty, if it is a variable)
	    ->  Cx2 = Cx
	    ;	cx_set_bound(Cx,upper,Cx2)
	    ),
	    find_literal(SubGoal,Cx2,Literal)
	;   ast_apply(SubGoal,_,Dbg),
		format("stoped here: ~w~n",[Dbg]),
		% Subgoal is not sufficiently instantiated.
	    % We cannot continue, (there is nothing to continue with!) 
	    % but we can add a new MSF-Rule .
	    ast_apply(Goal,Subst2,GoalTerm),	   
	    make_rule(GoalTerm,Cx,Subst2,Rule),
	    Literal=rule(Rule)
   	    
	).
    
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
match(Goal,Cx,[],[],Goal,Cx):-
    ast_variable_occurance(Goal,_),
    %pef_variable_occurance_query([id=Goal]),
    !.
match(Goal,Cx0,Subst,RuleSubst,Subformula,CxOut):-
    find_rule(Goal,Cx0,Rule,Cx1),    
    rule_get(Rule,
    	[	pattern=Pattern,    		
    		subformula=Subformula2,
    		subcontext=Subcontext2,
    		substitution=RuleSubst,
    		bound=Bound
		]
	),    
	%ast_apply(Goal,_,GoalTerm),
	%format("matching pattern:~w, goal:~w~n",[Pattern,GoalTerm]),	
	ast_match(Pattern,Goal,Subst),
	%format("success! pattern:~w, subst:~w~n",[Pattern,Subst]),
	
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
    ast_functor(Goal,Name,Arity),
	%pef_term_query([id=Goal,name=Name,arity=Arity]),
    rule_new(Rule),
    rule_get(Rule,	[	pattern=Pattern,	
						context=Context,
    					subformula=Subformula2,
    					substitution=RuleSubst,
    					subcontext=Subcontext,
    					bound=Bound,
    					id=RuleId
    	    		]
    ),
    functor(Pattern,Name,Arity),
    cx_binding(Cx0,Binding),
	(	Binding==unresolved
	->  throw(unresolved(Goal,Cx0))		
	;	Binding==builtin
	->  cx_context(Cx0,Context),
		call(Rule)
	;	cx_binding(Cx0,Pred),
		cx_context(Cx0,Context),
    	cx_program(Cx0,Program),
    	pef_msf_rule_query([	predicate=Pred,
    							program=Program,    
    							pattern=Pattern,	
    							context=Context,
    							subformula=Subformula2,
    							substitution=RuleSubst,
    							subcontext=Subcontext,
    							bound=Bound					
    						])	
    ),
    cx_set_last_rule(Cx0,RuleId,Cx1).	    	




    



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
    ast_apply(Head,Subst,Pattern),   
    !,
    term_variables(Goal,GoalVars),
    copy_term(Goal-GoalVars,Goal2-GoalVars2),
    unifiable(GoalVars,GoalVars2,RuleSubst),
    pef_msf_rule_program(Rule,Program),
    pef_msf_rule_predicate(Rule,Predicate),
    pef_msf_rule_pattern(Rule,Pattern),
    pef_msf_rule_subformula(Rule,Goal2),
    pef_msf_rule_substitution(Rule,RuleSubst),
    pef_msf_rule_context(Rule,InitialContext),
    pef_msf_rule_subcontext(Rule,Subcontext),
    pef_msf_rule_bound(Rule,Bound).
    %FIXME: add handling of upper bound rules ('any' subformula)


rule_exists(NewRule,ExistingId,Result):-
    rule_test_term(NewRule,NewTerm),
    pef_msf_rule_predicate(NewRule,Predicate),
	pef_msf_rule_query([predicate=Predicate,id=ExistingId],ExistingRule),
	rule_test_term(ExistingRule,ExistingTerm),
	(	subsumes(ExistingTerm,NewTerm)
	->	Result=more_general
	;	subsumes(NewTerm,ExistingTerm)
	->	Result=more_special
	;	fail
	).
	
assert_rule(Rule):-
    (	rule_exists(Rule,Id,more_special)
    ->	pef_msf_rule_retractall([id=Id])
    ;	rule_exists(Rule,Id,more_general)
    ->	fail
    ;   pef_msf_rule_program(Rule,Program),
	    pef_msf_rule_predicate(Rule,Predicate),
	    pef_msf_rule_pattern(Rule,Pattern),
	    pef_msf_rule_subformula(Rule,Goal),
	    pef_msf_rule_substitution(Rule,RuleSubst),
	    pef_msf_rule_context(Rule,InitialContext),
	    pef_msf_rule_subcontext(Rule,Subcontext),
	    pef_msf_rule_bound(Rule,Bound),
    	pef_reserve_id(pef_msf_rule,Id),
    	pef_msf_rule_assert([
    				id=Id,
    				program=Program,
    				predicate=Predicate,
    				pattern=Pattern,
    				subformula=Goal,
    				context=InitialContext,
    				subcontext=Subcontext,
    				bound=Bound,
    				substitution=RuleSubst
    	])    
    ).



resolve(Goal,Cx0,Cx1):-    
	cx_program(Cx0,Program),
	cx_context(Cx0,Context),
	cx_bound(Cx0,Bound0),
	ast_functor(Goal,Name,Arity),
	%pef_term_query([id=Goal,name=Name,arity=Arity]),
	(	resolve(Program,Context,Name,Arity,Bound0,Pred,Bound1)
	*-> cx_set(Cx0,[bound=Bound1,binding=Pred],Cx1)
	;	functor(Head,Name,Arity),
		predicate_property(Head,built_in)
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


rule( (A , _),	M, AA, M,[A=AA],lower,builtin).
rule( (_ , A),	M, AA, M,[A=AA],lower,builtin).
rule( (A ; _),	M, AA, M,[A=AA],lower,builtin).
rule( (_ ; A),	M, AA, M,[A=AA],lower,builtin).
rule( (A | _),	M, AA, M,[A=AA],lower,builtin).
rule( (_ | A),	M, AA, M,[A=AA],lower,builtin).
rule( (A -> _),	M, AA, M,[A=AA],lower,builtin).
rule( (_ -> A),	M, AA, M,[A=AA],lower,builtin).
rule( ( \+ A ),	M, AA, M,[A=AA],lower,builtin).
rule( ( call(A) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( not(A) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( once(A) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( ignore(A) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( call_with_depth_limit(A,_,_) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( call_cleanup(A,_,_) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( call_cleanup(_,_,A) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( call_cleanup(A,_) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( call_cleanup(_,A) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( setup_and_call_cleanup(A,_,_) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( setup_and_call_cleanup(_,A,_) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( setup_and_call_cleanup(_,_,A) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( catch(A,_,_) ),	M, AA, M,[A=AA],lower,builtin).
rule( ( catch(_,_,A) ),	M, AA, M,[A=AA],lower,builtin).
rule( (A *-> _),	M, AA, M,[A=AA],lower,builtin).
rule( (_ *-> A),	M, AA, M,[A=AA],lower,builtin).
rule( (M : A),	_, AA, M,[A=AA],lower,builtin).
rule( forall(A,_),	M, AA, M,[A=AA],lower,builtin).
rule( forall(_,A),	M, AA, M,[A=AA],lower,builtin).
rule( findall(_,A,_),	M, AA, M,[A=AA],lower,builtin).
rule( findall(_,A,_,_),	M, AA, M,[A=AA],lower,builtin).
rule( setof(_,A,_),	M, AA, M,[A=AA],lower,builtin).
rule( bagof(_,A,_),	M, AA, M,[A=AA],lower,builtin).








fp_init_todo(Cx,Goal):-
    pef_clause_query([predicate=Pred,toplevel=Tl]),
    predicate_owner(Pred,Prog),
	(	pef_predicate_property_definition_query([predicate=Pred,property=module_transparent])
    ->	cx_initial_context(Cx,Context)
    ;	pef_predicate_query([id=Pred,module=MID]),
    	module_name(MID,Context)
    ),
    ast_root(Tl,Root),
    ast_head_body(Root,_,Head,Goal),
    Goal \== [], Head \== [],	
    cx_new(Cx),
    cx_toplevel(Cx,Tl),
	cx_program(Cx,Prog),
	cx_predicate(Cx,Pred),	
	cx_context(Cx,Context),
    cx_head(Cx,Head),
    cx_bound(Cx,lower).



    
fp_init_todos:-
    forall(fp_init_todo(Cx,Goal),recordz(fp_todo,todo(Cx,Goal))).


fp_run:-
    fp_init_todos,
	repeat,
		fp_step,
		fp_done,
	!.

fp_step:-fp_step1,fp_step2,fp_step3.
    
fp_step1:-
    forall(
		recorded(fp_todo,Todo,Ref),
		(	format("processing: ~w~n",[Todo]),
			fp_process_todo(Todo),
			erase(Ref)			
		)
	).
fp_step2:-	
	forall(
		recorded(fp_result,Result,Ref),
		(	format("processing: ~w~n",[Result]),
			fp_process_result(Result),
			erase(Ref)			
		)
	).
fp_step3:-
	forall(
		recorded(fp_touched,Predicate,Ref),
		(	format("processing: ~w~n",[Predicate]),
			fp_process_touched(Predicate),
			erase(Ref)
		)
	).
    
    
fp_done:- \+ recorded(fp_todo,_).

fp_process_todo(todo(Cx,Goal)):-
    format("processing: ~w~n",[todo(Cx,Goal)]),
    forall(
    	find_literal(Goal,Cx,Result),
		recordz(fp_result,Result)	
	).


fp_process_touched(Predicate):-
	forall(
    	pef_call_query([predicate=Predicate,goal=Goal, cx=Cx]),
    	(	cx_head(Cx,Head),
    		ast_attach([Goal=GoalAst,Head=HeadAST]),
    		cx_set_head(Cx,HeadAST,Cx1),
    		recordz(fp_todo,todo(Cx1,GoalAst))
    	)
    ).
	
fp_process_result(rule(Rule)):-
    (	assert_rule(Rule)
    ->	pef_msf_rule_predicate(Rule,Predicate),
    	recordz(fp_touched,Predicate)
    ;	true
    ).
    
fp_process_result(literal(Cx,Goal)):-
    pef_reserve_id(pef_call,Id),
    cx_binding(Cx,Predicate),
    cx_head(Cx,Head),
    ast_node(Head,HeadNode),
    (	ast_node(Goal,GoalNode)
    ->	true
    ;	GoalNode=[]
    ),
    cx_set_head(Cx,HeadNode,Cx1),
    (	pef_call_query([goal=GoalNode,predicate=Predicate])
    ->  true
    ;	pef_call_assert([id=Id,goal=GoalNode,cx=Cx1,predicate=Predicate])
    ).
    
   
fp_process_result(meta_call(Cx,Goal)):-
	pef_reserve_id(pef_call,Id),
    cx_binding(Cx,Predicate),
    cx_head(Cx,Head),
    ast_node(Head,HeadNode),
    (	ast_node(Goal,GoalNode)
    ->	true
    ;	GoalNode=[]
    ),
    cx_set_head(Cx,HeadNode,Cx1),
    (	pef_call_query([goal=GoalNode,predicate=Predicate])
    ->  true
    ;	pef_call_assert([id=Id,goal=GoalNode,cx=Cx1,predicate=Predicate])
    ).
fp_process_result(unresolved(Cx,Goal)):-
    ast_node(Goal,Node),
	cx_context(Cx,Context),
	cx_toplevel(Cx,Tl),
	pef_toplevel_query([id=Tl,file=File]),
	ast_functor(Goal,Name,Arity),	
	get_pef_file(Path,File),			
	format("~w (~w): cannot resolve predicate: ~w/~w, context ~w~n",[Path,Node,Name,Arity,Context]).

spyme.
		
		
		
subsumes(General,Special):-    
    /* not entirely correct, but good enough for our purpose: 
       We simply assume that numbervars creates terms that do not occur in General.
    */
    \+ \+ (numbervars(Special,0,_),General=Special).

rule_test_term(Rule,t(Context,Pattern,Subcontext,Goal,Bound)):-
	copy_term(Rule,Rule2),
	pef_msf_rule_pattern(Rule2,Pattern),
    pef_msf_rule_subformula(Rule2,Goal),
    pef_msf_rule_context(Rule2,Context),
    pef_msf_rule_subcontext(Rule2,Subcontext),
    pef_msf_rule_bound(Rule2,Bound),
    pef_msf_rule_substitution(Rule2,RuleSubst),
    apply_subst(RuleSubst).

apply_subst([]).
apply_subst([S|Ss]):-call(S),apply_subst(Ss).    
    		