:- module(literals,[]).

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
the target literals(Resource) provides the facts of type pef_msf_rule and pef_call.

pef_call describes the relation between literals and the predicates they call.
pef_msf_rule describes the "behaviour" of user-defined meta-predicates.
A meta-predicate is a predicate that decides whether some formula can be proved.
Each rule associates a call pattern of the predicate with the formula the predicate will try to prove.
'MSF' is means "Maximul Sub-Formula", although by now i generalized the approach to allow cases where
the formula is not explicitly occuring int the call.  

the targets defined in this file require fix point calculations.
the granularity of dependencies is that of predicates.
*/

%!!!experimental!!! option.
% either 'file' or 'predicate' 
% Currently 'predicate' is known to work, I want to experiment with 'file'.
granularity(file).

pdt_builder:fix_point_target(literals(predicate(_))):-granularity(predicates).
pdt_builder:fix_point_target(literals(file(_))):-granularity(file).
pdt_builder:target_file(literals(directory(Path,_,_)),Path).
pdt_builder:target_file(literals(file(Path)),Path).
pdt_builder:target_mutable(literals(workspace),true).
pdt_builder:target_mutable(literals(project(_)),true).
pdt_builder:target_mutable(inverse_search(_),true).

pdt_builder:fp_process_hook(find_literals(Goal,Cx)):-
    cx_head(Cx,Head),
    ast_attach([Goal=GoalAst,Head=HeadAST]),
    cx_set_head(Cx,HeadAST,Cx1),  
    findall(
    	Result,
    	find_literal(GoalAst,Cx1,Result),    	
    	Results
    ),
    process_results(Results).



pdt_builder:fp_seed_hook(literals(predicate(Name/Arity))):-
	seed_predicate(Name/Arity).    

pdt_builder:fp_seed_hook(literals(file(Path))):-
	seed_file(Path).    
	
	
	
pdt_builder:build_hook(inverse_search(FunctorName)):-	
	% First use asts as search index to find all predicates with clauses 
	% that continaining the predicates functor symbol.
	% Then, run the cross referencer for all these predicates
	% Finally, lookup the calls.
	
	% build ASTs
	pdt_builder:dump(before),
	pdt_request_target(ast(workspace)),	
	pdt_builder:dump(after),	
	(	granularity(predicate)
	->	% lookup predicate clauses
		forall(
			pef_term_query([name=FunctorName,id=Candidate]),
			(	ast_toplevel(Candidate,Tl),
				ast_root(Tl,Root),
				ast_head_body(Root,_,H,_),
				(	H==[]
				->  true
				;	ast_functor(H,SName,SArity),
					pdt_request_target(literals(predicate(SName/SArity)))			
				)
			)
		)
	;	% lookup file clauses
		forall(
			pef_term_query([name=FunctorName,id=Candidate]),
			(	ast_toplevel(Candidate,Tl),
				pef_toplevel_query([id=Tl,file=File]),
				get_pef_file(Path,File),
				pdt_request_target(literals(file(Path)))							
			)
		)
	).
	
pdt_builder:build_hook(literals(Resource)):-
    (	Resource=file(Path)
    ->	request_file_literals(Path)    
    ;   pdt_request_target(Resource),
    	pdt_contains(Resource,Element),
    	pdt_request_target(literals(Element))
    ).
/*file_predicate(F,P):-
    pef_toplevel_query([file=F,id=Tl]),
    \+ pef_term_expansion_query([original=Tl]), %for expansion: ignore original.
    pef_clause_query([toplevel=Tl,predicate=P]).
  */
file_predicate(File,Name/Arity):-
	pef_program_query([file=File,id=Prog]),
	pef_program_module_query([program=Prog,module=Mod]),
	module_predicate(Mod,Pred),
	pef_predicate_query([id=Pred,name=Name,arity=Arity]).
	
request_file_literals(Path):-
    pdt_request_targets([parse(Path),interprete(Path)]),
    get_pef_file(Path,F),
    /*forall(file_depends(F,OtherF),
    	(	get_pef_file(OtherPath,OtherF),
    		pdt_request_target(literals(file(OtherPath)))
    	)
    ),*/
    forall(
		file_predicate(F,Signature),    		    
    	pdt_request_target(literals(predicate(Signature)))
    ).

seed_predicate(Name/Arity):-    
    %begin debug
    pef_predicate_query([id=Pred,name=Name,arity=Arity,module=Module]),
    module_name(Module,ModuleName),
    debug(literals,"seeding ~w:~w/~w~n",[ModuleName,Name,Arity]),    
    forall(
    	fp_init_todo2(Pred,Cx,Goal),
    	pdt_fp_enqueue(find_literals(Goal,Cx),literals(predicate(Name/Arity)))
    ).

seed_file(Path):-
    pdt_request_targets([parse(Path),interprete(Path),ast(file(Path))]),
    get_pef_file(Path,File),
    forall(
    	file_depends(File,Dep),
    	(	get_pef_file(DepPath,Dep),
    		pdt_request_target(literals(file(DepPath)))
    	)
    ),
    forall(
    	fp_init_todo3(File,Cx,Goal),
    	pdt_fp_enqueue(find_literals(Goal,Cx),literals(file(Path)))
    ).
    

process_results([]).
process_results([Result|Results]):-
    debug(literals,"process result: ~w~n",[Result]),
    fp_process_result(Result),
    process_results(Results).




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
		 % analized, or [] to indicate module transparency
		context, 
		
		 % The pef id of the ast node representing the head of the current clause
		head,
		
		% The pef id of the predicate beeing analized.
		predicate,
				 
		% The boundary that is currently exanded. (Either 'upper' or 'lower')
		bound,		
		
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
find_literal(Goal,Cx,Literal):- %Case 1: Context is a variable
    cx_context(Cx,Context),
    ast_variable_occurance(Context,VarId),
	
	!,	
	% Context is bound to a variable. We cannot continue. 
	% We will add a new msf rule IFF the variable also occurs in the clause head, AND if 
	% all variables appearing in goal apear in head.
	cx_head(Cx,Head),
	(	ast_occurs(Head,VarId),
		forall(ast_occurs(Goal,VarId2),ast_occurs(Head,VarId2))
	->	make_rule(Var,Cx,[VarId=Var|_],Rule),
		Literal=rule(Rule)
	;	Literal=cannot_infer(Cx,Goal)
	).

find_literal(Goal,Cx,Literal):- %Case 2: Goal is a variable
    ast_variable_occurance(Goal,VarId),
	ast_apply(Goal,_,Dbg),
	debug(literals,"looking at a variable: ~w~n",[Dbg]),
    
	%pef_variable_occurance_query([id=Goal,variable=VarId]),
	!,	
	% Goal represents a variable. 
	% We will add a new msf rule IFF the variable also occurs in the clause head
	cx_head(Cx,Head),
	(	ast_occurs(Head,VarId)
	->	make_rule(Var,Cx,[VarId=Var|_],Rule),
		Literal=rule(Rule)
	;	Literal=cannot_infer(Cx,Goal)
	).

find_literal(Goal,Cx0,Literal):- 
	ast_apply(Goal,_,Dbg),
	debug(literals,"looking at ~w~n",[Dbg]),       	
	resolve(Goal,Cx0,Cx1,Pred),	    	
	(	Pred==unresolved
	->	Literal=unresolved(Cx1,Goal)	
	;	match(Pred,Goal,Cx1,Subst,RuleSubst,SubGoal,Cx2)
	*-> % Soft cut, since we DO want backtracking in the condition part.
	    % If there is a match *at all*, and if it is due to a user-defined 
	    % meta-predicate, we have 
	    (	Pred \== builtin,
	    	Literal=meta_call(Cx2,Goal,Pred)
	    ;	find_literal__match_found(Goal,Subst,RuleSubst,SubGoal,Cx2,Literal)
	    )
	    	    
	;	% if there is no match, then Goal is an atomic formular
	    % i.e. a literal.
		Literal=literal(Cx1,Goal,Pred)
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
	;   % Subgoal is not sufficiently instantiated.
	    % We cannot continue, (there is nothing to continue with!) 
	    % but we can add a new MSF-Rule IFF all variables in Goal 
	    % also occur in the clause head, AND if the context is a constant or a variable
	    % appearing in head. (we do not allow infered rules to introduce new variables)
	    cx_head(Cx,Head),
	    cx_context(Cx,Context),
	    forall(ast_occurs(Goal,Var),ast_occurs(Head,Var)),% FIXME avoid redundant checks for variables occuring more than once.
	    ( ast_variable_occurance(Context,Var2) -> ast_occurs(Head,Var2) ; true )
	->	ast_apply(SubGoal,_,Dbg),
		debug(literals,"stopped here: ~w~n",[Dbg]),		
	    ast_apply(Goal,Subst2,GoalTerm),	   
	    make_rule(GoalTerm,Cx,Subst2,Rule),
	    Literal=rule(Rule)
	;	Literal=cannot_infer(Cx,Goal)

   	    
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
match(Pred,Goal,Cx0,Subst,RuleSubst,Subformula,CxOut):-
    find_rule(Pred,Goal,Cx0,Rule),    
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
	->	cx_bound(Cx0,Bound0)
	;	Bound0=upper
	),
	(	nonvar(Subformula2), 
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
		ast_apply(ModTerm,_,Subcontext)
	;	Subcontext=Subcontext2,
		Bound2=Bound1
	),
	/*(	var(Subcontext)
	->	throw(setting_var_context(Subcontext,Goal,Cx0,Rule))
	;	cx_set(Cx0,[bound=Bound2,context=Subcontext],CxOut)
	).*/
	cx_set(Cx0,[bound=Bound2,context=Subcontext],CxOut).




find_rule(Binding,Goal,Cx0,Rule):-
    ast_functor(Goal,Name,Arity),
	%pef_term_query([id=Goal,name=Name,arity=Arity]),
    rule_new(Rule),
    rule_get(Rule,	[	pattern=Pattern,	
						context=Context,
    					subformula=Subformula2,
    					substitution=RuleSubst,
    					subcontext=Subcontext,
    					bound=Bound
    	    		]
    ),
    functor(Pattern,Name,Arity),    
	(	Binding==unresolved
	->  throw(unresolved(Goal,Cx0))		
	;	Binding==builtin
	->  cx_context(Cx0,Context),
		call(Rule)
	;	cx_context(Cx0,Context),
    	cx_program(Cx0,Program),
    	pef_msf_rule_query([	predicate=Binding,
    							program=Program,    
    							pattern=Pattern,	
    							context=Context,
    							subformula=Subformula2,
    							substitution=RuleSubst,
    							subcontext=Subcontext,
    							bound=Bound					
    						])	
    ).	    	




    



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
    cx_context(Cx,Subcontext0),    
    (	Subcontext0==[]
    ->	Subcontext=InitialContext
    ;	ast_apply(Subcontext0,Subst,Subcontext)
    ),
    cx_bound(Cx,Bound),
    
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


assert_rule2(Rule):-
    (	rule_exists(Rule,Id,more_special)
    ->	pef_msf_rule_retractall([id=Id])
    ;	rule_exists(Rule,Id,more_general)
    ->	true
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



resolve(Goal,Cx0,Cx1,Pred):-    
	cx_program(Cx0,Program),
	cx_context(Cx0,Context0),
	ast_apply(Context0,_,Context),
	(	var(Context)
	->	throw(variable_context_in_resolve)
	;	true
	),
	cx_bound(Cx0,Bound0),
	ast_functor(Goal,Name,Arity),
	(	granularity(predicates)
	->	pdt_request_target(literals(predicate(Name/Arity)))
	;	true
	),	
	(	resolve(Program,Context,Name,Arity,Bound0,Pred,Bound1)
	*-> cx_set(Cx0,[bound=Bound1],Cx1)
	;	library_pred(Name,Arity)
	->	Cx0=Cx1,
		Pred=builtin
	;	functor(Head,Name,Arity),
		predicate_property(Head,built_in)
	->	Cx0=Cx1,Pred=builtin		
	;	Cx0=Cx1,Pred=unresolved
	).    


resolve(Program,Context,Name,Arity,Bound0,Pred,Bound1):-
	(	Context== []
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
rule( (M : A),	_, AA, MM,[A=AA,M=MM],lower,builtin).
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
    ->	Context=[]
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

fp_init_todo2(Pred,Cx,GoalNode):-
    pef_clause_query([predicate=Pred,toplevel=Tl]),
    % need to add a dependency to the interpretation of the file including the clause.
    % This is a bit awkward since the very fact that we can to refer to a predicate implies
    % that we already have interpeted that file. 
    % We could also refer to the predicate by its name and the file the clause apears in. 
    % But than we would have to do more lookups. 
    % FIXME: think of a better solution.
    pef_toplevel_query([id=Tl,file=File]),
    pef_directory_entry_query([child=File]),%ignore toplevels that belong to files outside the source path
    \+ pef_term_expansion_query([original=Tl]), %for expansion: ignore original.
    get_pef_file(Path,File),
    pdt_request_targets([interprete(Path),ast(file(Path))]),
	%    
    predicate_owner(Pred,Prog),
	(	pef_predicate_property_definition_query([predicate=Pred,property=module_transparent])
    ->	Context=[]
    ;	pef_predicate_query([id=Pred,module=MID]),
    	module_name(MID,Context)
    ),
    ast_root(Tl,Root),
    ast_head_body(Root,_,Head,Goal),
    Goal \== [], Head \== [],	
    ast_node(Head,HeadNode),
    ast_node(Goal,GoalNode),    
    cx_new(Cx),
    cx_toplevel(Cx,Tl),
	cx_program(Cx,Prog),
	cx_predicate(Cx,Pred),	
	cx_context(Cx,Context),
    cx_head(Cx,HeadNode),
    cx_bound(Cx,lower).


fp_init_todo3(File,Cx,GoalNode):-    
    pef_directory_entry_query([child=File]),%ignore files outside the source path
    pef_toplevel_query([id=Tl,file=File]),       
    \+ pef_term_expansion_query([original=Tl]), %for expansion: ignore original.
   	pef_clause_query([toplevel=Tl,predicate=Pred]),
    predicate_owner(Pred,Prog),
	(	pef_predicate_property_definition_query([predicate=Pred,property=module_transparent])
    ->	Context=[]
    ;	pef_predicate_query([id=Pred,module=MID]),
    	module_name(MID,Context)
    ),
    ast_root(Tl,Root),
    ast_head_body(Root,_,Head,Goal),
    Goal \== [], Head \== [],	
    ast_node(Head,HeadNode),
    ast_node(Goal,GoalNode),    
    cx_new(Cx),
    cx_toplevel(Cx,Tl),
	cx_program(Cx,Prog),
	cx_predicate(Cx,Pred),	
	cx_context(Cx,Context),
    cx_head(Cx,HeadNode),
    cx_bound(Cx,lower).







fp_process_result(cannot_infer(Cx,Goal)):-  	
    cx_head(Cx,Head),
    ast_node(Head,HeadNode),
    (	ast_node(Goal,GoalNode)
    ->	true
    ;	GoalNode=[]
    ),    
    cx_set_head(Cx,HeadNode,Cx1),
	(	pef_cannot_infer_rule_query([goal=GoalNode])
	->	true
	;	pef_reserve_id(pef_cannot_infer_rule,Id),
		pef_cannot_infer_rule_assert([id=Id,goal=GoalNode,cx=Cx1])
	).  

fp_process_result(rule(Rule)):-
    (	assert_rule(Rule)
    ->	pef_msf_rule_predicate(Rule,Callee),
    	recordz(callee,Callee),
    	forall(
    		pef_call_query([predicate=Callee,goal=Goal, cx=Cx]),
    		(	cx_predicate(Cx,Caller),
    			pdt_fp_enqueue(find_literals(Goal,Cx),literals(predicate(Caller)))
    		)    	
    	)
    ;	true
    ).
    
fp_process_result(literal(Cx,Goal,Predicate)):-
    pef_reserve_id(pef_call,Id),    
    cx_head(Cx,Head),
    ast_node(Head,HeadNode),
    (	ast_node(Goal,GoalNode)
    ->	true
    ;	GoalNode=[]
    ),
    cx_set_head(Cx,HeadNode,Cx1),
    (	pef_call_query([goal=GoalNode,predicate=Predicate2]),
    	Predicate2==Predicate
    ->  true
    ;	pef_call_assert([id=Id,goal=GoalNode,cx=Cx1,predicate=Predicate])
    ).
    
   
fp_process_result(meta_call(Cx,Goal,Predicate)):-
	pef_reserve_id(pef_call,Id),
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
    cx_head(Cx,Head),
    ast_node(Head,HeadNode),
    (	ast_node(Goal,GoalNode)
    ->	true
    ;	GoalNode=[]
    ),
    cx_set_head(Cx,HeadNode,Cx1),
	(	pef_unresolved_predicate_symbol_query([goal=GoalNode])
	->	true
	;	pef_reserve_id(pef_unresolved_predicate_symbol,Id),
		pef_unresolved_predicate_symbol_assert([id=Id,goal=GoalNode,cx=Cx1])
	).
	



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




%-----------
%hardcoded list of library preds generated from INDEX.pl in the systems library dir

library_pred(am_match, 1).
library_pred(am_compile, 1).
library_pred(am_bagof, 4).
library_pred(assoc_to_list, 2).
library_pred(empty_assoc, 1).
library_pred(gen_assoc, 3).
library_pred(get_assoc, 3).
library_pred(get_assoc, 5).
library_pred(list_to_assoc, 2).
library_pred(map_assoc, 2).
library_pred(map_assoc, 3).
library_pred(max_assoc, 3).
library_pred(min_assoc, 3).
library_pred(ord_list_to_assoc, 2).
library_pred(put_assoc, 4).
library_pred('$arch', 2).
library_pred('$version', 1).
library_pred('$home', 1).
library_pred('$argv', 1).
library_pred('$strip_module', 3).
library_pred(displayq, 1).
library_pred(displayq, 2).
library_pred(sformat, 2).
library_pred(sformat, 3).
library_pred(concat, 3).
library_pred(read_variables, 2).
library_pred(read_variables, 3).
library_pred(feature, 2).
library_pred(set_feature, 2).
library_pred(substring, 4).
library_pred(flush, 0).
library_pred(write_ln, 1).
library_pred(proper_list, 1).
library_pred(free_variables, 2).
library_pred(checklist, 2).
library_pred(convert_time, 2).
library_pred(convert_time, 8).
library_pred('C', 3).
library_pred(current_thread, 2).
library_pred(current_mutex, 3).
library_pred(message_queue_size, 2).
library_pred(base64, 2).
library_pred(base64, 3).
library_pred(please, 2).
library_pred(cputime, 1).
library_pred(include, 1).
library_pred(setdebug, 0).
library_pred(reconsult, 1).
library_pred(rerecord, 2).
library_pred(erase_all, 1).
library_pred(record, 3).
library_pred(inttoatom, 2).
library_pred(atomconcat, 3).
library_pred(update, 1).
library_pred(printf, 2).
library_pred(index, 2).
library_pred(predicate_type, 2).
library_pred(vread, 2).
library_pred(bindVariables, 1).
library_pred(writeClause, 2).
library_pred(listen, 3).
library_pred(listen, 2).
library_pred(unlisten, 1).
library_pred(unlisten, 2).
library_pred(unlisten, 3).
library_pred(listening, 3).
library_pred(broadcast, 1).
library_pred(broadcast_request, 1).
library_pred(cgi_get_form, 1).
library_pred(check_old_last, 0).
library_pred(check, 0).
library_pred(list_undefined, 0).
library_pred(list_autoload, 0).
library_pred(list_redefined, 0).
library_pred(check_old_select, 0).
library_pred(chr_show_store, 1).
library_pred(find_chr_constraint, 1).
library_pred(chr_trace, 0).
library_pred(chr_notrace, 0).
library_pred(chr_leash, 1).
library_pred(crypt, 2).
library_pred(is_alnum, 1).
library_pred(is_alpha, 1).
library_pred(is_ascii, 1).
library_pred(is_cntrl, 1).
library_pred(is_csym, 1).
library_pred(is_csymf, 1).
library_pred(is_digit, 1).
library_pred(is_digit, 3).
library_pred(is_endfile, 1).
library_pred(is_endline, 1).
library_pred(is_graph, 1).
library_pred(is_lower, 1).
library_pred(is_newline, 1).
library_pred(is_newpage, 1).
library_pred(is_paren, 2).
library_pred(is_period, 1).
library_pred(is_print, 1).
library_pred(is_punct, 1).
library_pred(is_quote, 1).
library_pred(is_space, 1).
library_pred(is_upper, 1).
library_pred(is_white, 1).
library_pred(to_lower, 2).
library_pred(to_upper, 2).
library_pred(date_time_value, 3).
library_pred(parse_time, 2).
library_pred(debug, 3).
library_pred(debug, 1).
library_pred(nodebug, 1).
library_pred(debugging, 1).
library_pred(debugging, 2).
library_pred(list_debug_topics, 0).
library_pred(assertion, 1).
library_pred(expects_dialect, 1).
library_pred(exists_source, 1).
library_pred(source_exports, 2).
library_pred(dif, 2).
library_pred(doc_server, 1).
library_pred(doc_server, 2).
library_pred(doc_browser, 0).
library_pred(doc_browser, 1).
library_pred(doc_server_root, 1).
library_pred(double_metaphone, 2).
library_pred(double_metaphone, 3).
library_pred(display, 1).
library_pred(display, 2).
library_pred(unknown, 2).
library_pred(debug, 0).
library_pred(nodebug, 0).
library_pred(edit, 1).
library_pred(edit, 0).
library_pred('$editor_load_code', 2).
library_pred(find_predicate1, 2).
library_pred(emacs_consult, 1).
library_pred(emacs_dabbrev_atom, 1).
library_pred(emacs_complete_atom, 1).
library_pred(emacs_previous_command, 0).
library_pred(emacs_next_command, 0).
library_pred(call_emacs, 1).
library_pred(call_emacs, 2).
library_pred(running_under_emacs_interface, 0).
library_pred(type_error, 2).
library_pred(domain_error, 2).
library_pred(existence_error, 2).
library_pred(permission_error, 3).
library_pred(instantiation_error, 1).
library_pred(must_be, 2).
library_pred(explain, 1).
library_pred(explain, 2).
library_pred(set_time_file, 3).
library_pred(can_open_file, 2).
library_pred(chdir, 1).
library_pred(reset_gensym, 0).
library_pred(reset_gensym, 1).
library_pred(gensym, 2).
library_pred(add_to_heap, 4).
library_pred(empty_heap, 1).
library_pred(get_from_heap, 4).
library_pred(heap_size, 2).
library_pred(heap_to_list, 2).
library_pred(list_to_heap, 2).
library_pred(min_of_heap, 3).
library_pred(min_of_heap, 5).
library_pred(predicate, 5).
library_pred(section, 4).
library_pred(function, 3).
library_pred(help, 1).
library_pred(help, 0).
library_pred(apropos, 1).
library_pred(iso_639_2, 2).
library_pred(iso_639_3, 2).
library_pred(iso_639, 2).
library_pred(jpl_get_default_jvm_opts, 1).
library_pred(jpl_set_default_jvm_opts, 1).
library_pred(jpl_get_actual_jvm_opts, 1).
library_pred(jpl_pl_lib_version, 1).
library_pred(jpl_c_lib_version, 1).
library_pred(jpl_new, 3).
library_pred(jpl_call, 4).
library_pred(jpl_get, 3).
library_pred(jpl_set, 3).
library_pred(jpl_servlet_byref, 3).
library_pred(jpl_servlet_byval, 3).
library_pred(jpl_class_to_classname, 2).
library_pred(jpl_class_to_type, 2).
library_pred(jpl_classname_to_class, 2).
library_pred(jpl_classname_to_type, 2).
library_pred(jpl_datum_to_type, 2).
library_pred(jpl_false, 1).
library_pred(jpl_is_class, 1).
library_pred(jpl_is_false, 1).
library_pred(jpl_is_null, 1).
library_pred(jpl_is_object, 1).
library_pred(jpl_is_object_type, 1).
library_pred(jpl_is_ref, 1).
library_pred(jpl_is_true, 1).
library_pred(jpl_is_type, 1).
library_pred(jpl_is_void, 1).
library_pred(jpl_null, 1).
library_pred(jpl_object_to_class, 2).
library_pred(jpl_object_to_type, 2).
library_pred(jpl_primitive_type, 1).
library_pred(jpl_ref_to_type, 2).
library_pred(jpl_true, 1).
library_pred(jpl_type_to_class, 2).
library_pred(jpl_type_to_classname, 2).
library_pred(jpl_void, 1).
library_pred(jpl_array_to_length, 2).
library_pred(jpl_array_to_list, 2).
library_pred(jpl_datums_to_array, 2).
library_pred(jpl_enumeration_element, 2).
library_pred(jpl_enumeration_to_list, 2).
library_pred(jpl_hashtable_pair, 2).
library_pred(jpl_iterator_element, 2).
library_pred(jpl_list_to_array, 2).
library_pred(jpl_terms_to_array, 2).
library_pred(jpl_map_element, 2).
library_pred(jpl_set_element, 2).
library_pred(listing, 0).
library_pred(listing, 1).
library_pred(portray_clause, 1).
library_pred(portray_clause, 2).
library_pred(member, 2).
library_pred(append, 3).
library_pred(select, 3).
library_pred(nextto, 3).
library_pred(delete, 3).
library_pred(nth0, 3).
library_pred(nth1, 3).
library_pred(last, 2).
library_pred(reverse, 2).
library_pred(permutation, 2).
library_pred(flatten, 2).
library_pred(sumlist, 2).
library_pred(numlist, 3).
library_pred(is_set, 1).
library_pred(list_to_set, 2).
library_pred(intersection, 3).
library_pred(union, 3).
library_pred(subset, 2).
library_pred(subtract, 3).
library_pred(main, 0).
library_pred(make, 0).
library_pred(new_memory_file, 1).
library_pred(free_memory_file, 1).
library_pred(size_memory_file, 2).
library_pred(open_memory_file, 3).
library_pred(open_memory_file, 4).
library_pred(atom_to_memory_file, 2).
library_pred(memory_file_to_atom, 2).
library_pred(memory_file_to_codes, 2).
library_pred(utf8_position_memory_file, 3).
library_pred(mime_parse, 2).
library_pred(empty_nb_set, 1).
library_pred(add_nb_set, 2).
library_pred(add_nb_set, 3).
library_pred(gen_nb_set, 2).
library_pred(size_nb_set, 2).
library_pred(nb_set_to_list, 2).
library_pred(contains_term, 2).
library_pred(contains_var, 2).
library_pred(free_of_term, 2).
library_pred(free_of_var, 2).
library_pred(occurrences_of_term, 3).
library_pred(occurrences_of_var, 3).
library_pred(sub_term, 2).
library_pred(sub_var, 2).
library_pred(push_operators, 1).
library_pred(pop_operators, 0).
library_pred(push_op, 3).
library_pred(option, 2).
library_pred(option, 3).
library_pred(select_option, 3).
library_pred(select_option, 4).
library_pred(list_to_ord_set, 2).
library_pred(ord_add_element, 3).
library_pred(ord_del_element, 3).
library_pred(ord_intersect, 2).
library_pred(ord_intersect, 3).
library_pred(ord_intersection, 3).
library_pred(ord_disjoint, 2).
library_pred(ord_subtract, 3).
library_pred(ord_union, 3).
library_pred(ord_union, 4).
library_pred(ord_subset, 2).
library_pred(ord_empty, 1).
library_pred(ord_memberchk, 2).
library_pred(oset_is, 1).
library_pred(oset_union, 3).
library_pred(oset_int, 3).
library_pred(oset_diff, 3).
library_pred(oset_dint, 2).
library_pred(oset_dunion, 2).
library_pred(oset_addel, 3).
library_pred(oset_delel, 3).
library_pred(oset_power, 2).
library_pred(pairs_keys_values, 3).
library_pred(pairs_values, 2).
library_pred(group_pairs_by_key, 2).
library_pred(transpose_pairs, 2).
library_pred(doc_collect, 1).
library_pred(pldoc_loading, 0).
library_pred(set_test_options, 1).
library_pred(begin_tests, 1).
library_pred(begin_tests, 2).
library_pred(end_tests, 1).
library_pred(run_tests, 0).
library_pred(run_tests, 1).
library_pred(load_test_files, 1).
library_pred(porter_stem, 2).
library_pred(unaccent_atom, 2).
library_pred(tokenize_atom, 2).
library_pred(atom_to_stem_list, 2).
library_pred(clause_info, 4).
library_pred(predicate_name, 2).
library_pred(clause_name, 2).
library_pred(prolog_server, 2).
library_pred(prolog_read_source_term, 4).
library_pred(prolog_open_source, 2).
library_pred(prolog_close_source, 1).
library_pred(prolog_canonical_source, 2).
library_pred(get_prolog_backtrace, 2).
library_pred(get_prolog_backtrace, 3).
library_pred(print_prolog_backtrace, 2).
library_pred(backtrace, 1).
library_pred(xref_source, 1).
library_pred(xref_called, 3).
library_pred(xref_defined, 3).
library_pred(xref_definition_line, 2).
library_pred(xref_exported, 2).
library_pred(xref_module, 2).
library_pred(xref_op, 2).
library_pred(xref_clean, 1).
library_pred(xref_current_source, 1).
library_pred(xref_done, 2).
library_pred(xref_built_in, 1).
library_pred(xref_expand, 2).
library_pred(xref_source_file, 3).
library_pred(xref_source_file, 4).
library_pred(xref_public_list, 4).
library_pred(xref_meta, 2).
library_pred(xref_hook, 1).
library_pred(xref_used_class, 2).
library_pred(xref_defined_class, 3).
library_pred(load_foreign_files, 0).
library_pred(load_foreign_files, 2).
library_pred(load_foreign_files, 3).
library_pred(make_shared_object, 3).
library_pred(make_foreign_wrapper_file, 1).
library_pred(make_foreign_wrapper_file, 2).
library_pred(qsave_program, 1).
library_pred(qsave_program, 2).
library_pred(unix, 1).
library_pred(abs, 2).
library_pred(sin, 2).
library_pred(cos, 2).
library_pred(tan, 2).
library_pred(log, 2).
library_pred(log10, 2).
library_pred(pow, 3).
library_pred(ceiling, 2).
library_pred(floor, 2).
library_pred(round, 2).
library_pred(acos, 2).
library_pred(asin, 2).
library_pred(atan, 2).
library_pred(atan2, 3).
library_pred(sign, 2).
library_pred(sqrt, 2).
library_pred(genarg, 3).
library_pred(mode, 1).
library_pred(public, 1).
library_pred((meta_predicate), 1).
library_pred(no_style_check, 1).
library_pred(otherwise, 0).
library_pred(subsumes_chk, 2).
library_pred(simple, 1).
library_pred(prolog_flag, 2).
library_pred(date, 1).
library_pred(current_stream, 3).
library_pred(stream_position, 3).
library_pred(skip_line, 0).
library_pred(skip_line, 1).
library_pred(compile, 1).
library_pred(atom_char, 2).
library_pred(midstring, 3).
library_pred(midstring, 4).
library_pred(midstring, 5).
library_pred(midstring, 6).
library_pred(raise_exception, 1).
library_pred(on_exception, 3).
library_pred(random, 1).
library_pred(random, 3).
library_pred(randseq, 3).
library_pred(randset, 3).
library_pred(getrand, 1).
library_pred(setrand, 1).
library_pred(rb_new, 1).
library_pred(rb_empty, 1).
library_pred(rb_lookup, 3).
library_pred(rb_update, 4).
library_pred(rb_update, 5).
library_pred(rb_apply, 4).
library_pred(rb_lookupall, 3).
library_pred(rb_insert, 4).
library_pred(rb_delete, 3).
library_pred(rb_delete, 4).
library_pred(rb_visit, 2).
library_pred(rb_keys, 2).
library_pred(rb_map, 2).
library_pred(rb_map, 3).
library_pred(rb_partial_map, 4).
library_pred(rb_clone, 3).
library_pred(rb_clone, 4).
library_pred(rb_min, 3).
library_pred(rb_max, 3).
library_pred(rb_del_min, 4).
library_pred(rb_del_max, 4).
library_pred(rb_next, 4).
library_pred(rb_previous, 4).
library_pred(list_to_rbtree, 2).
library_pred(ord_list_to_rbtree, 2).
library_pred(is_rbtree, 1).
library_pred(rb_size, 2).
library_pred(rb_in, 3).
library_pred(load_rdf_ntriples, 2).
library_pred(rdf_ntriple_part, 4).
library_pred(xml_to_plrdf, 3).
library_pred(element_to_plrdf, 3).
library_pred(rdf_name_space, 1).
library_pred(load_rdf, 2).
library_pred(load_rdf, 3).
library_pred(xml_to_rdf, 3).
library_pred(process_rdf, 3).
library_pred(rdf_triples, 2).
library_pred(rdf_triples, 3).
library_pred(rdf_reset_ids, 0).
library_pred(rdf_start_file, 2).
library_pred(rdf_end_file, 1).
library_pred(anon_prefix, 1).
library_pred(rdf_write_xml, 2).
library_pred(readln, 1).
library_pred(readln, 2).
library_pred(readln, 5).
library_pred(read_line_to_codes, 2).
library_pred(read_line_to_codes, 3).
library_pred(read_stream_to_codes, 2).
library_pred(read_stream_to_codes, 3).
library_pred(read_file_to_codes, 3).
library_pred(read_file_to_terms, 3).
library_pred(record, 1).
library_pred(rewrite, 2).
library_pred(rew_term_expansion, 2).
library_pred(rew_goal_expansion, 2).
library_pred(rlimit, 3).
library_pred(setting, 4).
library_pred(setting, 2).
library_pred(set_setting, 2).
library_pred(restore_setting, 1).
library_pred(load_settings, 1).
library_pred(load_settings, 2).
library_pred(save_settings, 0).
library_pred(save_settings, 1).
library_pred(current_setting, 1).
library_pred(setting_property, 2).
library_pred(list_settings, 0).
library_pred(convert_setting_text, 3).
library_pred(load_sgml_file, 2).
library_pred(load_xml_file, 2).
library_pred(load_html_file, 2).
library_pred(load_structure, 3).
library_pred(load_dtd, 2).
library_pred(load_dtd, 3).
library_pred(dtd, 2).
library_pred(dtd_property, 2).
library_pred(new_dtd, 2).
library_pred(free_dtd, 1).
library_pred(open_dtd, 3).
library_pred(new_sgml_parser, 2).
library_pred(free_sgml_parser, 1).
library_pred(set_sgml_parser, 2).
library_pred(get_sgml_parser, 2).
library_pred(sgml_parse, 2).
library_pred(sgml_register_catalog_file, 2).
library_pred(xml_quote_attribute, 3).
library_pred(xml_quote_cdata, 3).
library_pred(xml_quote_attribute, 2).
library_pred(xml_quote_cdata, 2).
library_pred(xml_name, 1).
library_pred(xml_is_dom, 1).
library_pred(html_write, 2).
library_pred(html_write, 3).
library_pred(sgml_write, 2).
library_pred(sgml_write, 3).
library_pred(xml_write, 2).
library_pred(xml_write, 3).
library_pred(sha_hash, 3).
library_pred(hmac_sha, 4).
library_pred(ls, 0).
library_pred(ls, 1).
library_pred(cd, 0).
library_pred(cd, 1).
library_pred(pushd, 0).
library_pred(pushd, 1).
library_pred(dirs, 0).
library_pred(pwd, 0).
library_pred(popd, 0).
library_pred(mv, 2).
library_pred(rm, 1).
library_pred(load_foreign_library, 1).
library_pred(load_foreign_library, 2).
library_pred(unload_foreign_library, 1).
library_pred(unload_foreign_library, 2).
library_pred(current_foreign_library, 2).
library_pred(reload_foreign_libraries, 0).
library_pred(tcp_socket, 1).
library_pred(tcp_close_socket, 1).
library_pred(tcp_open_socket, 3).
library_pred(tcp_connect, 2).
library_pred(tcp_bind, 2).
library_pred(tcp_accept, 3).
library_pred(tcp_listen, 2).
library_pred(tcp_fcntl, 3).
library_pred(tcp_setopt, 2).
library_pred(tcp_host_to_address, 2).
library_pred(tcp_select, 3).
library_pred(gethostname, 1).
library_pred(udp_socket, 1).
library_pred(udp_receive, 4).
library_pred(udp_send, 4).
library_pred(predsort, 3).
library_pred(merge, 3).
library_pred(merge_set, 3).
library_pred(locale_sort, 2).
library_pred(ssl_init, 3).
library_pred(ssl_accept, 3).
library_pred(ssl_open, 3).
library_pred(ssl_open, 4).
library_pred(ssl_exit, 1).
library_pred(time, 1).
library_pred(profile, 1).
library_pred(profile, 3).
library_pred(show_profile, 2).
library_pred(show_profile, 1).
library_pred(add_stream_to_pool, 2).
library_pred(delete_stream_from_pool, 1).
library_pred(close_stream_pool, 0).
library_pred(dispatch_stream_pool, 1).
library_pred(stream_pool_main_loop, 0).
library_pred(lock_predicate, 2).
library_pred(unlock_predicate, 2).
library_pred(system_mode, 1).
library_pred(system_module, 0).
library_pred(new_table, 4).
library_pred(open_table, 1).
library_pred(close_table, 1).
library_pred(free_table, 1).
library_pred(table_window, 3).
library_pred(read_table_record, 4).
library_pred(read_table_record_data, 4).
library_pred(read_table_fields, 4).
library_pred(get_table_attribute, 3).
library_pred(table_previous_record, 3).
library_pred(table_start_of_record, 4).
library_pred(in_table, 3).
library_pred(new_order_table, 2).
library_pred(order_table_mapping, 3).
library_pred(compare_strings, 4).
library_pred(prefix_string, 3).
library_pred(prefix_string, 4).
library_pred(sub_string, 3).
library_pred(sort_table, 2).
library_pred(verify_table_order, 1).
library_pred(show_coverage, 1).
library_pred(covered_clauses, 4).
library_pred(make_tests, 3).
library_pred(make_test, 3).
library_pred(concurrent, 3).
library_pred(first_solution, 3).
library_pred(thread_run_interactor, 0).
library_pred(threads, 0).
library_pred(join_threads, 0).
library_pred(interactor, 0).
library_pred(attach_console, 0).
library_pred(tspy, 1).
library_pred(tspy, 2).
library_pred(tdebug, 0).
library_pred(tdebug, 1).
library_pred(tnodebug, 0).
library_pred(tnodebug, 1).
library_pred(tprofile, 1).
library_pred(alarm, 3).
library_pred(alarm, 4).
library_pred(remove_alarm, 1).
library_pred(install_alarm, 1).
library_pred(current_alarm, 4).
library_pred(call_with_time_limit, 2).
library_pred(tty_clear, 0).
library_pred(tty_flash, 0).
library_pred(menu, 3).
library_pred(add_edges, 3).
library_pred(add_vertices, 3).
library_pred(complement, 2).
library_pred(compose, 3).
library_pred(del_edges, 3).
library_pred(del_vertices, 3).
library_pred(edges, 2).
library_pred(neighbors, 3).
library_pred(neighbours, 3).
library_pred(reachable, 3).
library_pred(top_sort, 2).
library_pred(top_sort, 3).
library_pred(transitive_closure, 2).
library_pred(transpose, 2).
library_pred(vertices, 2).
library_pred(vertices_edges_to_ugraph, 3).
library_pred(ugraph_union, 3).
library_pred(fork, 1).
library_pred(exec, 1).
library_pred(wait, 2).
library_pred(kill, 2).
library_pred(pipe, 2).
library_pred(dup, 2).
library_pred(detach_IO, 0).
library_pred(environ, 1).
library_pred(parse_url, 2).
library_pred(parse_url, 3).
library_pred(is_absolute_url, 1).
library_pred(global_url, 3).
library_pred(http_location, 2).
library_pred(www_form_encode, 2).
library_pred(parse_url_search, 2).
library_pred(file_name_to_url, 2).
library_pred(utf8_codes, 3).
library_pred(when, 2).
library_pred(www_open_url, 1).
library_pred(expand_url_path, 2).
library_pred(xsdp_type, 1).
library_pred(xsdp_numeric_uri, 2).
library_pred(xsdp_subtype_of, 2).
library_pred(xsdp_convert, 3).
library_pred(zopen, 3).
library_pred(gzopen, 3).
library_pred(gzopen, 4).

