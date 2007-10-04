:- module(literals,[]).

%Wir berechnen folgende Relation:
%lit(Fml,CxName,Prog,SubFml,SubCxName).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).

:- pdt_define_context(cx(program,toplevel,module)).
    
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('builder/builder')).
:- use_module(library('builder/targets/parse')).
find_literal(Goal,Cx,Literal):-        
	pef_term_query([id=Goal,name=Name,arity=Arity]),
	functor(Pattern,Name,Arity),
	cx_get(Cx,[program=Program,module=Module]),
	pef_msf([
			formula=Pattern,
			context=Module,
			program=Program,
			sub_formula=SubGoal,
			sub_context=SubModule
	]),
	ast_match(Pattern,Goal,Subst),
	has_tail([],Subst), %close the list
	(	Subst=[]
	->  % ast is less general than pattern
	    % use whatever was matched in SubGoal to continue. 
	    SubGoal='$var'(Subgoals),
	    has_tail([],Subgoals), % close the list.
		cx_set_module(Cx,SubModule,Cx2),
		member(SubGoal2,SubGoals),
   		find_literal(SubGoal,Cx2,Literal)
   		    
	;	% ast is more general than pattern
	    % there is no sensible way continue _right now_,
	    % but we can 
	)
   	
    ).
    	
    