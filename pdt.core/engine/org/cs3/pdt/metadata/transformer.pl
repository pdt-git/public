



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% vv transformation rules below this line vv
transform_clause(
(Head:-Body),
(do_clause(InHead,OutHead):-
  unify([],InHead,Head,S0),
  TBody,
  apply_subst(SOut,Head,OutHead))
):-   
  !,
  functor(Head,Functor,Arity),
  functor(InHead,Functor,Arity),
  transform_body(Body,S0,SOut,TBody).


transform_clause((:-Body),(:-TBody)):-   
  !,
  transform_body(Body,[],_,TBody).

transform_clause(
(Head),
(do_clause(InHead,OutHead):-
  unify([],InHead,Head,S0),
  apply_subst(S0,Head,OutHead))
):-   
  !,
  functor(Head,Functor,Arity),
  functor(InHead,Functor,Arity).



transform_body(Goal,Sin,Sout,(
  apply_subst(Sin,Goal,InGoal),
  do_goal(InGoal,OutGoal),
  unify(Sin,InGoal,OutGoal,Sout)		     
)),


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% vv predicates for driving the transformation process vv

% transform_file(+File,+Reporter)
%  Reporter should be the name of an unary predicate that 
% will be used to report the transfomred directives and clauses.
% For each transformed clause or directive, the transformer will call
% call(Reporter,TransformedClause).

transform_file(File,Reporter):-
  open(File,read,Input),
  call_cleanup(transform_stream(Input,Reporter),
  	close(Input)
  ).

transform_stream(Input,Reporter):-
	repeat,
    read(Input,Term),
    (	Term == end_of_file
    ->	true
    ;	transform_clause(Term,Transformed),
    	call(Reporter,Transformed),
    	fail
    ).












