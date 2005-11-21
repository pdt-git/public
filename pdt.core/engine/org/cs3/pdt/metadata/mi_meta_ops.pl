:-module(mi_meta_ops,[mi_unify/2,mi_apply_subst/2]).

% unify(+SubstBefore,?T1,?T2,-SubstAfter)
%
% called by the meta interpreter when a unification should accur.
% SubstBefore is a list of variable=substitution terms
% should succeed when T1 and T2 are unifyable when applying SubstBefore 
% in praxis, variables in T1 or T2 will already be substituted according to 
% SubstBefore.
% should find out which additional substitutions WOULD BE required to unify
% T1 and T2.
% should unify SubstAfter with the union of SubstBefore and those additional 
% bindings.
% Should *NOT* change instantiation state of ISubst, T1 or T2.
% In particular, shoul *NOT* unify T1 and T2.
% 
mi_unify(T1,T2):-
  mi_apply_subst(T1,ST1),
  mi_apply_subst(T2,ST2),
  unifyable(ST1,ST2,Subst),
  attribute_subst_vars(Subst).
  
attribute_subst_vars([]).
attribute_subst_vars([Var=Val|T]):-
    put_attr(Var,meta_ops,Val),
    %format("~w->~w;~n",[Val,Var]),
    writeln(unify(Var=Val)),
    attribute_subst_vars(T).

% when this one get's called, we have an error in the 
% meta interpreter: substituted variables should never
% actualy be unified.
attr_unify_hook(AttValue,VarValue):-
    writeln('bug in meta int'),
    throw(exception(should_not_happen(AttValue,VarValue))).

  
% apply_subst(+Subst, +InTerm,-OutTerm)
%
% called by the meta interpreter when a variable substitution should be applied
% to a given term.
% should always succeed.
% should unify OutTerm with a *COPY* of InTerm that reflects the result of the substitution.
% should *NOT* alter the instantiation state of Subst or InTerm
%
% the current implementation uses attributed variables and does not
% need the subst.
mi_apply_subst(T1,T2):-
  subst(T1,T2).
  
subst(GroundTerm,GroundTerm):-
    ground(GroundTerm),
    !.
subst(Var,Subst):-
	var(Var),
	!,
	(	attvar(Var)
	->	get_attr(Var,meta_ops,Value),		
		subst(Value,Subst)
		%writeln(subst(Var=Subst))
	;	Var=Subst
	).	
subst(InTerm,OutTerm):-
	InTerm=..[Functor|Args],
	!,
	subst_args(Args,OutArgs),
	OutTerm=..[Functor|OutArgs].

subst_args([],[]).
subst_args([InTerm|InTail],[OutTerm|OutTail]):-
    subst(InTerm,OutTerm),
    subst_args(InTail,OutTail).