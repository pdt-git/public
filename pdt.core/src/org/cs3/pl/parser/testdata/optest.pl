:-module(meta_ops,[sim/1]).

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
unify(ISubst,T1,T2,OSubst):-
  unifyable(T1,T2,Vars),
  sort(Vars,SortedVars),
  merge_set(ISubst,SortedVars,OSubst),
  format("bindings: ~w~n",[OSubst]).
  
  
% apply_subst(+Subst, +InTerm,-OutTerm)
%
% called by the meta interpreter when a variable substitution should be applied
% to a given term.
% should always succeed.
% should unify OutTerm with a *COPY* of InTerm that reflects the result of the substitution.
% should *NOT* alter the instantiation state of Subst or InTerm
apply_subst(Subst,T1,T2):-
  copy_term(T1,T2),
  unifyable(T1,T2,L),
  subst(L,Subst),
  format("substituted: ~w with ~w~n",[T1,T2]).
subst([],[]):-!.    
subst([],_).
subst([A=W|T],[A=W|ST]):-
  !,
  subst(T,ST).
subst([A=W|T],Subst):-
  nonvar(Subst),!,
  (   memberchk(X=V,Subst),X==W
->    A=V
;     A=W
  ),
  subst(T,Subst).

