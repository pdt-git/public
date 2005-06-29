test:-
  test(A),A.
test:-
  test(A),writeln(A).

test(writeln(regenschirm)).

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


transform_body(Goal,Sin,Sout,(
  apply_subst(Sin,Goal,InGoal),
  do_goal(InGoal,OutGoal),
  unify(Sin,InGoal,OutGoal,Sout)		     
)):-  
  var(Goal),!.
transform_body((Goal,Tail),Sin,Sout,(
  apply_subst(Sin,Goal,InGoal),
  do_goal(InGoal,OutGoal),
  unify(Sin,InGoal,OutGoal,S),
  TTail
)):-
  !,
  transform_body(Tail,S,Sout,TTail).
transform_body(Goal,Sin,Sout,(
  apply_subst(Sin,Goal,InGoal),
  do_goal(InGoal,OutGoal),
  unify(Sin,InGoal,OutGoal,Sout)		     
)):-
  !.


unify(ISubst,T1,T2,OSubst):-
  unifyable(T1,T2,Vars),
  sort(Vars,SortedVars),
  merge_set(ISubst,SortedVars,OSubst),
  format("bindings: ~w~n",[OSubst]).
  
  
  
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
  














