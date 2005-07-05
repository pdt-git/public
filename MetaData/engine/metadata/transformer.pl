
test:-
  test(A),A.
test:-
  test(A),writeln(A).

test(writeln(regenschirm)).

do_clause(test, A) :-
	unify([], test, test, B),
	apply_subst(B, test(C), D),
	do_goal(D, E),
	unify(B, D, E, F),
	apply_subst(F, C, G),
	do_goal(G, H),
	unify(F, G, H, I),
	apply_subst(I, test, A).

do_clause(test, A) :-
	unify([], test, test, B),
	apply_subst(B, test(C), D),
	do_goal(D, E),
	unify(B, D, E, F),
	apply_subst(F, writeln(C), G),
	do_goal(G, H),
	unify(F, G, H, I),
	apply_subst(I, test, A).
%% ^^ only testing stuff above this line ^^	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% do_goal(+InHead,-OutHead)
%
% called by the meta interpreter when a goal is encountered.
% Should succeed if InHead is provable.
% on Success, should unify OutHead with a *COPY* of InHead that represents the instantiation 
% state of InHead after successfully proving it.
% Should *NOT* alter the instantiation state of InHead.
do_goal(A,B):-
    copy_term(A,B),
    format("In a real application i would now do something like do_clause(~w,~w).~n",[A,B]).

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
)):-  
  var(Goal),!.


transform_body((Goal,Tail),Sin,Sout,(TGoal,TTail)):-
  !,
  transform_body(Goal,Sin,S,TGoal),
  transform_body(Tail,S,Sout,TTail).
transform_body(Goal,Sin,Sout,(
  apply_subst(Sin,Goal,InGoal),
  do_goal(InGoal,OutGoal),
  unify(Sin,InGoal,OutGoal,Sout)		     
)):-
  !.


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












