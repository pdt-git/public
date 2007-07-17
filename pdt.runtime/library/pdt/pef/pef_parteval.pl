% a primitive partial evaluator for specializing pef utility predicates for each pef.


%pe(Goal,Subst,Goal2,Subst2).

pe(functor('$var'(A),'$var'(B),'$var'(C)),Subst,GoalOut,Subst3,false):-
	subst_maps(Subst,'$var'(A),AVal),
	!,
	functor(AVal,BVal,CVal),
	(	subst_and(Subst,'$var'(B),BVal,Subst2),
		subst_and(Subst2,'$var'(C),CVal,Subst3)
	->	GoalOut = true
	;	GoalOut = fail, subst_empty(Subst3)
	).
pe(functor('$var'(A),'$var'(B),'$var'(C)),Subst,GoalOut,Subst2,false):-
	subst_maps(Subst,'$var'(B),BVal),
	subst_maps(Subst,'$var'(C),CVal),		
	!,
	functor(AVal,BVal,CVal),
	(	subst_and(Subst,'$var'(A),AVal,Subst2)
	->	GoalOut = true
	;	GoalOut = fail, subst_empty(Subst2)
	).
	

pe((A,B),Subst,GoalOut,SubstOut,CutOut):-
    !,
	pe(A,Subst,A2,SubstA,CutA),
	%the following assumes that a goal that ALWAYS fails has no relevant side effects.
	% not good for unterminated reapeat,fail loops, but ok in our case.
	(	A2==fail
	->	GoalOut=fail,
		subst_empty(SubstOut),
		CutOut=CutA
	;	A2==true
	->	pe(B,SubstA,GoalOut,SubstOut,CutB),
		cut_and(CutA,CutB,CutOut)
	;	pe(B,SubstA,B2,SubstOut,CutB),			
		cut_and(CutA,CutB,CutOut),
		(	B2==fail
		->	GoalOut=fail
		;	A2==true
		->	GoalOut=B2	
		;	B2==true
		->	GoalOut=A2
		;	GoalOut=(A2,B2)
		)
	).

pe((A -> B ; C),Subst,(A2 -> B2),SubstOut,CutOut):-
    !,
    pe(A,Subst,A2,SubstA,_),
    (	A2==true
    ->	pe(B,SubstA,GoalOut,SubstOut,CutOut)
    ;	A2==fail
    ->	pe(C,Subst,GoalOut,SubstOut,CutOut)
    ;	pe(B,SubstA,B2,SubstB,CutB),
    	pe(C,Subst,C2,SubstC,CutC),
    	GoalOut=(A2 -> B2 ; C2 ),
    	cut_lub(CutB,CutC,CutOut),
    	subst_lub(SubstB,SubstC,SubstOut)
    ).
pe((A;B),Subst,GoalOut,SubstOut,CutOut):-
    !,
	pe(A,Subst,A2,SubstA,Cut1),
	pe(B,Subst,B2,SubstB,Cut2),
	cut_lub(Cut1,Cut2,CutOut),
	subst_lub(SubstA,SubstB,SubstOut),
	(	A2==fail
	->	GoalOut=B2
	;	B2==fail
	->	GoalOut=A2
	;	GoalOut=(A2;B2)
	).
	

parteval(Goal,PEGoal):-
	copy_term(Goal,GGoal),
	clause(evaluated(GGoal),PEGoal).
 numbervars(Term):-
     numbervars(Term,0,_,[]).
 varsnumber(Term,Term2):-
     new_memory_file(MF),
     open_memory_file(MF,write,Out),
     write_canonical(Out,Term),
     write(Out,'.'),
     close(Out),
     open_memory_file(MF,read,In),
     read(In,Term2),
     close(In),
     free_memory_file(MF).
 
 general_special(G,S):-
 	copy_term(S,SS),
 	\+ \+ (G=SS,SS=@=S).
 	  
    