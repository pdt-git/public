:- use_module(library('spike/pef_base')).

peft_term(Id,_):-
	pef_variable_occurance_query([id=Id]),
	!.
peft_term(Id,Term):-
    nonvar(Term),
    !,
   	functor(Term,Name,Arity),
	pef_term_query([id=Id,name=Name,arity=Arity]),
	peft_term_args(1,Arity,Id,Term).  
peft_term(Id,Term):-
	pef_term_query([id=Id,name=Name,arity=Arity]),
	functor(Term,Name,Arity),
	peft_term_args(1,Arity,Id,Term).  

peft_term_args(I,N,_Id,_Term):-
    I>N,!.
peft_term_args(I,N,Id,Term):-    
    peft_arg(I,Id,ArgId),
    peft_term(ArgId,Arg),
    arg(I,Term,Arg),
    J is I + 1,
    peft_term_args(J,N,Id,Term).
    
peft_arg(I,Id,ArgId):-    
    pef_arg_query([num=I,parent=Id,child=ArgId]).
    
peft_unifiable(A,B,[A=B]):-
	peft_var(A),
	!.
peft_unifiable(A,B,[B=A]):-
	peft_var(B),
	!.	
peft_unifiable(A,B,Unifier):-
	peft_functor(A,Name,Arity),
	peft_functor(B,Name,Arity),
	peft_unifiable_args(1,Arity,A,B,[],Unifier).		

peft_unifiable_args(I,N,_A,_B,Unifier,Unifier):-
	I>N,
	!.
peft_unifiable_args(I,N,A,B,In,Out):-	