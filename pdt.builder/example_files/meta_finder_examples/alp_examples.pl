% Test data: These should be identified as meta-predicates.
my_meta_1(Goal) :- 
	call(Goal).

my_meta_2(F,A)  :- 
	functor(Goal,F,A), 
	call(Goal).

my_meta_3(List) :- 
	Goal =.. List, 
	call(Goal).

my_meta_4(Term) :- 
	arg(_N,Term,Goal), 
	call(Goal).
	
my_meta_5(Term) :-
	my_meta_4(Term).

% Test data: These should not be considered to be meta-predicates.
just_a_call(X) :- 
	call( p(X) ).
	
just_a_call(_X) :- 
	call(_G).  

	
	
% member(Goal, [my_meta_1(X), my_meta_2(X,Y), my_meta_3(X), my_meta_4(X), just_a_call(X)]), infer_meta_arguments_for(user,Goal,MetaSpec).