:- use_module(library('/org/cs3/pdt/util/pdt_source_term')).
ifif(N):-
    (	N=a(A)
    ->	writeln(prefix_a_arg(A))
    ;	N=b(B)
    ->  writeln(prefix_b_arg(B))
    ;	writeln(none(N))
    ).
ifif(_):-
	writeln(choice).    

bla:-
	umf,
	\+	(	nana
		;	bla
		),
	blabla.




do_goal(Goal,CxIn, CxOut):-
	meta_call(CxIn,Goal),
	prefix_op(CxIn,Goal),
	!,
	(	needs_parenthesis(CxIn,Goal)
	->	output('(',CxIn,Cx1),
		inc_indent(Cx1,Cx2),
		align(0,Cx2,Cx3),		
		source_term_functor(Goal,Name,_),
		output(Name,Cx3,Cx4),
		inc_indent(Cx4,Cx5),
		align(0,Cx5,Cx6),
		source_term_arg(1,Goal,Arg1),
		output_goal(Arg1,Cx6,Cx7),		
		dec_indent(Cx7,Cx8),
		dec_indent(Cx8,Cx9),
		new_line(Cx9,Cx10),
		align(0,Cx10,Cx11),
		output(')',Cx11,CxOut)
	;	source_term_functor(Goal,Name,_),
		output(Name,CxIn,Cx1),
		inc_indent(Cx1,Cx2),
		source_term_arg(1,Goal,Arg1),
		output_goal(Arg1,Cx2,Cx3),
		dec_indent(Cx3,Cx4)
	).
		
	
do_goal(Goal,CxIn, CxOut):-
	meta_call(CxIn,Goal),
	infix_op(CxIn,Goal),
	!,
	goal_left_paren(Goal,CxIn,Cx3),
	source_term_arg(1,Goal,Arg1),
	output_goal(Arg1,Cx3,Cx4),
	new_line(Cx4,Cx5),
	align(-1,Cx5,Cx6),
	source_term_functor(Goal,Name,_),
	output(Name,Cx6,Cx7),
	align(0,Cx7,Cx8),
	source_term_arg(2,Goal,Arg2),
	output_goal(Arg2,Cx8,Cx9),		
	goal_right_paren(Goal,Cx9,CxOut).
		
goal_left_paren(Goal,CxIn,CxOut):-
	output('(',CxIn,Cx1),
	inc_indent(Cx1,Cx2),
	align(0,Cx2,CxOut).
		
goal_right_paren(Goal,Cx9,CxOut):-    	
	dec_indent(Cx9,Cx10),
	new_line(Cx10,Cx11),
	align(0,Cx11,Cx12),
	output(')',Cx12,CxOut).
