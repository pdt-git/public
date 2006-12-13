:- use_module(library('/org/cs3/pdt/util/pdt_source_term')).

:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).


:- pdt_define_context(layout(mode,op,offset,row,col,indent_base,indent_current,module,stream,last_pred,comments)).

init_layout_cx(Cx1):-
    pdt_map_empty(Cmts),
    layout_new(Cx0),
    layout_set(Cx0, [
    	mode=[],
    	op=[],
    	offset=0,
    	row=0,
    	col=0,
    	indent_base=0,
    	indent_current=0,
    	module=user,
    	stream=current_output,
    	last_pred=[],
    	comments=Cmts
    ], Cx1 ).


do_members(Terms):-
    init_layout_cx(Cx0),
	do_members(Terms,Cx0,_).    

do_members([],Cx,Cx).
do_members([Member|Members],CxIn,CxOut):-
    do_member(Member,CxIn,CxNext),
    do_members(Members,CxNext,CxOut).
    
do_member(Term,CxIn,CxOut):-
    (	source_term_property(Term,clause_of,Pred)
    ->	do_clause(Pred,Term,CxIn,Cx1)
    ;	do_directive(Term,CxIn,Cx1)
    ),
    output('.',Cx1,CxOut).
    
do_clause(Pred,Term,CxIn,CxOut):-
    layout_last_pred(CxIn,Pred),
    !,
    new_line(CxIn,Cx1),
    do_clauseX(Term,Cx1,CxOut).
do_clause(Pred,Term,CxIn,CxOut):-
    layout_set_last_pred(CxIn,Pred,Cx1),
    new_line(Cx1,Cx2),
    new_line(Cx2,Cx3),
    do_clauseX(Term,Cx3,CxOut).

do_clauseX(Term,CxIn,CxOut):-
    \+ source_term_var(Term),
    source_term_functor(Term,':-',2),
    !,
    do_rule(Term,CxIn,CxOut).
do_clauseX(Term,CxIn,CxOut):-
    do_fact(Term,CxIn,CxOut).


do_fact(Term,CxIn,CxOut):-
    do_head(Term,CxIn,CxOut).

do_directive(Term,CxIn,CxOut):-
    layout_set_last_pred(CxIn,[],Cx0),
    new_line(Cx0,Cx05),
    output_op(':-',Cx05,Cx1),
    output(' ',Cx1,Cx2),
    inc_indent(Cx2,Cx3),
	push_mode(body,Cx3,Cx4),
	source_term_arg(1,Term,Body),
    do_body(Body,Cx4,Cx5),
    dec_indent(Cx5,CxOut).

do_rule(Term,CxIn,CxOut):-
    source_term_arg(1,Term,Head),
    source_term_arg(2,Term,Body),
    push_mode(head,CxIn,Cx1),
    do_head(Head,Cx1,Cx2),
    pop_mode(Cx2,Cx3),
	output_op(':-',Cx3,Cx4),
	inc_indent(Cx4,Cx5),
	new_line(Cx5,Cx6),
	align(0,Cx6,Cx7),
	push_mode(body,Cx7,Cx8),
	do_body(Body,Cx8,Cx9),
	pop_mode(Cx9,Cx10),
	dec_indent(Cx10,CxOut).

do_head(Head,CxIn,CxOut):-
    push_mode(head,CxIn,Cx1),
    output_term(Head,Cx1,Cx2),
    pop_mode(Cx2,CxOut).

do_body(Term,CxIn,CxOut) :-
    push_operator(op(1000,xfy,','),CxIn,Cx1),
    do_goal(Term,Cx1,Cx2),
    push_operator(op(1000,xfy,','),Cx2,CxOut).
    
do_goal(Goal,CxIn, CxOut):-
	meta_call(CxIn,Goal),
	!,
	goal_left_paren(Goal,CxIn,Cx1),
	do_meta_call(Goal,Cx1,Cx2),
	goal_right_paren(Goal,Cx2,CxOut).
do_goal(Goal,CxIn, CxOut):-
    goal_left_paren(Goal,CxIn,Cx1),
	do_data(Goal,Cx1,Cx2),
	goal_right_paren(Goal,Cx2,CxOut).

do_meta_call(Goal,CxIn,CxOut):-	
	conjunction(Goal,CxIn,Goals),
	!,
	push_mode(conjunction,CxIn,Cx1),
	push_functor(Goal,Cx1,Cx2),
	do_conjunction(Goals,Cx2,Cx3),
	pop_functor(Cx3,Cx4),
	pop_mode(Cx4,CxOut).
do_meta_call(Goal,CxIn,CxOut):-	
	prefix_op(Goal,CxIn),
	!,
	source_term_functor(Goal,Name,_),
	output_op(Name,CxIn,Cx1),
	inc_indent(Cx1,Cx2),
	source_term_arg(1,Goal,Arg1),
	output_goal(Arg1,Cx2,Cx3),
	dec_indent(Cx3,CxOut).
do_meta_call(Goal,CxIn,CxOut):-	
	postfix_op(Goal,CxIn),
	!,
	source_term_arg(1,Goal,Arg1),
	output_goal(Arg1,CxIn,Cx1),
	source_term_functor(Goal,Name,_),
	output_op(Name,Cx1,CxOut).
do_meta_call(Goal,CxIn, CxOut):-
	infix_op(Goal,CxIn),
	!,
	source_term_arg(1,Goal,Arg1),
	output_goal(Arg1,CxIn,Cx4),
	new_line(Cx4,Cx5),
	align(-1,Cx5,Cx6),
	source_term_functor(Goal,Name,_),
	output_op(Name,Cx6,Cx7),
	align(0,Cx7,Cx8),
	source_term_arg(2,Goal,Arg2),	
	output_goal(Arg2,Cx8,CxOut).
do_meta_call(Goal,CxIn, CxOut):-    
	source_term_functor(Goal,Name,Arity),
	output_functor(Name,CxIn,Cx1),
	output_args(Arity,Goal,Cx1,CxOut).	

do_comments(LeftRight,Term,CxIn,CxOut):-
	term_comments_todo(LeftRight,Term,CxIn,Ids,Comments),
	mark_comments_done(Ids,CxIn,Cx1),
	do_comments(Comments,Cx1,CxOut).

do_comments([],Cx,Cx).
do_comments([Comment|Comments],CxIn,CxOut):-
	do_comment(Comment,CxIn,CxNext),
	do_comments(Comments,CxNext,CxOut).

do_comment(Comment,CxIn,CxOut):-
    output_codes(Comments,CxIn,CxOut),
    

mark_comments_done(LeftRight,Term,CxIn,CxOut):-
	term_comments_todo(LeftRight,Term,CxIn,Ids,_),
	mark_comments_done(Ids,CxIn,CxOut).
	
mark_comments_done(Ids,CxIn,CxOut):-
	layout_comments(CxIn,CMapIn),
	map_remove_all(Ids,CMapIn,CMapOut),
	layout_set_comments(CxIn,CMapOut,CxOut).

term_comments_todo(LeftRight,Term,Cx,Ids,Comments):-
    atom_concat(comments_,LeftRight,Key),
   	(	source_term_property(Term,Key,CommentIds)
   	->	layout_comments(Cx,Map),
   		map_get_all(CommentIds,Map,Ids,Comments)
   	;	Ids=[],Comments=[]
   	).
map_get_all([],_,[],[]).
map_get_all([Key|Keys],Map,[Key|FoundKeys],[Value|Values]):-
    pdt_map_get(Map,Key,Value),
    !,
    map_get_all(Keys,Map,FoundKeys,Values).
map_get_all([_Key|Keys],Map,FoundKeys,Values):-
    map_get_all(Keys,Map,FoundKeys,Values).
    
map_remove_all([],Map,Map).
map_remove_all([Key|Keys],MapIn,MapOut):-
    (pdt_map_remove(MapIn,Key,MapNext);true),
    !,
    map_remove_all(Keys,MapNext,MapOut).

output_args(0,_,Cx,Cx).
output_args(Arity,Goal,CxIn,CxOut):-
    push_mode(arguments,CxIn,Cx0),
	output('(',Cx0,Cx1),
	inc_indent(Cx1,Cx2),	
	output_args(1,Arity,Goal,Cx2,Cx3),
	dec_indent(Cx3,Cx4),	
	new_line(Cx4,Cx5),
	align(0,Cx5,Cx6),
	output(')',Cx6,Cx7),
	pop_mode(Cx7,CxOut).
	
output_args(I,Arity,_,Cx,Cx):-
    I>Arity,
    !.
output_args(1,Arity,Goal,CxIn,CxOut):-
	source_term_arg(1,Goal,Arg),
	output_goal(Arg,CxIn,Cx1),
	output_args(2,Arity,Goal,Cx1,CxOut).
output_args(I,Arity,Goal,CxIn,CxOut):-
	J is I+1,
	output(',',CxIn,Cx1),
	new_line(Cx1,Cx2),
	align(0,Cx2,Cx3),
	source_term_arg(I,Goal,Arg),
	output_goal(Arg,Cx3,Cx4),
	output_args(J,Arity,Goal,Cx4,CxOut).
	
		
goal_left_paren(Goal,CxIn,CxOut):-
    needs_parenthesis(Goal,CxIn),
    !,
	output('(',CxIn,Cx1),
	inc_indent(Cx1,Cx2),
	align(0,Cx2,Cx3),
    push_functor(Goal,Cx3,Cx4),
    push_mode(operands,Cx4,CxOut).
goal_left_paren(Goal,CxIn,CxOut):-
	push_functor(Goal,CxIn,Cx1),
    push_mode(operands,Cx1,CxOut).

goal_right_paren(Goal,CxIn,CxOut):-    	
	pop_functor(CxIn,Cx0),
	pop_mode(Cx0,Cx1),
    needs_parenthesis(Goal,Cx1),
    !,
	dec_indent(Cx1,Cx2),
	new_line(Cx2,Cx3),
	align(0,Cx3,Cx4),
	output(')',Cx4,CxOut).
goal_right_paren(_,CxIn,CxOut):-
	pop_functor(CxIn,Cx1),
    pop_mode(Cx1,CxOut).


conjunction(Goal,_CxIn,Goals):-
    source_term_functor(Goal,',',2),
    !,
    flatten_right(Goal,Goals).


do_conjunction([],Cx,Cx).
do_conjunction([Goal],CxIn,CxOut):-
    !,
    do_goal(Goal,CxIn,CxOut).
do_conjunction([Goal|Goals],CxIn,CxOut):-
	output_goal(Goal,CxIn,Cx1),
	output_op(',',Cx1,Cx2),
	new_line(Cx2,Cx3),
	align(0,Cx3,Cx4),
	do_conjunction(Goals,Cx4,CxOut).
    
list(Term,_,_):-
    source_term_var(Term),!,fail.
    
list(Term,_,[]):-
	source_term_functor(Term,[],0),
	!.
list(Term,Cx,[Elm|Elms]):-
    source_term_arg(1,Term,Elm),
    source_term_functor(Term,'.',2),
    source_term_arg(2,Term,Tail),
    list(Tail,Cx,Elms).

list_tail(Term,[],Term):-
    source_term_var(Term),
    !.
list_tail(Term,[],[]):-
	source_term_functor(Term,[],0),
	!.
list_tail(Term,[Elm|Elms],Tail):-
	source_term_functor(Term,'.',2),
	!,
	source_term_arg(1,Term,Elm),
	source_term_arg(2,Term,Next),
	list_tail(Next,Elms,Tail).
    
flatten_right(Goal,[Left|RightGoals]):-
    source_term_arg(2,Goal,Right),
    source_term_functor(Goal,F,2),
    source_term_functor(Right,F,2),
    !,
    source_term_arg(1,Goal,Left),
    flatten_right(Right,RightGoals).
flatten_right(Goal,[Left,Right]):-
	source_term_functor(Goal,_F,2),
    source_term_arg(1,Goal,Left),	
    source_term_arg(2,Goal,Right).
    
    
    


meta_call(_CxIn,Goal):-
    source_term_expand(Goal,Term),
    \+ var(Term),
	xref_meta(Term,_Terms).
	
do_data(Term,Cx1,Cx2):-
    output_term(Term,Cx1,Cx2).
    
push_mode(NewMode,CxIn,CxOut):-
    layout_mode(CxIn,Modes),
    layout_set_mode(CxIn,[NewMode|Modes],CxOut).

pop_mode(CxIn,CxOut):-
    layout_mode(CxIn,[_Mode|Modes]),
    layout_set_mode(CxIn,Modes,CxOut).
    

push_functor(Goal,CxIn,CxOut):-
    (	term_op(Goal,CxIn,Op)
    ->	push_operator(Op,CxIn,CxOut)
    ;	source_term_var(Goal)
    ->	push_operator(var,CxIn,CxOut)
    ;	source_term_functor(Goal,Name,Arity),
    	push_operator(Name/Arity,CxIn,CxOut)
    ).
pop_functor(CxIn,CxOut):-
    pop_operator(CxIn,CxOut).
    
push_operator(Op,CxIn,CxOut):-
    layout_op(CxIn,Ops),
    layout_set_op(CxIn,[Op|Ops],CxOut).
pop_operator(CxIn,CxOut):-
    layout_op(CxIn,[_Op|Ops]),
    layout_set_op(CxIn,Ops,CxOut).
    

prefix_op(Goal,CxIn):-
    term_op(Goal,CxIn,op(_,Type,_)),
	prefix_op_X(Type).    

prefix_op_X(fx).    
prefix_op_X(fy).    

infix_op(Goal,CxIn):-
    term_op(Goal,CxIn,op(_,Type,_)),
	infix_op_X(Type).    

infix_op_X(xfx).    
infix_op_X(xfy).    
infix_op_X(yfx).    
infix_op_X(yfy).    

postfix_op(Goal,CxIn):-
    term_op(Goal,CxIn,op(_,Type,_)),
	postfix_op_X(Type).    

postfix_op_X(xf).    
postfix_op_X(yf).    

term_op(Goal,Cx,op(P,T,Name)):-
    \+ source_term_var(Goal),
    layout_module(Cx,Module),
    source_term_functor(Goal,Name, Arity),
    Module:current_op(P,T,Name),
    atom_length(T,L),
    L is Arity + 1.

output_op(Op,CxIn,CxOut):-
    output(Op,CxIn,CxOut).

output_goal(Goal,CxIn,CxOut):-
    do_goal(Goal,CxIn,CxOut).

    
inc_indent(CxIn,CxOut):-
    layout_indent_base(CxIn,Old),
    New is Old + 1,
    layout_set_indent_base(CxIn,New,CxOut).
dec_indent(CxIn,CxOut):-
    layout_indent_base(CxIn,Old),
    New is Old - 1,
    layout_set_indent_base(CxIn,New,CxOut).
    

align(Pos,CxIn,CxOut):-
    layout_indent_base(CxIn,Base),
    layout_indent_current(CxIn,Current),
    New is Base + Pos,
    (	New < 0
    ->	throw(error(negative_indent))
    ;	New < Current
    -> throw(error(cannot_move_left))
    ;	layout_set_indent_current(CxIn,New,CxOut),
    	Delta is New - Current,
    	n_times(Delta,output_raw('\t',CxOut))
    ).


n_times(0,_Goal):-
    !.
n_times(N,_Goal):-
    N<0,
    !,
    throw(domain_error(positive_integer,N)).
n_times(N,Goal):-
    Goal,
    M is N -1,
    n_times(M,Goal).

needs_parenthesis(Goal,Cx):-
    term_op(Goal,Cx,Op),
    (	peek_mode(Cx,arguments)
    ->	arg_needs_parenthesis(Goal,Cx)
    ;	op_needs_parenthesis(Op,Cx)
    ).

    

op_needs_parenthesis(op(_,T,_),Cx):-
    infix_op_X(T),
	(	peek_mode(Cx,Mode)
	->	mode_needs_parenthesis(Mode)
	;	true
	),
    !.
op_needs_parenthesis(op(P,_T,_O),Cx):-
	peek_op(Cx,op(NP,_NT,_NO)),
	P>NP. % No? YES! ;-)
arg_needs_parenthesis(Goal,_):-
    \+ source_term_var(Goal),
    source_term_functor(Goal,',',2).
mode_needs_parenthesis(clause_body).
mode_needs_parenthesis(directive_body).

new_line(CxIn,CxOut):-
    output_raw('\n',CxIn),
    layout_set_indent_current(CxIn,0,CxOut).

output(Term,Cx,Cx):-
	output_raw(Term,Cx).    

output_functor(Name,Cx,Cx):-
    layout_stream(Cx,Out),   
	write_term(Out,Name, [quoted(true),character_escapes(true)]).
	
output_raw(Term,Cx):-
    layout_stream(Cx,Out),
    write(Out,Term).
	%format("output_raw(~w, ~w)~n",[Term,Cx]).
output_term(Term,CxIn,CxOut):-
    source_term_var(Term),
    !,
    (	source_term_property(Term,variable_name,Name)
    ->	output(Name,CxIn,CxOut)
    ;	output('_',CxIn,CxOut)
    ).
output_term(List,CxIn,CxOut):-  
	list_tail(List,Elms,Tail),
	!,
	output('[',CxIn,Cx1),
	output_elms(Elms,Cx1,Cx2),	
	(	Tail=[]
	->	Cx2=CxNext
	;	output_op('|',Cx2,Cx3),
		output_term(Tail,Cx3,CxNext)
	),
	output(']',CxNext,CxOut).		
output_term(Term,CxIn,CxOut):-
    source_term_functor(Term,Name,Arity),
    (	Arity=0    	
    ->	output_functor(Name,CxIn,CxOut)
    ;	term_op(Term,CxIn,Op)
    ->	output_op_term(Op,Term,CxIn,CxOut)
    ;	output_compound_term(Term,CxIn,CxOut)	
    ).

output_op_term(op(_P,T,N),Term,CxIn,CxOut):-
    term_left_paren(Term,CxIn,Cx1),
    (	prefix_op_X(T)
    ->	output_op(N,Cx1,Cx2),
    	output(' ',Cx2,Cx3),
    	source_term_arg(1,Term,Arg),
    	output_term(Arg,Cx3,CxNext)
    ;	infix_op_X(T)
    ->	source_term_arg(1,Term,Arg1),
    	output_term(Arg1,Cx1,Cx2),
    	(	N=','
    	->	Cx2=Cx3
    	;	output(' ',Cx2,Cx3)
    	),
    	output_op(N,Cx3,Cx4),
    	output(' ',Cx4,Cx5),
    	source_term_arg(2,Term,Arg2),
    	output_term(Arg2,Cx5,CxNext)
	;	/*postfix_op*/
    	source_term_arg(1,Term,Arg),
    	output_term(Arg,Cx1,Cx2),
    	output(' ',Cx2,Cx3),
		output_op(N,Cx3,CxNext)  	
    ),
    term_right_paren(Term,CxNext,CxOut).

term_left_paren(Term,CxIn,CxOut):-
    needs_parenthesis(Term,CxIn),
    !,
	output('(',CxIn,Cx1),

    push_functor(Term,Cx1,Cx2),
    push_mode(operands,Cx2,CxOut).
term_left_paren(Term,CxIn,CxOut):-
	push_functor(Term,CxIn,Cx1),
    push_mode(operands,Cx1,CxOut).

term_right_paren(Term,CxIn,CxOut):-    	
	pop_functor(CxIn,Cx0),
	pop_mode(Cx0,Cx1),
    needs_parenthesis(Term,Cx1),
    !,
	output(')',Cx1,CxOut).
term_right_paren(_,CxIn,CxOut):-
	pop_functor(CxIn,Cx1),
    pop_mode(Cx1,CxOut).

output_elms([],Cx,Cx).	
output_elms([Elm],CxIn,CxOut):-
	!,
	output_term(Elm,CxIn,CxOut).
output_elms([Elm|Elms],CxIn,CxOut):-
	output_term(Elm,CxIn,Cx1),
	output_op(', ',Cx1,Cx2),
	output_elms(Elms,Cx2,CxOut).
	
output_compound_term(Term,CxIn,CxOut):-
	source_term_functor(Term,Name,Arity),
	output_functor(Name,CxIn,CxNext),
	output_args2(Arity,Term,CxNext,CxOut).
	
output_args2(0,_,Cx,Cx).
output_args2(Arity,Term,CxIn,CxOut):-
    push_mode(arguments,CxIn,Cx0),
	output('(',Cx0,Cx1),
	inc_indent(Cx1,Cx2),	
	output_args2(1,Arity,Term,Cx2,Cx3),
	dec_indent(Cx3,Cx4),	
	Cx4=Cx6,%new_line(Cx4,Cx5),
	%align(0,Cx5,Cx6),
	output(')',Cx6,Cx7),
	pop_mode(Cx7,CxOut).
	
output_args2(I,Arity,_,Cx,Cx):-
    I>Arity,
    !.
output_args2(1,Arity,Term,CxIn,CxOut):-
    !,
	source_term_arg(1,Term,Arg),
	output_term(Arg,CxIn,Cx1),
	output_args2(2,Arity,Term,Cx1,CxOut).
output_args2(I,Arity,Term,CxIn,CxOut):-
	J is I+1,
	output(', ',CxIn,Cx1),
%	new_line(Cx1,Cx2),
%	align(0,Cx2,Cx3),
	Cx1=Cx3,
	source_term_arg(I,Term,Arg),
	output_term(Arg,Cx3,Cx4),
	output_args2(J,Arity,Term,Cx4,CxOut).	
	
	
peek_op(Cx,Op):-
	layout_op(Cx,[Op|_]).    
	
peek_mode(Cx,Mode):-
	layout_mode(Cx,[Mode|_]).    	
	
	debugme.
