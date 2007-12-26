:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('util/ast_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('org/cs3/pdt/util/pdt_util_io')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(layout_rules).

current_indent(File,Pos,Codes):-
    get_memory_file(File,SrcMemFile),
    open_memory_file(SrcMemFile,read,In),
    new_memory_file(TmpMemFile),
    open_memory_file(TmpMemFile,write,Out),
    call_cleanup(
    	current_indent__copy_data(Pos,In,Out),
    	(	close(In),
    		close(Out),
    		memory_file_to_codes(TmpMemFile,Codes0),
    		free_memory_file(TmpMemFile)
    	)
    ),
    current_indent__process_codes(Codes0,Codes).

current_indent__copy_data(Pos,In,Out):-    
    pef_property_query([pef=File,newlines=NewLines]),
    SearchKey is 0 - Pos,
    (	pdt_map_next(NewLines,SearchKey,FoundKey,NLStreamPos)
    ->	set_stream_position(In,NLStreamPos),
    	stream_position_data(char_count,NLStreamPos,NLCharPos)
    ;	NLCharPos=0
    ),
    Len is Pos - NLCharPos,
    copy_stream_data(In,Out,Len).
current_indent__process_codes([],[]):-!.    
current_indent__process_codes([Nl|Codes],[Codes]):-
    code_type(Nl,end_of_line),
    !.
current_indent__process_codes([Code0|Codes0],[Code|Codes]):-
	(	Code0==9
	->	Code=9
	;	Code=32
	),
	current_indent__process_codes(Codes0,Codes).
	
:- dynamic '$pef_file__memfile'/3.

get_memory_file(File,SrcMemFile):-
	get_pef_file(Path,File),
	time_file(Path,Stamp),
	(	'$pef_file__memfile'(File,Stamp,SrcMemFile)		
	->	true
	;	clean_memory_file(File),
		new_memory_file(SrcMemFile),
		copy_file_to_memfile(Path,SrcMemFile),		
		assert('$pef_file__memfile'(File,Stamp,SrcMemFile))
	).
	
clean_memory_file(File):-
    forall(
    	retract('$pef_file__memfile'(File,Stamp,MemFile)),
    	free_memory_file(MemFile)
    ).

:- pdt_define_context(
	lt(
		term,
		indent,
		child_indent,
		precedence,
		out
	)
).
token_codes(cma,_,[44]).
token_codes(tab,_,[9]).
token_codes(lpr,_,[40]).
token_codes(rpr,_,[41]).
token_codes(nl,_,[10]).
token_codes(spc,_,[32]).
token_codes(eoc,_,[46]).
token_codes(fun,L,Codes):-
    lt_term(L,Term),
    functor(Term,Name,_),
    atom_codes(Name,Codes).
token_codes(ind,L,Codes):-
	lt_indent(L,Codes).
token_codes(arg(_),_,[]).
token_codes(arg_num(_),_,[]).       
 
 
 
 
layout_ast(Ast0,File,Offset,Out):-
    ast_attach(Ast0,Ast),
    current_indent(File,Offset,Indent),
    lt_new(L),
    lt_out(Out),    
	lt_term(L,Ast),
	lt_indent(L,[]),
	lt_precedence(L,[2000]),
	lt_child_indent(L,[]),
	layout_ast(L,_).


%child_class(ParentClass,Parent,Child,ChildClass).
child_class(argument,_,Child,ChildClass):-
    (	pef_call_query([goal=Child])
    ->	ChildClass=formula
    ;	ChildClass=term
    ).
child_class(formula,Parent,Child,ChildClass):-
	(	pef_call_query([goal=Child])
    ->	ChildClass=formula
    ;	pef_call_query([goal=Parent])
    ->	ChildClass=term
    ;	ChildClass=formula
    ).  
child_class(toplevel,Parent,Child,ChildClass):-    
    pef_term_query([id=Parent,arity=Arity]),
    pef_arg_query([parent=Parent,child=Child,num=Num]),    
    (	Num < Arity)
    ->	ChildClass=head
    ;	ChildClass=formula
    ).
child_class(head,_,_,head).
    
layout_ast(LIn,LOut):-
    lt_term(LIn,Ast),    
    term_precedence(Ast,Precedence,ArgPrecedence),
    lt_precedence(LIn,[ParentPrecedence|_]),
    (	Precedence >= ParentPrecedence
    ->	HaveParenthesis = true
    ;	HaveParenthesis = false
    ),
    node_class(Ast,Class),
    ast_functor(Ast,Name,Arity),
    functor(Pattern,Name,Arity),
	once(
		(	layout_rule(Pattern,Class,HaveParenthesis,Tokens),
			ast_match(Pattern,Ast,Subst),
			var(Subst)
		)
	),	
	lt_set_precedence(LIn,ArgPrecedence,LChild),    
	layout_test_X(Tokens,LChild,LOut).
    

    	

layout_test(T):-    	
	lt_new(L),
	lt_term(L,T),
	lt_indent(L,[]),
	lt_precedence(L,[2000]),
	lt_child_indent(L,[]),
	layout_test(L,_).

layout_test(LIn,LOut):-
    lt_term(LIn,T),
    term_precedence(T,Precedence,ArgPrecedence),
    lt_precedence(LIn,[ParentPrecedence|_]),
    (	Precedence >= ParentPrecedence
    ->	HaveParenthesis = true
    ;	HaveParenthesis = false
    ),
    lt_set_precedence(LIn,ArgPrecedence,LChild),
	once(layout_rule(T,literal,HaveParenthesis,Tokens)),	
	layout_test_X(Tokens,LChild,LOut).




term_precedence(T,Precedence,Precedences):-
    ast_functor(T,Name,A),
    B is A + 1,
    current_op(Precedence,Type,Name),
    atom_length(Type,B),
    !,
    atom_chars(Type,TypeCodes),
    term_precedence__process_type(TypeCodes,Precedence,Precedences).
term_precedence(T,0,Ps):-
    ast_functor(T,_,Arity),
    term_precedence__default_type(Arity,Ps).

term_precedence__process_type([],_,[]):-!.        
term_precedence__process_type([x|Ts],Base,[Base|Ps]):-
    !,
    term_precedence__process_type(Ts,Base,Ps).
term_precedence__process_type([y|Ts],Base,[BaseX|Ps]):-
    !,
    BaseX is Base + 1,
    term_precedence__process_type(Ts,Base,Ps).
term_precedence__process_type([f|Ts],Base,Ps):-    
	term_precedence__process_type(Ts,Base,Ps).        


term_precedence__default_type(0,[]):-!.
term_precedence__default_type(N,[1000|Ts]):-
    M is N - 1,
    term_precedence__default_type(M,Ts).

layout_test_X([],L,L):-!.
layout_test_X([arg(A)|Tokens],L,LOut):-
	!,
	lt_child_indent(L,Indent),
	lt_set(L,[indent=Indent,term=A],LChild),	
	layout_test(LChild,LChildOut),
	lt_child_indent(LChildOut,ChildIndent),
	lt_precedence(L,[_|NextPrecedence]),
	lt_set(L,[child_indent=ChildIndent,precedence=NextPrecedence],LNext),
	layout_test_X(Tokens,LNext,LOut).
layout_test_X([arg_num(I)|Tokens],L,LOut):-
	!,
	lt_term(L,Term),
	arg(I,Term,A),
	lt_child_indent(L,Indent),
	lt_set(L,[indent=Indent,term=A],LChild),
	layout_test(LChild,LChildOut),
	lt_child_indent(LChildOut,ChildIndent),
	lt_set_child_indent(L,ChildIndent,LNext),
	layout_test_X(Tokens,LNext,LOut).

layout_test_X([nl|Tokens],L,LOut):-
	!,	
	token_codes(nl,L,Codes),
	format("(~w)",[nl]),
	format("~s",[Codes]),
	lt_set_child_indent(L,[],NextL),
	layout_test_X(Tokens,NextL,LOut). 
layout_test_X([ind|Tokens],L,LOut):-
	!,	
	token_codes(ind,L,Codes),
	format("(~w)",[ind]),
	format("~s",[Codes]),
	lt_indent(L,MyIndent),
	lt_set_child_indent(L,MyIndent,NextL),
	layout_test_X(Tokens,NextL,LOut).
layout_test_X([Token|Tokens],L,LOut):-
	token_codes(Token,L,Codes),
	format("(~w)",[Token]),
	format("~s",[Codes]),
	lt_child_indent(L,ChildIndent),
	(	Token==tab
	->	append_indent(ChildIndent,Codes,NextChildIndent)
	;	NextChildIndent=ChildIndent
	),
	lt_set_child_indent(L,NextChildIndent,NextL),
	layout_test_X(Tokens,NextL,LOut).	



layout_ast_X([],L,L):-!.
layout_ast_X([arg($var([A|_]))|Tokens],L,LOut):-
	!,
	lt_child_indent(L,Indent),		
	lt_class(L,Class),
	lt_term(L,Parent),
	ast_node(A,ChildNode),
	ast_node
	
	lt_set(L,[indent=Indent,term=A],LChild),	
	layout_ast(LChild,LChildOut),
	lt_child_indent(LChildOut,ChildIndent),
	lt_precedence(L,[_|NextPrecedence]),
	lt_set(L,[child_indent=ChildIndent,precedence=NextPrecedence],LNext),
	layout_ast_X(Tokens,LNext,LOut).
layout_ast_X([arg_num(I)|Tokens],L,LOut):-
	!,
	lt_term(L,Term),
	ast_arg(I,Term,A),
	lt_child_indent(L,Indent),
	lt_set(L,[indent=Indent,term=A],LChild),
	layout_ast(LChild,LChildOut),
	lt_child_indent(LChildOut,ChildIndent),
	lt_set_child_indent(L,ChildIndent,LNext),
	layout_ast_X(Tokens,LNext,LOut).

layout_ast_X([nl|Tokens],L,LOut):-
	!,	
	token_codes(nl,L,Codes),
	format("(~w)",[nl]),
	format("~s",[Codes]),
	lt_set_child_indent(L,[],NextL),
	layout_ast_X(Tokens,NextL,LOut). 
layout_ast_X([ind|Tokens],L,LOut):-
	!,	
	token_codes(ind,L,Codes),
	format("(~w)",[ind]),
	format("~s",[Codes]),
	lt_indent(L,MyIndent),
	lt_set_child_indent(L,MyIndent,NextL),
	layout_ast_X(Tokens,NextL,LOut).
layout_ast_X([Token|Tokens],L,LOut):-
	token_codes(Token,L,Codes),
	format("(~w)",[Token]),
	format("~s",[Codes]),
	lt_child_indent(L,ChildIndent),
	(	Token==tab
	->	append_indent(ChildIndent,Codes,NextChildIndent)
	;	NextChildIndent=ChildIndent
	),
	lt_set_child_indent(L,NextChildIndent,NextL),
	layout_ast_X(Tokens,NextL,LOut).	


	
append_indent(ChildIndent,Codes0,NextChildIndent):-
    current_indent__process_codes(Codes0,Codes),
    append(ChildIndent,Codes,NextChildIndent).    
