:- module(layout,[layout_node/2]).
:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).
:- use_module(library('util/ast_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_map')).
:- use_module(library('org/cs3/pdt/util/pdt_util_io')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(layout_rules).



/*
find_newlines(+InStream,-PDTMap).

scans the input stream for new line characters and returns there
position in a pdt_map.

Keys are NEGATIVE character offsets.
 (why negative? 
 the current use case needs involves right to left traversal of the 

Values are position terms returned by stream_property(_,position(Pos)). 

*/
find_newlines(In,Assoc):-
    pdt_map_empty(Assoc0),
	find_newlines(In,Assoc0,Assoc).
	
	   
find_newlines(In,Assoc0,Assoc):-
	(	at_end_of_stream(In)
	->	Assoc=Assoc0
	;	check_code(In,Assoc0,Assoc1),
		find_newlines(In,Assoc1,Assoc)
	).
	
check_code(In,Assoc0,Assoc):-
    character_count(In,CharCount),
    stream_property(In,position(Pos)), 
    get_code(In,Code),      
    (	code_type(Code,end_of_line)	
	->	% rb tree traversal is currently only implemnted from left
		% to right. We need it the other way arround. 
		% Work arround: negate keys.
		Key is -1 * CharCount,
		pdt_map_put(Assoc0,Key,Pos,Assoc)
	;	Assoc=Assoc0
	).


%% node_indent(+Node,-Indent)
% Detect base indention of a node representing existing code.
%
% We examine the code interval represented by the node, looking for the first 
% that starts with tokens that belong to this node (not to its children!).
% Any whitespace preceding this token will be assumed to be the nodes
% base indention.
%
% If there is no such line, an empty indention is returned.
% If the Node is not associated to any code interval, this predicate fails silently. 
node_indent(Node,Indent):-
    pef_property_query([pef=Node,key=tokens,value=Tokens]),
    pef_property_query([pef=Node,key=start,value=Offset]),
    relative_positions(Tokens,Offset,Tokens2),
    node_to_memory_file(Node,Mf),
    open_memory_file(Mf,read,In),
    call_cleanup(
    	node_indent_X(Tokens2,In,Indent),
    	(	close(In),
    		free_memory_file(Mf)
    	)
    ).

/*
node_indent_X([],_,[]).
node_indent_X([Token|Tokens],In,Indent):-
    token_codes(Token,In,Codes),
    
  */  

current_indent(File,Pos,Codes):-
    get_memory_file(File,SrcMemFile),
    open_memory_file(SrcMemFile,read,In),
    new_memory_file(TmpMemFile),
    open_memory_file(TmpMemFile,write,Out),
    call_cleanup(
    	current_indent__copy_data(File,Pos,In,Out),
    	(	close(In),
    		close(Out),
    		memory_file_to_codes(TmpMemFile,Codes0),
    		free_memory_file(TmpMemFile)
    	)
    ),
    current_indent__process_codes(Codes0,Codes).

current_indent__copy_data(File,Pos,In,Out):-    
    pef_property_query([pef=File,key=newlines,value=NewLines]),
    SearchKey is 0 - Pos,
    (	pdt_map_next(NewLines,SearchKey,_,NLStreamPos)
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
:- thread_local '$pef_file__memfile'/3.

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
    	retract('$pef_file__memfile'(File,_Stamp,MemFile)),
    	free_memory_file(MemFile)
    ).

:- pdt_define_context(
	lt(
		term, % the "term" to be layouted. uses the mixed representation from ast_util.
		indent, % a list of whitespace codes that shifts the cariage from start of line to the current base position		
		out, % stream to which output is written
		class, % current layout class
		hints, % a pdt_map term for keeping track of layout hints.
		stop_at%the id of an ast node. Used when running dry to collect hints for a particular node.
	)
).
token_codes(cma,_,[44]).
token_codes(tab,_,[9]).
token_codes(lpr,_,[40]).
token_codes(rpr,_,[41]).
token_codes(lsb,_,[91]).
token_codes(rsb,_,[93]).
token_codes(mid,_,[124]).

token_codes(spc,_,[32]).
token_codes(eoc,_,[46]).
token_codes(inc,_,[]):-
token_codes(arg(_),_,[]).
token_codes(arg_num(_),_,[]).    
token_codes(raw(Codes),_,Codes).   
token_codes(nl,L,[10|Indent]):-
    lt_indent(L,Indent).
token_codes(fun,L,Codes):-
    lt_term(L,Term),
    (	ast_variable_occurance(Term,Var)
    ->	pef_variable_query([id=Var,name=Name]),
    	atom_codes(Name,Codes)
    ;   ast_functor(Term,Name,_Arity),
    	format(codes(Codes),"~q",[Name])    	
    ).
    
	
 
 
%%
% layout_ast(+Node,+Offset,+Out).
% pretty-print a subtree.
%
% The layout engine ignores subterm positions attached to the ast, but otherwise
% assumes that the database is in a consistent state. In particular this extends to the
% set of operator definitions that are visible by the toplevel contitaining the 
% term beeing processed.
% Running the layout engine in the middle of a program transformation may lead to syntax errors
% in the generated program text.
%
% Comments are currently not processed.
%
% To guess the current indent level and other context-dependend information, the engine will
% examin the surrounding toplevel. 
% For the base indent level this means:
%  - Everything is fine aslong as the layout of the surrounding term follows similar indention rules 
%    as the ones used by the engine.
%  - Otherwise, the indention of the processed subterm will be inconsistent. Otherwise however the term 
%    will be correct. 
%  
% here is a rather bad example of how this can go wrong:
%  p:-(if -> then ; else).
%
% replacing 'if' by 'if,if2' and 'then' by 'then,then2' would yield something like
%  p:-(if,
%         if2 -> then,
%         then2 ; else).
%
% More sophisticated analysis of the surrounding layout may help to to 
% improve the results in this situations.
% My impression however is, that in most situations this effort would be wasted. 
% I will see how bad this limitations are when applied to real programs.
%
% @param Node an id of an ast node
% @param Out the Stream to which the layouted term should be written.
       
layout_node(Node,Out):-
    % prepare dry run down to Node to 
    % guess indention levels and layout hints.
    % Then continue producing actual output.      
    open_null_stream(Null),
	ast_toplevel(Node,Toplevel),
	ast_root(Toplevel,Root),
	lt_new(L),
    lt_out(L,Null),    
	lt_term(L,Root),
	lt_class(L,toplevel),
	lt_indent(L,""),	
	pdt_map_empty(Hints),
	lt_hints(L,Hints),
	lt_stop_at(L,Node),
	% dry run
	call_cleanup(layout_ast(L,L1),close(Null)),
	% no do it for real
	lt_set(L1,[stop_at=[],out=Out],L2),
	layout_ast(L2,_).
	
	
 
layout_ast(Ast0,File,Offset,Class,Out):-
    ast_attach(Ast0,Ast),
    current_indent(File,Offset,Indent),
    lt_new(L),
    lt_out(L,Out),    
	lt_term(L,Ast),
	lt_class(L,Class),
	lt_indent(L,Indent),	
	pdt_map_empty(Hints),
	lt_hints(L,Hints),
	layout_ast(L,_).

layout_ast(L,LStop):-
	lt_term(L,Ast),
	lt_stop_at(L,Node),
	ast_node(Ast,Node),
	!,
	lt_set_stop_at(L,now,LStop).
layout_ast(LIn,LOut):-
    lt_term(LIn,Ast),   
    lt_hints(LIn,Hints0), 
    term_precedence(Ast,Precedence),
    parent_precedence(Ast,ParentPrecedence),
    (	Precedence >= ParentPrecedence
    ->	pdt_map_put(Hints0,have_pars,true,Hints)
    ;	pdt_map_put(Hints0,have_pars,false,Hints)
    ),
    lt_class(LIn,Class),
    (	ast_var(Ast)
    ->	true
    ;	ast_functor(Ast,Name,Arity),
    	functor(Pattern,Name,Arity)
    ),    
	once(
		(	layout_rule(Pattern,Class,Hints,Tokens),
			ast_match(Pattern,Ast,Subst),
			var(Subst)
		)
	),	
	    
	layout_ast_X(Tokens,LIn,LOut).
    




parent_precedence(T,Precedence):-
    ast_node(T,ChildNode),
    (	pef_arg_query([child=ChildNode,parent=ParentNode,num=Num])
    ->	ast_attach(ParentNode,Parent),
    	term_precedence(Parent,_,ArgPrecedence),
    	nth1(Num,ArgPrecedence,Precedence)
    ;	Precedence=2000
    ).

term_precedence(T,Precedence):-
    term_precedence(T,Precedence,_).

term_precedence(T,Precedence,Precedences):-
    ast_functor(T,Name,A),
    B is A + 1,
    current_op(Precedence,Type,Name),
    atom_length(Type,B),
    !,
    atom_chars(Type,TypeCodes),
    term_precedence__process_type(TypeCodes,Precedence,Precedences).
term_precedence(T,0,Ps):-
    (	ast_var(T)
    ->	Arity=0
    ;   ast_functor(T,_,Arity)
    ),
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

layout_ast_X([],L,L):-!.
layout_ast_X([arg('$var'([A|_]))|Tokens],L,LOut):-
	!,
	lt_class(L,Class),
	lt_term(L,Parent),
	ast_node(A,ChildNode),
	ast_node(Parent,ParentNode),
	child_class(Class,ParentNode,ChildNode,ChildClass), 	
	lt_set(L,[term=A,class=ChildClass],LChild),	
	
	layout_ast(LChild,LChildOut),
	
	(	lt_stop_at(LChildOut,now)
	->	LOut=LChildOut
	;	lt_hints(LChildOut,NextHints),
		lt_set(L,[hints=NextHints],LNext),
		layout_ast_X(Tokens,LNext,LOut)
	).

layout_ast_X([arg_num(I)|Tokens],L,LOut):-
	!,
	lt_term(L,Term),
	ast_arg(I,Term,A),
	lt_class(L,Class),
	lt_term(L,Parent),
	ast_node(A,ChildNode),
	ast_node(Parent,ParentNode),
	child_class(Class,ParentNode,ChildNode,ChildClass), 	
	lt_set(L,[term=A,class=ChildClass],LChild),
	
	layout_ast(LChild,LChildOut),
	
	(	lt_stop_at(LChildOut,now)
	->	LOut=LChildOut
	;	lt_hints(LChildOut,NextHints),
		lt_set(L,[hints=NextHints],LNext),
		layout_ast_X(Tokens,LNext,LOut)
	).
layout_ast_X([inc|Tokens],L,LOut):-
	!,		
	lt_indent(L,Indent),
	token_codes(tab,L,Tab),
	append(Indent,Tab,NextIndent),
	lt_set_indent(L,NextIndent,NextL),
	layout_ast_X(Tokens,NextL,LOut).
layout_ast_X([dec|Tokens],L,LOut):-
	!,		
	lt_indent(L,Indent),
	token_codes(tab,L,Tab),
	append(NextIndent,Tab,Indent),
	lt_set_indent(L,NextIndent,NextL),
	layout_ast_X(Tokens,NextL,LOut).

layout_ast_X([Key=Value|Tokens],L,LOut):-
    !,
    lt_hints(L,Hints),
    pdt_map_put(Hints,Key,Value,HintsNext),
    lt_set_hints(L,HintsNext,LNext),
    layout_ast_X(Tokens,LNext,LOut).
layout_ast_X([Token|Tokens],L,LOut):-
	token_codes(Token,L,Codes),
	lt_out(L,Out),
	format(Out,"~s",[Codes]),	
	layout_ast_X(Tokens,L,LOut).	
