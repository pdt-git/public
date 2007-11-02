:- module(ast_util,[
ast_arg/3,
ast_attach/1,
ast_attach/2,
ast_equivalent/2,
ast_match/3,
ast_variable_occurance/2,
ast_functor/3,
ast_head_body/4,
ast_strip_module/3,
ast_apply/3,
ast_root/2,
ast_node/2
]).
:- use_module(library('pef/pef_base')).    
%% 	ast_match(+Pattern,+AST,-Subst)
% match an ast against a term pattern.
% The pattern is just a usual prolog term + the following conventions: 
%
% To match a variable, use $var(Occurance:Variable). On success, Variable is unified with the 
% id of the respective pef_variable fact and Occurance with the one of the respective 
% pef_variable_occurance fact.
%
% To match a term of the form $var(A), write $var($var(A))
%
% All other (i.e. unwrapped) variables in your pattern will be unified with terms of the form
% $var(List), where List is an open list of ast nodes simultanously matched by the respective variable. A variable can simultanously match two different subterms
% if they are isomorph and if corresponding variable occurances  point to the 
% same variables. (see ast_equivalent/2).
%
% Subst is an open list of <Id> = <value> pairs. If the matched AST is more general
% then the pattern, this list contains the bindings for the variables occuring in AST.
ast_match('$var'([Head|Tail]),AST,_):-
    !,
    ast_equivalent(Head,AST),
    (	AST==Head
    ->  true
    ;	has_tail(Tail,[AST|_])
    ).
ast_match('$var'('$var'(Pattern)),AST,Subst):-
    !,
    ast_match_X('$var'(Pattern),AST,Subst).
ast_match('$var'(AST:Var),AST,_):-
    !,
    ast_variable_occurance(AST,Var).
    %pef_variable_occurance_query([id=AST,variable=Var]).
ast_match(Pattern,AST,Subst):-
    ast_match_X(Pattern,AST,Subst).

%pattern's principle functor is nonvar and unescaped.
ast_match_X(Pattern,AST,Subst):-   
    ast_variable_occurance(AST,Var),
    !,
    memberchk(Var=PatternX,Subst),
    PatternX=Pattern.    
ast_match_X(Pattern,AST,Subst):-
    functor(Pattern,Name,Arity),
    ast_functor(AST,Name,Arity),
    %pef_term_query([id=AST,name=Name,arity=Arity]),
    ast_match_args(1,Arity,Pattern,AST,Subst).

ast_match_args(I,N,_,_,_):-
    I>N,!.
ast_match_args(I,N,Pattern,AST,Subst):-
    ast_arg(I,AST,ArgAST),
	%pef_arg_query([parent=AST,num=I,child=ArgAST]),
	arg(I,Pattern,ArgPattern),
	ast_match(ArgPattern,ArgAST,Subst),
	J is I +1,
	ast_match_args(J,N,Pattern,AST,Subst).
    
	
%% ast_equivalent(+A,+B).
% succeeds if A and B point to equivalent ASTs.
%
% Two ASTs are equivalent, if
%  - they are identical, or
%  - they are two occurances of the same variable, or
%  - they have the same principal functor and for each i=1,...,n 
%    the i-th argument is equivalent to the i-th argument of B, where n is the functor arity of 
%    A and B.
ast_equivalent(A,A):-!.
ast_equivalent(A,B):-
    ast_variable_occurance(A,V),
    %pef_variable_occurance_query([id=A,variable=V]),
    !,
    ast_variable_occurance(B,V).
	%pef_variable_occurance_query([id=B,variable=V]).    
ast_equivalent(A,B):-
    ast_functor(A,Name,Arity),
    ast_functor(B,Name,Arity),
	%pef_term_query([id=A,name=Name,arity=Arity]),
	%pef_term_query([id=B,name=Name,arity=Arity]),
	ast_equivalent_args(Arity,A,B).

ast_equivalent_args(0,_A,_B):-!.
ast_equivalent_args(I,A,B):-
    ast_arg(I,A,AA),
    ast_arg(I,B,BB),
    ast_eqivalent(AA,BB),
    J is I -1,
    ast_equivalent_args(J,A,B). 
    
ast_functor(A,Name,Arity):-
    (	attvar(A)
    ->	get_attr(A,ast_match,AST),
    	pef_term_query([id=AST,name=Name,arity=Arity])	
	;	functor(A,Name,Arity)
    ).
ast_arg(I,A,AA):-
	(	attvar(A)
    ->	get_attr(A,ast_match,AST),
    	pef_arg_query([parent=AST,num=I,child=Child]),
    	put_attr(AA,ast_match,Child)	
	;	arg(I,A,AA)
    ).    
ast_variable_occurance(A,V):-
    attvar(A),
    get_attr(A,ast_match,AST),
    pef_variable_occurance_query([id=AST,variable=V]).


ast_root(Toplevel,AST):-
    pef_ast_query([toplevel=Toplevel,root=Root]),
	ast_attach(Root,AST).    
        
ast_attach(A,B):-
	ast_attach([A=B]).
ast_attach([]).
ast_attach([A=B|Subst]):-
	(	var(A), integer(B)
	->	put_attr(A,ast_match,B)
	;	var(B), integer(A)
	->  put_attr(B,ast_match,A)
	;	var(A), \+ attvar(A), attvar(B)
	->	get_attr(B,ast_match,Id),
	    put_attr(A,ast_match,Id)
	;	var(B), \+ attvar(B), attvar(A)
	->	get_attr(A,ast_match,Id),
	    put_attr(B,ast_match,Id) 
	;	nonvar(A),A='$var'(OList), nonvar(OList),var(B)
	->	copy_term(OList,OList1),
		has_tail([],OList1),
		member(AA,OList1),
		get_attr(AA,ast_match,Id),
		put_attr(B,ast_match,Id)   
	;	nonvar(B),B='$var'(OList), nonvar(OList),var(A)
	->	copy_term(OList,OList1),
		has_tail([],OList1),
		member(BB,OList1),
		get_attr(BB,ast_match,Id),
		put_attr(A,ast_match,Id)
	;	var(A),var(B)
	->	true		
	;	spyme,
		throw(expected_list_of_var_int_pairs([A=B|Subst]))
	),
	ast_attach(Subst).
	
ast_head_body(Ast,Module,Head,Body):-
	ast_strip_module(Ast,OuterModule,StrippedAst),
	(	ast_match( (H:-B) ,StrippedAst, [])
	->	H='$var'([HH|_]),
		B='$var'([Body|_]),
		ast_strip_module(HH,InnerModule,Head)
	;	ast_match( (:-B) ,StrippedAst, [])
	->	Head=[],
		InnerModule=[],
		B='$var'([Body|_])
	;	Head=StrippedAst,
		Body=[],
		InnerModule=[]
	),
	(	InnerModule==[]
	->	Module=OuterModule
	;	Module=InnerModule
	).

ast_strip_module(Ast,Module,Stripped):-
	ast_match( (M:T) ,Ast, []),
	!,
	T='$var'([TT|_]),
	M='$var'([OuterModule|_]),
	ast_strip_module(TT,InnerModule,Stripped),
	(	InnerModule==[]
	->	Module=OuterModule
	;	Module=InnerModule
	).
ast_strip_module(Ast,[],Ast).	


%% apply(+Ast,+Subst,-Term)
% renders an ast into a term, applying a given variable substitution.
%
% @param Ast the pef id of an AST node.
% @param Subst an __open__ list of VarId=Value pairs.
% @param Term will be unified with the rendered term.
ast_apply(Ast,Subst,Term):-
    ast_variable_occurance(Ast,Var),
    %pef_variable_occurance_query([id=Ast,variable=Var]),
    !,
    memberchk(Var=Term,Subst).
ast_apply(Ast,Subst,Term):-
    ast_functor(Ast,Name,Arity),
    %pef_term_query([id=Ast,name=Name,arity=Arity]),
    functor(Term,Name,Arity),
    apply_args(Arity,Ast,Subst,Term).

apply_args(0,_,_,_):-!.
apply_args(I,Ast,Subst,Term):-
	ast_arg(I,Ast,ArgAst),
	%pef_arg_query([num=I,parent=Ast,child=ArgAst]),
	arg(I,Term,ArgTerm),
	ast_apply(ArgAst,Subst,ArgTerm),
	J is I -1,
	apply_args(J,Ast,Subst,Term).

ast_node(Ast,Node):-
    attvar(Ast),
    get_attr(Ast,ast_match,Node).
    
spyme.    