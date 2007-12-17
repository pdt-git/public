:- use_module(library('pef/pef_base')).
:- use_module(library('pef/pef_api')).

ast_token(Ast,Pos,Type):-
    (	pef_type(Ast,pef_variable_occurance)
    ->	var_token(Ast,Pos,Type)
    ;	term_token(Ast,Start,End,Type)
    ).
term_token(Ast,Start,End,Type):-
	pef_term_query([id=Ast,arity=Arity]),
	between(1,Arity,I), %make sure arguments are visited in order!
	pef_arg_query([parent=Ast,num=I,child=Arg]),
	
	
	    