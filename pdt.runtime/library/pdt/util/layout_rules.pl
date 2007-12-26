:- module(layout_rules,[layout_rule/4]). 
layout_rule((A,B), literal, true, [    tab,lpr,tab,arg(A),fun,nl,
								   ind,tab,tab,arg(B),nl, 
								   ind,tab,rpr
								  ]).                             
layout_rule((A,B), literal, false,[    arg(A),fun,nl,
								   ind,arg(B)
								  ]).            	
layout_rule((A->B), literal, true, [    tab,lpr,arg(A),nl,
                                    	ind,tab,fun,arg(B),nl,
                                    	ind,tab,rpr
                                   		]).
layout_rule((A->B), literal, false,[   arg(A),nl,
                                    	ind,fun,arg(B)
                                   		]).
layout_rule((A;B), literal, HavePars,Tokens):-
	layout_rule((A->B),literal,HavePars,Tokens).
layout_rule((A *-> B), literal, HavePars,Tokens):-
	layout_rule((A->B),literal,HavePars,Tokens).


layout_rule( (:-A), _,_,[fun,spc,arg(A),eoc]).
layout_rule( (A:-B), _,_,[arg(A),spc,fun,nl,
                                    arg(B),eoc]).

layout_rule( A ,Where,_,TokensOut):-
	nonvar(A),    
    functor(A,_,Arity),
    (	Arity > 0 
    ->	arg_layout([cma,spc,arg_num(Arity)],ArgTokens),
    	append([fun,lpr|ArgTokens],[rpr],Tokens)
    ;	Tokens=[fun]
    ),
    (	Where==literal
    ->	TokensOut=[tab|Tokens]
    ;	TokensOut=Tokens
    ).

arg_layout([cma,spc,arg_num(1)|More],[arg_num(1)|More]).
arg_layout([cma,spc,arg_num(I)|More],EvenMore):-
	J is I - 1,
	arg_layout([cma,spc,arg_num(J),cma,spc,arg_num(I)|More],EvenMore).
