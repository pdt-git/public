:- module(swixreftest,[p1/1,p2/1,p3/1,invoke_super/2]).

:- use_module(swixreftest_super).

%:-multifile swixreftest_super:invoke_super/2.
%
%invoke_super(overriding,X).


invoke(1,X) :- call(p2(X)).
invoke(2,X) :- user_defined_meta1(p3(X)).
invoke(3,X) :- user_defined_meta2(p3,[X]).
invoke(4,X) :- user_defined_meta3(p3,[X]).

user_defined_meta1(G) :-                       call(G).
user_defined_meta2(F,Args) :- G =..  [F|Args], call(G).
user_defined_meta3(F,Args) :- alias(F,F1),
	                          G =.. [F1|Args], call(G).

alias(X,X).

p1(p1). % not called
p2(p2). % called via instantiated metacall
p3(p3). % apparently not called but actually there is
        %  - a variable metacall in this module
        %  - an instantiated metacall in a submodule
        %  - an instantiated metacall in a supermodule

blub :- meta_predicate(bla).