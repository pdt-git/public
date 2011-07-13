:- module(swixreftest_sub, []).

:- use_module(swixreftest).

invoke_sub(sub,X) :- call(p3(X)). 
