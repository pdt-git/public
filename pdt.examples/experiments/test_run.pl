:- module( find_reference_test, [go/2] ).

:- use_module(swixreftest_sub).
% Includes ... and ..._super

go(Ref,X) :- swixreftest_sub:invoke_sub(Ref,X).
go(Ref,X) :- swixreftest_sub:invoke_super(Ref,X).
go(Ref,X) :- swixreftest:invoke(Ref,X).
