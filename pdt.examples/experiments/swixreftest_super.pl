:- module( swixreftest_super, [invoke_super/2] ).

:- module_transparent invoke_super/2.
invoke_super(super(Context),X) :- context_module(Context),call(p3(X)).

