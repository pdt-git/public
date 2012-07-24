/* $LICENSE_MSG$ */

:- module(multifileUser, [b/1]).

:- use_module(multifileA). 

b(A):-multi(A).

