:- module(fonk,[]).
:- use_module(foo2).
% This adds another twist:
% foo actually refers to the module defined in foo2.pl. But when loaded from main, we have a module name
% conflict, the last directive does not succeed and the name foo is still bound to the module defined in foo.pl
% During consulting of main, subsequent extensions to foo will therefor go into the module defined in foo.pl.
%
foo:rabatz.