:- module(foo,[bar/0, bang/1]).
bar.
baz.
:- multifile bang/1.
bang(foo).