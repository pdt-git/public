

:- use_module(other).
% other contains an ad hoc definition of module foo. we reuse it.

:- use_module(foo).

% program foo.pl defines a regular module foo. We drop our ad hoc definition in favour 
% of an extension of that module definition. Since we do not own the old definition, we must not delete it, though.

