:- multifile foo:bang/1.
foo:bang(main).
% now we own an ad-hoc definition of module foo.
:- use_module(foo).

% program foo.pl defines a regular module foo. We drop our ad hoc definition in favour 
% of an extension of that module definition
