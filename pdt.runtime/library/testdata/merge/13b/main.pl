:- multifile foo:bang/1.
foo:bang(main).
% now we own an ad-hoc definition of module foo.
:- use_module(extension).

% program extension.pl contains an extension of a regular module foo. We copy the extension and merge
% our ad hoc definition into it. Since we own the old ad-hoc definition, we should delete it.

