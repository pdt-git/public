:- use_module(other).
% program other contains an ad-hoc definition of module foo. We reuse it.

:- use_module(extension).

% program extension.pl contains an extension of a regular module foo. We copy the extension and merge
% our ad hoc definition into it. Since we do not own the old ad-hoc definition, we MUST NOT delete it.

