

:- use_module(extension).
% extension contains an extension of original. we reuse it.

:- use_module(other).
% other contains an ad-hoc definition of original. Since we do not own the above extension, we have to copy it.
