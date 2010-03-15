
:- use_module(first).
%first.pl contains an ad-hoc definiiton of module foo. We reuse it.

:- use_module(other).

% other.pl contains another ad-hoc definition of module foo. We need to copy the first definition and then merge 
% the other.