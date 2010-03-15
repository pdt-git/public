

:- use_module(original).
original:bla(main).
% now we own an extension of module original

:- use_module(other).
% other contains an ad-hoc definition of original. Since we  own the above extension, we can modify it.
