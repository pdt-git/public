% Author:
% Date: 24.09.02

% checks if 2 ct's begin with same name prefix
ct_same_prefix(_Prefix, _Ct1, _Ct2) :-
    ct(_Ct1,_,_),
    ct(_Ct2,_,_),
    _Ct1 \== _Ct2,
    term_to_atom(_Ct1, _a1),
    term_to_atom(_Ct2, _a2),
    atom_prefix(_a1, _Prefix),
    atom_prefix(_a2, _Prefix).

:- [accessorCounterDependencies].
:- [mixedHighLowLevelDependencies].
:- [abstractDependencies].
:- [replaceDependencies].
:- [referenceDependencies].

