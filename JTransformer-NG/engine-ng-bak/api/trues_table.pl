% Author:
% Date: 23.12.2002

trues_table(_dnf_terms, _all_terms, _Table) :-
    maplist(filter_not, _dnf_terms, _fdnf),
    
    findall(_m, (member(_m, _all_terms), not(member(_m, _fdnf))), _diff),

    maplist(apply_not, _diff, _neg_inters),
    append(_dnf_terms, _neg_inters, _Table).
%    append(X, _neg_inters, _Table).
    
filter_not(not(X), X) :- !.
filter_not(X,Y) :- X \= not(_), Y=X.

apply_not(X, X).
apply_not(X, not(X)).


