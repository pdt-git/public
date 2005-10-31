/**
 * Translation of expressions in disjunctive normal form via dnf/2.
 * The simplify/2 predicate is made public just for testing purposes.
 * There are currently no clients using it (gk, 19.09.2005).
 */
:- module('condor.depend.expand.dnf', [dnf/2, simplify/2]).

dnf(true, true)     :- !.
dnf(false,false)    :- !.

dnf(not(_term), _Term):-
    dnf(_term,_tmp),
    notdnf(not(_tmp), _Term),
    !.

dnf(','(_term1,_term2), _Term) :-
    dnf(_term1,_tmp1),
    dnf(_term2,_tmp2),
    anddnf(','(_tmp1,_tmp2),_Term2),
    simplify(_Term2, _Term),
    !.

dnf(';'(_term1,_term2), _Term):-
    dnf(_term1,_Term1),
    dnf(_term2,_Term2),
    ordnf(';'(_Term1,_Term2), _Term),
    !.

dnf(_term,_term). % normales literal: weder true, false, not/1, ','/2, ';'/2


anddnf(','(';'(_x,_y),_z),_X):-  !, dnf(';'(','(_x,_z),','(_y,_z)),_X).
anddnf(','(_z,';'(_x,_y)),_X):-  !, dnf(';'(','(_z,_x),','(_z,_y)),_X).
anddnf(','(','(_a,_b),_c),_X):- !, dnf(','(_a,','(_b,_c)),_X).
anddnf(','(_x,_y),','(_x,_y)).


ordnf(';'(';'(_a,_b),_c),_X) :- !, dnf(';'(_a,';'(_b,_c)), _X).
ordnf(';'(_term1,_term2), ';'(_term1,_term2)).


notdnf(not(not(_x)),_X)     :- !, dnf(_x,_X).
notdnf(not(','(_x,_y)),_X)  :- !, dnf(';'(not(_x),not(_y)),_X).
notdnf(not(';'(_x,_y)),_X)  :- !, dnf(','(not(_x),not(_y)),_X).
notdnf(not(_x),not(_x)).



simplify(_h, _h) :-
    _h \= ','(_,_),
    !.
simplify(','(_h, _t), _T) :- % streicht später vorkommende literale (alternativ durch true ersetzen)
    comma_member(_m, _t),
    _m == _h,
    !,
    simplify(_t, _T).
simplify(','(_h, _t), ','(false,_T)) :- % ersetzt später negiert vorkommende literale durch false
    comma_member(_m, _t),
    (_m == not(_h); not(_m) == _h),
    !,
    simplify(_t, _T).
simplify(','(_h, _t), ','(_h,_T)) :-
    simplify(_t, _T).


test('dnf_and')     :-  dnf(((1,2),3,4),(1,2,3,4)).
test('dnf_or')      :-   dnf(((1;2);3;4),(1;2;3;4)).
test('dnf_complex1'):- dnf((1,(2,not((4,(7;6)));3)),(1, 2, not(4);1, 2, not(4), not(6);1, 2, not(7), not(4);1, 2, not(7), not(6);1, 3)).

test('dnf_term')    :- dnf( (var(V,C,x),(class(C,P,ooo);package(P,foo))), (var(V, C, x), class(C, P,ooo);var(V, C, x), package(P, foo))).

test(dnf_simple1) :- dnf((a,b,a),(b,a)).
test(dnf_simple2) :- dnf((not(a),b,not(a)),(b,not(a))).
test(dnf_simple3) :- dnf((not(a),b,a),(false,b,a)).
test(dnf_simple4) :- dnf((a,b,not(a)),(false,b,not(a))).
test(dnf_simple5) :- dnf((a(X),b,a(X)),(b,a(X))).
test(dnf_simple6) :- dnf((a(Y),b,a(X)),(a(Y),b,a(X))).

%test('dnf_or') :-
%    dnf(((1;2);3;4),(1;2;3;4)).
%test('dnf_vars') :-
%    dnf_vars((4,(_a;((a;_b;not(_a)),b))),(4, _a;4, a, b;4, _b, b;4, not(_a), b)).


/* ************* Rest is dead code ******************************** */

/*
unwrapVars(_term, _Translated) :-
    _term =.. [_name | _args],
    echangeVarNamesWithVar(_args, _translatedArgs),
    _Translated =.. [_name | _translatedArgs].

echangeVarNamesWithVar([], []) :- !.
echangeVarNamesWithVar([wrappedVar(_var) | _t], [_var | _rest]) :-
%    atom_to_term(_varname,_Term, _binding),
    echangeVarNamesWithVar(_t, _rest).

echangeVarNamesWithVar([_term | _t], [_Term | _rest]) :-
    not(atomic(_term)),
    !,
    unwrapVars(_term,_Term),
    echangeVarNamesWithVar(_t, _rest).
echangeVarNamesWithVar([_term | _t], [_term | _rest]) :-
    echangeVarNamesWithVar(_t, _rest).
*/

/* ************************************************************************** */


/*
DNF1 : dnf(True) -> True
DNF2 : dnf(False) -> False
DNF3 : dnf(Atom(x)) -> Atom(x)
DNF4 : dnf(Not(x)) -> not(dnf(x))
DNF5 : dnf(And(x,y)) -> and(dnf(x),dnf(y))
DNF6 : dnf(Or(x,y)) -> Or(dnf(x),dnf(y))

AND1 : and(Or(x,y),z) -> Or(and(x,z),and(y,z))
AND2 : and(z,Or(x,y)) -> Or(and(z,x),and(z,y))
AND3 : and(x,y) -> And(x,y) (default)

NOT1 : not(Not(x)) -> x
NOT2 : not(And(x,y)) -> Or(not(x),not(y))
NOT3 : not(Or(x,y)) -> and(not(x),not(y))
NOT4 : not(x) -> Not(x) (default)
*/

/*
dnf(true, true).
dnf(false,false).
dnf(_term,_term):-
    atomic(_term).

dnf(not(_term), _ReTerm):-
    dnf(_term,_tmp),
    notdnf(not(_tmp), _Term),
    dnf(_Term,_ReTerm),
    !.
dnf(not(_term), not(_term)).

dnf(and(_term1,_term2), _ReTerm) :-
    dnf(_term1,_tmp1),
    dnf(_term2,_tmp2),
    anddnf(and(_tmp1,_tmp2),_Term),
    dnf(_Term, _ReTerm),
    !.
dnf(and(_term1,_term2), and(_term1,_term2)).

    
dnf(or(_term1,_term2), or(_Term1,_Term2)):-
    dnf(_term1,_Term1),
    dnf(_term2,_Term2).

anddnf(and(or(_x,_y),_z),or(and(_x,_z),and(_y,_z))):-!.
anddnf(and(_z,or(_x,_y)),or(and(_z,_x),and(_z,_y))):-!.
%anddnf(and(_x,_y),and(_x,_y)).

notdnf(not(not(_x)),_x):-!.
notdnf(not(and(_x,_y)),or(not(_x),not(_y))):-!.
notdnf(not(or(_x,_y)),and(not(_x),not(_y))):-!.
%notdnf(not(_x),not(_x)).
*/


%dnf_vars(_term, _Term) :-
%    dnf(_term, _atom_term),
%    unwrapVars(_atom_term, _Term).


%dnf(_term,wrappedVar(_term)):-
%    var(_term),
%    !.

%    term_to_atom(_term, _Atom),
%    assert1T(varTable(_Atom,[_Atom = _term])).

%dnf(wrappedVar(_term),wrappedVar(_term)).
