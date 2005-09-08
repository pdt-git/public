% DEAD CODE (ENTIRE FILE)

/** Filtering: Get preprocessed conditional transformer clauses.
 **/
ct_filter(_name, _Pre, _Act, _PreNE, _PrePatt) :-
       ct(_name, _prexy, _actxy),
       preproccessDASyntax(_prexy, _actxy, _pre, _Act),
       removeEqualsAndPattern(_pre ,_Pre ,_preIneq, _PrePatt),
       normalizeInequality(_preIneq, _PreNE).
       
preproccessDASyntax(_prexy, _actxy, _pre, _Act) :-
       % falls dnf ausdruck forme in liste um
       semicolon_member(_kc, _prexy),
%       comma_member(_kc, _sc),
       make_list(_kc, _pre),
       % falls dnf ausdruck forme in liste um
       semicolon_member(_ka, _actxy),
%       comma_member(_ka, _sa),
       make_list(_ka, _act),
       % falls operatoren add/1, delete/1 replace/2 verwendet wurden, expandiere diese
       maplist(mapEffect, _act, _Act).


make_list(_l, _l) :- is_list(_l), !.
make_list(_k, _l) :- comma2list(_k, _l).
%make_list(';'(_a,_b), _l) :- semicolon2list(';'(_a,_b), _l).

mapEffect(add(_a),_a) :- !.
mapEffect(delete(_a),not(_a)) :-  !.
mapEffect(replace(_a,_b), replace(_b)) :-  !.
mapEffect(_x,_x).


removeEqualsAndPattern([],[],[],[]) :- !.
removeEqualsAndPattern([\=(_a, _b)|_t], _T, [\=(_a, _b)|_NE], _PT) :-
    !,
    removeEqualsAndPattern(_t, _T, _NE, _PT).
removeEqualsAndPattern([=(_a, _b)|_t], _T, _NE, _PT) :-
    _a = _b,
    !,
    removeEqualsAndPattern(_t, _T, _NE, _PT).
removeEqualsAndPattern([pattern(_p, _v, _s)|_t], _T, _NE, [pattern(_p, _v, _s)|_PT]) :-
    (ground(_v) ; ground(_s)),
    !,
    pattern(_p, _v, _s),
    removeEqualsAndPattern(_t, _T, _NE, _PT).
removeEqualsAndPattern([pattern(_p, _v, _s)|_t], _T, _NE, [pattern(_p, _v, _s)|_PT]) :-
    !,
    removeEqualsAndPattern(_t, _T, _NE, _PT).
removeEqualsAndPattern([not(pattern(_p, _v, _s))|_t], _T, _NE, [not(pattern(_p, _v, _s))|_PT]) :-
    !,
    removeEqualsAndPattern(_t, _T, _NE, _PT).
removeEqualsAndPattern([_h|_t], [_h|_T], _NE, _PT) :-
    removeEqualsAndPattern(_t, _T, _NE, _PT).

