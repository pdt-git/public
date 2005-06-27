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


checkPattern(_plist1, _plist2) :-
    sharingPattern(_plist1, _plist2, _p1, _p2, _v),
    !,
    comparePattern(_p1, _p2, _v).
% if there are no sharing variables, try to apply all patterns and check that none fails
checkPattern(_plist1, _plist2) :-
    checklist(applyPattern, _plist1),
    checklist(applyPattern, _plist2).

% TODO
comparePattern(pattern(_a, _, _),   pattern(_a, _, _), _) :- !.
comparePattern(pattern(_a, _b, _c), pattern(_d, _e, _f), _x) :-
    _c == _x,
    _f == _x,
    not(comparePatternString(_a, _d)),
    !,
%    dformat('failing to compare pattern ~a with ~a.~n',[pattern(_a, _b, _c), pattern(_d, _e, _f)]),
    fail.
comparePattern(not(pattern(_a, _b, _c)), pattern(_d, _e, _f), _x) :-
    _c == _x,
    _f == _x,
    comparePatternString(_a, _d),
    !,
%    dformat('failing to compare pattern ~a with ~a.~n',[pattern(_a, _b, _c), pattern(_d, _e, _f)]),
    fail.
comparePattern(pattern(_a, _b, _c), not(pattern(_d, _e, _f)), _x) :-
    _c == _x,
    _f == _x,
    comparePatternString(_a, _d),
    !,
%    dformat('failing to compare pattern ~a with ~a.~n',[pattern(_a, _b, _c), pattern(_d, _e, _f)]),
    fail.
comparePattern(_, _, _).

% vergleicht zwei pattern Strings.
% wenn beide mindestens 1 * enthalten, werden prefix und postfix bis zum erreichen eines * verglichen
% TODO: ist ein term konstant, wird das normale pattern/3 prädikat verwendet
test('comparePatternString#1') :- comparePatternString('set*','se*').
test('comparePatternString#2') :- comparePatternString('*','se*reterter').
test('comparePatternString#3') :- comparePatternString('set*','setMyClass').
test('comparePatternString#4') :- not(comparePatternString('set*','a*')).
comparePatternString(_a, _d) :-
    atom_chars(_a, _l1),
    atom_chars(_d, _l2),
    comparePatternString_(_l1, _l2),
    reverse(_l1, _l1r),
    reverse(_l2, _l2r),
    comparePatternString_(_l1r, _l2r).
comparePatternString_([*|_t],_) :- !.
comparePatternString_(_, [*|_t]) :- !.
comparePatternString_([_c|_t1],[_c|_t2]) :-
    !,
    comparePatternString_(_t1, _t2).
    


%comparePattern(_, _, _).
/*comparePattern(pattern(_a, _b, _v), pattern(_c, _d, _v), _v) :-
    pattern(_a, _b, _c),
    pattern(_c, _d, _a).
*/

/**
 * Apply Pattern if possible
 */
applyPattern(not(pattern(_p, _v, _s))) :-
    !,
    not(pattern(_p, _v, _s)).
applyPattern(pattern(_p, _v, _s)) :-
    (ground(_v); ground(_s)),
    !,
    pattern(_p, _v, _s).
applyPattern(_).

    
posMember(_m, _l) :- member(not(_m), _l).
posMember(_m, _l) :- member(_m, _l), _m \= not(_).

sharingPattern(_plist1, _plist2, _p1, _p2, _x) :-
%    posMember(_p1, _plist1),
    member(_p1, _plist1),
%    _p1 \= not(_),
    free_variables(_p1, _free1),
%    posMember(_p2, _plist2),
    member(_p2, _plist2),
%    _p2 \= not(_),
    free_variables(_p2, _free2),
    member(_x1, _free1),
    member(_x2, _free2),
    _x1 == _x2,
    _x = _x1.
        
        
        
        
/** Preprocessing of negation and equality literals:
    1. "equals(_t1,_t2)" literals from the first argument
       are pre-evaluated by unifying _t1 and _t2.
    2. "not(equals(_t1,_t2))" literals from the first argument
       can only be evaluated latter, during the computation of
       dependencies. They are extracted as "notequal(_var,_term)"
       literals into the third argument, to ease deferred processing.
    3. For other "not(_term)" literals from the first argument
       only _term is added to the second argument. These negations
       are irrelevant for the computation of dependencies.
 **/
   % _Lwith    = ListWithNegationAndEquality
   % _lwithout = listWithoutNegationAndEquality
   % _norm     = normalizednotequalList
/*
removeNotAndEquals(_Lwith,_lwithout,_normalizednotequalList) :-
    removeNotAndEquals(_Lwith,_lwithout, [],_notequalList),
    normalizeInequality( _notequalList,_normalizednotequalList).

removeNotAndEquals([], [], _Ne, _Ne) :-
    !.
removeNotAndEquals([=(_T1,_T2)|_Body], _body, _Ne, _ne) :-
    !,
    _T1 = _T2,
    removeNotAndEquals(_Body, _body, _Ne , _ne ).

removeNotAndEquals([pattern(_p, _v, _s)|_Body], [pattern(_p, _v, _s)|_body], _Ne, _ne) :-
    !,
    pattern(_p, _v, _s),
    removeNotAndEquals(_Body, _body, _Ne , _ne ).

removeNotAndEquals([\=(_T1,_T2)|_Body], _body, _Ne, [\=(_T1,_T2)|_ne]) :-
    !,
    removeNotAndEquals(_Body, _body, _Ne , _ne ).
removeNotAndEquals([not(_Pred)|_Body], [_Pred|_body], _Ne, _ne) :-
    !,
    % not(_Pred=equals(_,_)) gilt bereits durch ! in vorheriger Klausel
    removeNotAndEquals(_Body, _body, _Ne , _ne ).
removeNotAndEquals([_Head|_Body], [_Head|_body], _Ne, _ne) :-
    %not(_Head = not(_x)) gilt bereits durch ! in vorheriger Klausel
    removeNotAndEquals(_Body, _body, _Ne, _ne ).
*/



/******************************* Normalization ******************************/
/** The list of inequalities in the first arguments is equivalent to
    the list of inequalities in the second argument. The latter
    contains only terms of the form "notequal(_variable,_term)" or
    or "disjunction(_reslist)" where _reslist is a list of such terms.
    The latter case represents a disjunction of inequalities.
 **/

normalizeInequality([],[]).
normalizeInequality([\=(_T1,_T2)|_Body],[_head|_body]):-
    normalizeInequalityPair(_T1,_T2,_head),
    normalizeInequality(_Body,_body).

normalizeInequalityPair(_T1,_T2,\=(_var,_term)) :-
   var(_T1),
   !,
   _var = _T1,
   _term = _T2.
normalizeInequalityPair(_T1,_T2,\=(_var,_term)) :-
   var(_T2),
   !,
   _var = _T2,
   _term = _T1.
normalizeInequalityPair(_T1,_T2,disjunction(_reslist)) :-
   _T1 =.. [_functor1 | _args1],
   _T2 =.. [_functor2 | _args2],
   _functor1 = _functor2,
   normalizeInequalityLists(_args1,_args2,_reslist).
   % Die Bedingung "length(_args1, _n), length(_args2, _n)"
   % ist implzit in dem rekursiven Aufruf.

/**Die impliziten notequal-paare der argumentlisten normalisieren:
 **/
normalizeInequalityLists([],[],[]).
normalizeInequalityLists([_H1|_Body1],[_H2|_Body2],[_head|_body]) :-
   normalizeInequalityPair(_H1,_H2,_head),
   normalizeInequalityLists(_Body1,_Body2,_body).


/** Überprüfen, ob Ungleichungen nun auswertbar sind und gegebenenefalls
    auswerten. Eine normalisierte Ungleichung ist auswertbar, wenn ihre
    Variable instantiiert ist.
 **/
checkInequalities([]) :- !.

%checkInequalities([disjunction(_List)|_Rest]) :- % var
%   !,
%   checkInequalities(_List),
%   checkInequalities(_Rest).
%%checkInequalities([\=(_V,_V)|_Rest]) :- !, fail.

checkInequalities([\=(_Var,_Term)|_Rest]) :- % var
   var(_Var),
   nonvar(_Term),%Uwe hinzugefügt falls beides variablen sind: _g112=_g112
   !,
   checkInequalities(_Rest).
checkInequalities([\=(_Var,_Term)|_Rest]) :- % var
   var(_Var),
   var(_Term),%Uwe hinzugefügt falls beides variablen sind: _g112=_g112
   !,
   _Var \== _Term,                              % eigentlicher check
   checkInequalities(_Rest).
checkInequalities([\=(_Var,_Term)|_Rest]) :- % nonvar
    !,
   _Var \= _Term,                              % eigentlicher check
   checkInequalities(_Rest).




/*
checkPatterns([]) :- !.
checkPatterns([pattern(_patt, _vars, _str)|_Rest]) :- % pattern
    format('checking pattern ~a~n',[_patt]),
   (ground(_str);ground(_vars)),
    !,
   pattern(_patt, _vars, _str),
   checkPatterns(_Rest).
checkPatterns([_h|_t]) :-
    checkPatterns(_t).
*/


