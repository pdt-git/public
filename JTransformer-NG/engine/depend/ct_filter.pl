
:- multifile test/1.
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
comparePatternString(_a, _a).
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
    member(_p1, _plist1),
    free_variables(_p1, _free1),
    member(_p2, _plist2),
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
checkInequalities([]).
checkInequalities([disjunction(_List)|_Rest]) :- % var
   !,
   checkInequalities(_List),
   checkInequalities(_Rest).
%checkInequalities([\=(_V,_V)|_Rest]) :- !, fail.

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

/**
 * pattern(_pattStr, _varList, _valStr).
 */
test('pattern/3#1') :- pattern('* bardey',[uwe],'uwe bardey').
test('pattern/3#2') :- pattern('uwe *',[bardey],'uwe bardey').
test('pattern/3#3') :- pattern('*',['uwe bardey'],'uwe bardey').
test('pattern/3#4') :- pattern('uwe * bardey',[tarek],'uwe tarek bardey').
test('pattern/3#5') :- pattern('* tarek *',[uwe,bardey],'uwe tarek bardey').
test('pattern/3#6') :- pattern('*tarek*',['uwe ',' bardey'],'uwe tarek bardey').
%test('pattern/3#7') :- not(pattern('* tarek *',_,_)).
%test('pattern/3#8') :- not(pattern('* tarek *',[uwe,_],_)).
test('pattern/3#9') :- not(pattern('* tarek *',_,'uwe bardey')).
test('pattern/3#10'):- not(pattern('*tarek*',[uwe,bardey],'uwe tarek bardey')).
test('pattern/3#11'):- not(pattern('aaa*',[_],'bbbb')).

/*
	pattern(+Pattern, ?WildcardReplacements, +Atom)

	Example:
	pattern('* tarek *',[uwe,bardey],'uwe tarek bardey')
	*/

pattern(_pattStr, _L, _valStr) :-
    ground(_valStr),
    !,
    % throw away first pattern if empty & move _valStr for _len positions
    nextPattern(_pattStr, _firstPatt, _restPatt),
    atom_prefix(_valStr, _firstPatt),
    atom_length(_firstPatt, _len),
    atom_length(_valStr, _vlen),
    plus(_diff, _len, _vlen),
    sub_atom(_valStr, _len, _diff, _, _restVal),
    patternRest(_restPatt, _L, _restVal).

pattern(_pattStr, _varList, _ValStr) :-
    ground(_varList),
    !,
    atom_chars(_pattStr, _pattList),
    replaceWildCards(_pattList, _varList, _valList),
    flatten( _valList, _ValList),
    %format('out: ~a',_ValList),
    atom_chars(_ValStr, _ValList).


% if pattern is empty and there's no string left, finish with empty var
patternRest('',[],'') :- !.
% if pattern is empty and there's a string left, put string in var list and finish
patternRest('',[_r],_r) :- !.
patternRest(_pattStr, [_h|_t], _valStr) :-
    nextPattern(_pattStr, _nextPatt, _restPatt),
    !,
    nextVarValue(_valStr, _nextPatt, _h, _restVal),
    patternRest(_restPatt, _t, _restVal).

nextPattern('', _, _) :- !, fail.
nextPattern(_pattStr, _NextPatt, _Rest) :-
    atom_chars(_pattStr, _l),
    nextSublist(_l, _L, _R),
    !,
    atom_chars(_NextPatt, _L),
    atom_chars(_Rest, _R).

/**
 * liefere die teilliste, bis zum nächsten * oder ?
 * nextSublist(_patternList, _NextPattern, _PatternRest).
 */
nextSublist([],[], []) :- !.
nextSublist(['*'|_t],[], _t) :- !.
%nextSublist(['?'|_t],[], _t) :- !. %TODO
nextSublist([_h|_t],[_h|_Y], _T) :- nextSublist(_t, _Y, _T ).

/**
 * nextVarValue(_str, _nextPatt, _Value, _restStr).
 */
nextVarValue(_str, _nextPatt, _Value, _restStr) :-
    % find next constant pattern
    sub_atom(_str, _pos, _len, _after, _nextPatt),
    !,
    % assign everything before this constant pattern to var value
    sub_atom(_str, 0   , _pos, _     , _Value),
    % compute rest string
    plus(_pos, _len, _newPos),
    sub_atom(_str, _newPos, _after, _, _restStr).

replaceWildCards([],[],[]) :- !.
replaceWildCards(['*'|_t],[_v|_vt],[_l|_T]) :-
    ground(_v),
    !,
    atom_chars(_v, _l),
    replaceWildCards(_t, _vt, _T).
replaceWildCards(['*'|_t],[_v|_vt],['*'|_T]) :-
    var(_v),
    !,
    %term_to_atom(_v, _vName),
    %atom_chars(_vName, _l),
    replaceWildCards(_t, _vt, _T).
replaceWildCards([_h|_t],_v,[_h|_T]) :-
    replaceWildCards(_t, _v, _T).

/*
change('*',_l, _L) :-


change('?',[_h|_t], _h).

pattern(_patt,_vars, _str) :-
    nonvar(_str),
    concat_atom(_pl, '*', _patt),
    get_vars(0, _pl, _str, _vars).

get_vars(_, [], _, []).
get_vars(_lpos, [_ph|_pt], _str, [_vh|_vt]) :-
    sub_atom(_str, _pos, _len, _, _ph),
    %_pos > _lpos,
    !,
    plus(_len2,_lpos, _pos),
    sub_atom(_str, _lpos, _len2, _, _vh),

    format('found var ~a',[_vh]),

    plus(_pos, _len, _newPos),
    get_vars(_newPos, _pt, _str, _vt).

  */
/*
    
    
pattern(_patt,_vars, _str) :-
    nonvar(_str),
    atom_chars(_patt, _pl),
    atom_chars(_str, _sl),
    get_vars(0, _pl, _sl, _vars).
    
get_vars(_pos, [*|_tail],
*/



/************************** check param patterns ***************************/

sharingParamPattern(_plist1, _plist2, _p1, _p2, _x) :-
%    posMember(_p1, _plist1),
    member(_p1, _plist1),
%    _p1 \= not(_),
%    free_variables(_p1, _free1),
%    posMember(_p2, _plist2),
    member(_p2, _plist2),
%    _p2 \= not(_),
%    free_variables(_p2, _free2),
%   FIX: nur die _param variable in matchParams(..) soll verglichen werden!
%    member(_x1, _free1),
%    member(_x2, _free2),
    _p1 = matchParams(_x1,_),
    _p2 = matchParams(_x2,_),
    _x1 == _x2,
    _x = _x1.

checkParamPattern(_plist1, _plist2 ,_l1,_l2):-
    sharingParamPattern(_plist1, _plist2, _p1, _p2, _v),
    !,
    _p1 = matchParams(_,_patt1),
    _p2 = matchParams(_,_patt2),
    compareParamPatterns(_patt1, _patt2,_l1,_l2).

checkParamPattern(_plist1,_plist,_pre,_post):-
%    maplist(removeActions,_plist1, _plist1ra),
    maplist(removeActions,_post, _postra),
    matchParamsFilter(_plist1,_postra,_pre).
%    matchParamsFilter(_plist2,_l1).


matchParamsFilter([],_,_).
matchParamsFilter([matchParams(_paramIdList,_patterns)|_plist],_post,_pre):-
    ((
        is_list(_paramIdList),
        !,
        getParamTypes(_paramIdList,_post,_paramTypes),
        compareParamPatterns(_paramTypes,_patterns,_post,_pre)
    );
        true
    ),
    matchParamsFilter(_plist,_post,_pre).


getParamTypes([],_l,[]).
getParamTypes([_paramId|_paramIds],_l,[_Type|_Types]):-
    getTreeIdType(_paramId,_l,_Type),
    getParamTypes(_paramIds,_l,_Types).


%  compareParamPatterns
%  Vergleicht zwei Pattern Type / Type Listen miteiander die folgenden Elemente enthalten:
%  '..'                               - entspricht * in einer regex
%  type(basic |class, _typeid, _dim)  -
%

compareParamPatterns([], [],_,_).
compareParamPatterns(['..' | _pat1Tail], ['..' | _pat2Tail],_l1,_l2) :-
    compareParamPatterns(_pat1Tail, _pat2Tail,_l1,_l2).

compareParamPatterns([], ['..' | _pat2Tail],_l1,_l2) :-
    compareParamPatterns([], _pat2Tail,_l1,_l2).

compareParamPatterns(['..' | _pat1Tail], [],_l1,_l2) :-
    compareParamPatterns(_pat1Tail, [],_l1,_l2).

compareParamPatterns([_ | _pat1Tail], ['..' | _pat2Tail],_l1,_l2) :-
    compareParamPatterns(_pat1Tail, ['..' |_pat2Tail],_l1,_l2).

compareParamPatterns(['..' | _pat1Tail], [_ | _pat2Tail],_l1,_l2) :-
    compareParamPatterns(['..'|_pat1Tail], _pat2Tail,_l1,_l2).

compareParamPatterns(_vardefs, ['..' | _patTail],_l1,_l2) :-
    compareParamPatterns(_vardefs, _patTail,_l1,_l2).

compareParamPatterns([_pat1 | _pat1Tail], [[_pat2 , _] | _pat2Tail],_l1,_l2) :-
    !,
    compareParamPatterns([_pat1 | _pat1Tail], [_pat2 | _pat2Tail],_l1,_l2).

compareParamPatterns([[_pat1 | _]| _pat1Tail], [_pat2  | _pat2Tail],_l1,_l2) :-
    !,
    compareParamPatterns([_pat1 | _pat1Tail], [_pat2 | _pat2Tail],_l1,_l2).

/*
compareParamPatterns([type(basic,_name,_dim) | _pat1Tail], [type(basic,_name,_dim) | _pat2Tail],_l1,_l2) :-
    _name == _name;
    compareParamPatterns(_pat1Tail, _pat2Tail,_l1,_l2).
*/

compareParamPatterns([type(_basicOrClass2,_id1,_dim) | _pat1Tail], [type(_basicOrClass2,_id2,_dim) | _pat2Tail],_l1,_l2) :-
% TODO subtype, Klassen aus unterschiedlichen Packages
% TODO: besser flat_equal ???
    getTreeTypeName(_basicOrClass1, _l1,_id1,_name),
    getTreeTypeName(_basicOrClass2, _l2,_id2,_name),
    compareParamPatterns(_pat1Tail, _pat2Tail,_l1,_l2).

compareParamPatterns([type(_basicOrClass,_id,_dim) | _pat1Tail], [typePattern(_pattern,_dim) | _pat2Tail],_l1,_l2) :-
% TODO subtype, Klassen aus unterschiedlichen Packages
    getTreeTypeName(_basicOrClass, _l1,_id,_name),
    pattern(_pattern, _, _name),
    compareParamPatterns(_pat1Tail, _pat2Tail,_l1,_l2).

compareParamPatterns([typePattern(_pattern,_dim) | _pat1Tail], [type(_basicOrClass,_id,_dim) | _pat2Tail],_l1,_l2) :-
% TODO subtype, Klassen aus unterschiedlichen Packages
    getTreeTypeName(_basicOrClass, _l2,_id,_name),
    pattern(_pattern, _, _name),
    compareParamPatterns(_pat1Tail, _pat2Tail,_l1,_l2).

compareParamPatterns([typePattern(_patt1,_dim) | _pat1Tail], [typePattern(_patt2,_dim) | _pat2Tail],_l1,_l2) :-
% TODO subtype, Klassen aus unterschiedlichen Packages
    comparePatternString(_patt1,_patt2),
    compareParamPatterns(_pat1Tail, _pat2Tail,_l1,_l2).

/*
compareParamPatterns([type(basic,_name,_dim) | _pat1Tail], [type(basic,_name,_dim) | _pat2Tail],_l1,_l2) :-
    _name == _name;
    compareParamPatterns(_pat1Tail, _pat2Tail,_l1,_l2).
*/

getTreeTypeName(class, _trees,_id,_name):-
    member(classDefT(_cid,_,_name,_),_trees),
    _cid == _id.

getTreeTypeName(basic, _trees,_name,_name).

/***************************************************************************/

getTreeIdType(_id,_l, _Type):-
    (
        exists_tree(_id, _l, _Tree),
        !,
        getTreeType(_Tree,_l,_Type)
    );
    _Type = '..'.

/***************************************************************************/

getTreeType(localDefT(_, _, _,_Type, _,_),_,_Type):-!.
getTreeType(fieldDefT(_, _, _Type, _,_),_,_Type):-!.
getTreeType(paramDefT( _, _,_Type, _),_,_Type):-!.

getTreeType(identT(_ident,_,_,_,_ref),_l, _Type) :-
    !,
    getTreeIdType(_ref,_l, _Type).


getTreeType(getFieldT(_,_,_,_,_,_ref), _l,_Type) :-
    !,
    getTreeIdType(_ref,_l, _Type).

getTreeType(selectT(_,_,_,_,_,_ref), _l,_Type) :-
    !,
    getTreeIdType(_ref,_l, _Type).

getTreeType(methodDefT(_methodDef,_, _,_, _Type, _,_), _,_Type):-!.

getTreeType(literalT(_literal,_,_,_Type,_),_, _Type):-!.

getTreeType(classDefT(_class,_,_,_), _,type(class, _class, 0)):-!.

getTreeType(newClassT(_class,_,_,_,_,_ref,_,_), _,_type) :-
    !,
    getTreeIdType(_ref,_l, _Type).


/***************************************************************************/

bdd(_n):-
    dst(_n).
    
bdd(_n):-
    write('hey2').

dst(_n):-
%    !,
    write('hey1').
dst(_n):-
    write('hey3').
