:- dynamic countvar/1.
:- multifile countvar/1.

newID(_New) :- lastID(_last), sum(_last, 1, _New), retract(lastID(_last)), assert(lastID(_New)).

countvar(0).

incCounter(_i) :-
    not(nonvar(_i)),
    countvar(_old),
    retractall(countvar(_)),
    plus(_old, 1, _i),
    assert(countvar(_i)).


counter(_i) :-
    nonvar(_i),
    !,
    retractall(countvar(_)),
    assert(countvar(_i)).

counter(_i) :-
    not(nonvar(_i)),
    !,
    countvar(_i).

    

newIDs([]).
newIDs([_H | _T]) :-
    newID(_H),
    newIDs(_T).

stringAppend(_str1, _str2, _str3, _Ret) :-
    stringAppend(_str1, _str2, _tmpName),
    stringAppend(_tmpName, _str3, _Ret).

stringAppend(_str1, _str2, _str3, _str4, _Ret) :-
    stringAppend(_str1, _str2, _str3, _tmpName),
    stringAppend(_tmpName, _str4, _Ret).

append(_first, _second, _third, _Ret) :-
    append(_first, _second, _dummyList),
    append(_dummyList, _third, _Ret).

append(_first, _second, _third, _fourth, _Ret) :-
    append(_first, _second, _third, _dummyList),
    append(_dummyList, _fourth, _Ret).


getSymbol(_id, _Symbol) :- selectT(_id, _, _, _, _, _Symbol).
getSymbol(_id, _Symbol) :- identT(_id, _, _, _, _Symbol).

getSymbolName(_id, _name) :-
    methodDefT(_id, _, _name, _,_,_, _),!.
getSymbolName(_id, _name) :-
    varDefT(_id,_,_,_,_name, _),!.
getSymbolName(_id, _name) :-
    classDefT(_id,_, _name, _),!.
getSymbolName(_id, 'null').

prepend(_list, _elem, _return) :-
    append([_elem],_list,_return).

append3(_x,_y,_z,_xyz) :- append(_x,_y,_xy), append(_xy,_z,_xyz).
beforeInBlock(_elem, _newElem, _EnclBlock, _NewList) :-
    enclBlockMember(_elem, _BlockMember),
    tree(_BlockMember, _EnclBlock, _),
    blockT(_EnclBlock, _BlockParent, _, _List),
    insertBefore(_List, _BlockMember, _newElem, _NewList).

insertBefore([_Head | _Tail], _elem, _newElem, _RetList) :-
    equals(_Head, _elem),
    append3([_Head], [_newElem], _Tail, _RetList).

/*.equals(_RetList, [_newElem,_Head , _Tail]).*/
insertBefore([_Head | _Tail], _elem, _newElem, _RetList) :-
    not(equals(_Head, _elem)),
    insertBefore(_Tail, _elem, _newElem, _NewTail),
    append([_Head], _NewTail, _RetList).

insertBefore([], _elem, _newElem, _RetList) :- equals(_RetList, []).

enclBlockMember(_tree, _tree) :-
    tree(_tree, _Parent, _),
    blockT(_Parent, _, _, _).
enclBlockMember(_tree, _BlockMember) :-
    tree(_tree, _Parent, _),
    tree(_Parent, _, _Factname),
    not(equals(_Factname  , blockT)),
    not(equals(_Factname, methodDefT)),
    not(equals(_Factname, classDefT)),
    not(equals(_Factname, packageT)),
    enclBlockMember(_Parent, _BlockMember).

enclBlock(_tree, _tree) :- tree(_tree, _p, blockT).
enclBlock(_tree, _Block) :- enclBlockMember(_tree, _BlockMember), tree(_BlockMember, _Block, _).



/*
enclBlock(_parent, _Block) :- FIRST(enclBlockRec(_parent, _Block)).
enclBlockRec(_parent, _Block) :- tree(_parent, _, blockT), equals(_Block, _parent)  .
enclBlockRec(_parent, _Block) :- tree(_parent, _parentOfParent, _Factname), not(equals(_Factname, blockT)), enclBlockRec(_parentOfParent, _Block).
*/

/*
enclMethod(_tree, _tree) :-
    tree(_tree, _, methodDefT).
enclMethod(_tree, _Method) :-
    tree(_tree, _parent, _Factname),
    not(equals(_Factname, methodDefT)),
    not(equals(_Factname, classDefT)),
    not(equals(_Factname, packageT)),
    enclMethod(_parent, _Method).
*/


count(_pred) :-
    findall(_pred, call(_pred), _list),
    length(_list, _len),
    writef("found %d results.\n",[_len]),
    fail.
count_bag(_pred) :-
    bagof(_pred, call(_pred), _list),
    length(_list, _len),
    writef("found %d results.\n",[_len]),
    fail.


/*
  getReceiver(identT | selectT, _Receiver)

  Für Field Zugriffe:
        im Param2 wird öParam1 zurückgegeben
  Für Methoden:
  Param1 type
        identT
            gibt im Param2 'null' als zurück
        selectT
            gibt im Param2 den selected tree zurück

*/

/*
getReceiver(_ident, _ident) :-  identT(_ident, _, _, _, _vardef),
                                varDefT(_vardef, _, _, _, _,_),!.
getReceiver(_select, _select) :- selectT(_select, _, _, _, _, _vardef),
                                varDefT(_vardef, _, _, _, _,_),!.
*/

getReceiver(_ident, 'null') :- identT(_ident, _, _, _, _),!.
getReceiver(_select, _Receiver) :- selectT(_select, _, _, _, _Receiver, _).


/*
  getReceiverType(identT | selectT, _Type)

  liefert im Param2 den receiver type((class | basic tpyename), classID, ArrayCount)

  Param1 type
        identT
            gibt im Param2 den type den umschließenden Klasse zurück
        selectT
            gibt im Param2 den type des selected trees zurück
*/


% Uwe todo : kann die ersten beiden regeln nicht nachfolziehen. Der receiver bleibt unabhängig vom referenzierten element !?
/*getReceiverType(_ident, _Type) :-
    identT(_ident, _, _, _, _vardef),
    varDefT(_vardef, _, _, _Type, _,_),
    !.
getReceiverType(_select, _Type) :-
    selectT(_select, _, _, _, _selected, _vardef),
    varDefT(_vardef, _, _, _Type, _,_),
    !.
*/
getReceiverType(_ident, _Type) :-
    identT(_ident, _, _, _, _),
    !,
    enclClass(_ident, _enclClass),
    equals(_Type, type(class, _enclClass, 0)).
getReceiverType(_select, _Type) :-
    selectT(_select, _, _, _, _selected, _),
    getSymbol(_selected, _varMethDef),
    getType(_varMethDef, _Type).

/*
    getType(varDefT | methodDefT, _Type)

    gibt im Param2 den type type((class | basic tpyename), classID, ArrayCount)
    der *Def zurück

    Param1 type
        identT
            gibt im Param2 den type den umschließenden Klasse zurück
        selectT
            gibt im Param2 den return type zurück
*/
getType(_varDef, _Type) :-
    varDefT(_varDef, _,_, _Type, _,_).
getType(_methodDef, _Type) :-
    methodDefT(_methodDef,_, _,_, _Type, _,_).

/*
    Returns null if the id refers to an this ident,
    else it returns id
*/
nullIfThis(_this, 'null') :-
    identT(_this, _, _, 'this', _),!.
nullIfThis(_id, _id).


:- dynamic test_unify/0.
test_unify(_a, _b) :-
    retractall(test_unify),
    _a = _b,
    assertz(test_unify),
    fail.
test_unify(_a, _b) :-
    test_unify.


comma_member(_member, _member) :- _member \= ','(_,_).
comma_member(_member, ','(_member,_)).
comma_member(_member, ','(_h,_t)) :- comma_member(_member, _t).

comma2list(_member, [_member]) :-
    _member \= ','(_,_),
    !.
comma2list(_member, [_member|_T]) :-
    nonvar(_T),
    _T = [],
    !.
comma2list(','(_member,_t), [_member|_T]) :-
    comma2list(_t, _T),
    !.
comma2list(_, []).


% reverse order of comma expression
comma_reverse(_c, _Crev) :-
    comma_reverse(_c, null, _Crev).
comma_reverse(','(_h, _t), null, _T) :-
    !,
    comma_reverse(_t, _h, _T).
comma_reverse(','(_h, _t), _c, _T) :-
    !,
    comma_reverse(_t, ','(_h,_c), _T).
comma_reverse(_h, null, _h) :- !.
comma_reverse(_h, _c, ','(_h,_c)) :- !.


%comma_map(_func, ','(_h, _t), ','(_fh, _t)) :-


% zweites argument sollte kürzere liste sein -> bessere performanz
comma_append(_b, _a, _T) :-
    comma_reverse(_a, _ar),
    comma_prepend_(_ar, _b, _T),
    !.

comma_prepend_(','(_h, _t), _comma, _T) :-
    !,
    comma_prepend_(_t, ','(_h,(_comma)), _T).
comma_prepend_(_h, _comma, ','(_h,(_comma))) :- !.


comma_length(_c, _L) :-
    comma_length(_c, 1, _L).
comma_length(','(_h,_t), _l, _L) :-
    !,
    _l2 is _l + 1,
    comma_length(_t, _l2, _L).
comma_length(_h, _L, _L) :- !.

    

semicolon_member(_member, _member) :- _member \= ';'(_,_).
semicolon_member(_member, ';'(_member,_)).
semicolon_member(_member, ';'(_h,_t)) :- semicolon_member(_member, _t).

semicolon2list(_member, [_member]) :-
    _member \= ';'(_,_),
    !.
semicolon2list(_member, [_member|_T]) :-
    nonvar(_T),
    _T = [],
    !.
semicolon2list(';'(_member,_t), [_member|_T]) :-
    semicolon2list(_t, _T),
    !.
semicolon2list(_, []).



list_to_set_save(_l, _S) :-
    list_to_set_save(_l, [], _sr),
    reverse(_sr, _S).

list_to_set_save(_h,_S, _S) :-
    _h == [],
    !.
list_to_set_save([_h|_t], _s, _S) :-
    member_save(_h, _s),
    !,
    list_to_set_save(_t, _s, _S).
list_to_set_save([_h|_t], _s, _S) :-
    !,
    list_to_set_save(_t, [_h|_s], _S).



% only unifies, non-shared variables
% shared vairiables must be asserted beforehands as term
%test('member_save#1') :- retractall(sharedVar(_)), member_save(class(_A,_,_),[class(_A,_B,_C)]).
%test does work this way, because first occurence of _X is named '_L???' and second occurence is named '_G???'. on commandline it works
%test('member_save#2') :- retractall(sharedVar(_)), term_to_atom(_X, _t), assert(sharedVar(_t)), !, not(member_save(class(_X,_,_),[class(_A,_B,_C)])).


member_save(_m,[])      :- fail.
member_save(_m,[_h|_t]) :-
%     unify_non_shared(1, _m, _h),
     _m == _h.
member_save(_m,[_h|_t]) :-
%     unify_non_shared(1, _m, _h),
     member_save(_m, _t).

/**
  @form getRealParent(_id,_Parent)
  @constraints
    @ground _id
    @unrestricted _Parent)
  @descr Gibt "realen" parent des durch _id bestimmten
         trees zurück. <br>
         Wenn der tree durch eine Forwarding Methode gekapselt wurde
         so wird der ursprüngliche Parent zurückgegeben, sonst der
         im tree angegebene "low level" parent.
*/

getRealParent(_id, _Parent) :-
    forwards(_forw,_,_,_id),
    !,
    getRealParent(_forw, _Parent).

getRealParent(_id,_Parent) :-
    tree(_id,_Parent,_).

/**
  @form getRealMethod(_meth,_Encl)
  @constraints
    @ground _meth
    @unrestricted _Encl)
  @descr Gibt die "reale" Methode zurück.
         Wenn die durch _meth bestimmte Methode eine Forwarding Methode  ist
         so wird die Methode zurückgegeben in der sie aufgerufen wird.
         getRealMethod geht hierbei rekursiv vor.
         Bestimmt _meth keine Methode, so wird eine Exception geworfen:
         'getRealEnclMethod: first Argument must be a method id'
*/

getRealMethod(_forw, _Encl) :-
    forwards(_apply,_forw,_,_),
    !,
    applyT(_apply,_,_encl,_,_),
    getRealMethod(_encl, _Encl).

getRealMethod(_meth, _meth) :-
    methodDefT(_meth,_,_,_,_,_,_),
    !.
    
getRealMethod(_meth, _encl) :-
    throw('getRealEnclMethod: first Argument must be a method id').
