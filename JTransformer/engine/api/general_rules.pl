/*
	The module jtbase provides general predicates for 
	the traversal of the PEFs and serveral predicates
    that do not depend on the PEFs.
    TODO: move these independend predicates to a separate module.
	

:- module(jtbase,[new_id/1,findId/2,getSymbol/2,getSymbolName/2,getIdentName/2,replaceIdInTerm/4,
				  getSymbol/2,getSymbolName/2,getIdentName/2, getTerm/2,
				  append/4, append/5, prepend/3,insertBefore/4,count/1,
				  sub_trees/2]).
*/

:- multifile test/1.


%:- use_module('../base/measure').

/*
class_source_without_toplevel(ID,Package,FQN,Source):-
	classDefT(ID, PID,_,_),
	(
	  packageT(PID,Package);
	  Package = ''
	),
	not(toplevelT(_, PID, _, MLIST), member(ID, MLIST)), 
	fullQualifiedName(ID,FQN), 
	gen_tree(ID, Source).
*/
/**
 new_id(-Id)

 Binds Id to a unique number.
*/	

new_id(_New) :-
    lastID(_last),
    sum(_last, 1, _New),
    retract(lastID(_last)),
    assert(lastID(_New)).


findId(_root, _id) :-
    sub_trees(_root, _subs),
    !,
    findId(_root, _subs, _id).
findId(_root, _id) :-
    format("Tree node for ~d does not exist!~n", [_root]).


findId(_, [], _).
findId(_root, [_h | _t], _h) :-
    tree(_h, _,_type),
    !,
    format("~s(~d)~n", [_type, _root]),
    findId(_root, _t, _h).

findId(_root, [_h | _t], _h) :-
    writeln(_root),
    !,
    findId(_root, _t, _h).

findId(_root, [_h | _t], _id) :-
    !,
    findId(_h, _id),
    findId(_root, _t, _id).



new_ids_by_list([],[]).
new_ids_by_list([_ | _t],[_id | _T]) :-
    new_id(_id),
    new_ids_by_list(_t,_T).

/**
 new_ids([-id1,-id2,...])
 
 Binds the variables id1, ... to new Ids.
*/
 
new_ids([]).
new_ids([_H | _T]) :-
    new_id(_H),
    new_ids(_T).

/**
	getSymbol(?id, ?symbol)
	
	Binds ?symbol to the resolved class (member)
	of a getFieldT, applyT, identT of selectT.
*/

getSymbol(_id, _Symbol) :- getFieldT(_id, _, _, _, _, _Symbol).
getSymbol(_id, _Symbol) :- applyT(_id, _, _, _, _, _,_Symbol).
getSymbol(_id, _Symbol) :- selectT(_id, _, _, _, _, _Symbol).
getSymbol(_id, _Symbol) :- identT(_id, _, _, _, _Symbol).

/**
	getSymbolName(?id, ?name)
	
	Binds ?name to the name of the program element ?id.
	If the tree ?id has no name argument ?name will be bound
	to 'null'. 
	Following trees have a name attribute:
	
	  * local variable (localDefT/6)
	  * field          (fieldDefT/5)
	  * parameter      (paramDefT/4)
	  * method         (methodDefT/7)
	  * class          (classDefT/4)
	  * package        (packageT/3)
*/
getSymbolName(_id, _name) :-
    localDefT(_id,_,_,_,_name, _),!.
getSymbolName(_id, _name) :-
    fieldDefT(_id,_,_,_name, _),!.
getSymbolName(_id, _name) :-
    methodDefT(_id, _, _name, _,_,_, _),!.
getSymbolName(_id, _name) :-
    classDefT(_id,_, _name, _),!.
getSymbolName(_id, _name) :-
    paramDefT(_id,_,_,_name),!.
getSymbolName(_id, 'null').

getIdentName(_vd, _name) :-
    localDefT(_vd,_,_,_,_name, _),!.
getIdentName(_cl, 'this') :-
    classDefT(_cl,_, _, _),!.
getIdentName(_meth, _name) :-
    methodDefT(_meth, _, _name, _,_,_, _),!.
getIdentName(_vd, _name) :-
    paramDefT(_vd,_,_,_name),!.

beforeInBlock(_elem, _newElem, _EnclBlock, _NewList) :-
    enclBlockMember(_elem, _BlockMember),
    tree(_BlockMember, _EnclBlock, _),
    blockT(_EnclBlock, _BlockParent, _, _List),
    insertBefore(_List, _BlockMember, _newElem, _NewList).


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
enclBlock(_tree, _Block) :-
   enclBlockMember(_tree, _BlockMember), 
   tree(_BlockMember, _Block, _).



getReceiver(_ident, 'null') :- identT(_ident, _, _, _, _),!.
getReceiver(_select, _Receiver) :- selectT(_select, _, _, _, _Receiver, _).
getReceiver(_select, _Receiver) :- getFieldT(_select, _, _, _Receiver, _, _).



/*
getReceiverType(_ident, type(class, _enclClass, 0)) :-
    identT(_ident, _, _, _, _),
    !,
    enclClass(_ident, _enclClass).
getReceiverType(_select, _Type) :-
    selectT(_select, _, _, _, _selected, _),
    getSymbol(_selected, _varMethDef),
    getType(_varMethDef, _Type).

getReceiverType(_select, _Type) :-
    getFieldT(_select, _, _, _, _,Field),
    getType(Field, _Type).

getReceiverType(_select, _Type) :-
    getFieldT(_select, _, _, Recv, _,Field),
    getType(Field, _Type).

getReceiverType(_select, _Type) :-
    applyT(_select, _, _, _selected, _,_,_),
    getSymbol(_selected, MethDef),
    getType(MethDef, _Type).
*/


/*
    getType(localDefT | fieldDefT | paramDefT | methodDefT, _Type)

    gibt im Param2 den type type((class | basic tpyename), classID, ArrayCount)
    der *Def zurück

    Param1 type
        identT
            gibt im Param2 den type den umschließenden Klasse zurück
        selectT
            gibt im Param2 den return type zurück
*/

getType(type(_kind,_class,_dim),type(_kind,_class,_dim)) :-  !.

getType(_varDef, _Type) :-
    localDefT(_varDef, _,_, _Type, _,_).
    
getType(_varDef, _Type) :-
    fieldDefT(_varDef, _, _Type, _,_).

getType(_varDef, _Type) :-
    paramDefT(_varDef, _, _Type, _).

getType(_ident, _Type) :-
    identT(_ident,_,_,_,_ref),
    getType(_ref, _Type).

getType(_select, _Type) :-
    getFieldT(_select,_,_,_,_,_ref),
    getType(_ref, _Type).

getType(Apply, Type) :-
    applyT(Apply,_,_,_,_,_,Meth),
    getType(Meth,Type).

getType(_methodDef, type(class,_class,0)) :-
    methodDefT(_methodDef,_class, '<init>',_, _Type, _,_),
    !.

getType(_methodDef, _Type) :-
    methodDefT(_methodDef,_, _,_, _Type, _,_).

getType(_literal, _Type) :-
    literalT(_literal,_,_,_Type,_).

getType(_class, type(class, _class, 0)) :-
    classDefT(_class,_,_,_).

getType(_class, _type) :-
    newClassT(_class,_,_,_,_,_identSelect,_,_),
    getType(_identSelect,_type).


getType(Op, Type) :-
    operationT(Op,_,_, [Expr|_], _,_),
    !,
    getType(Expr,Type).
    

getType(_select, _Type) :-
    selectT(_select,_,_,_,_,_ref),
    getType(_ref, _Type).

%TODO: weiter Expressions

getType(null, null).

getTypeIfNullEnclClass(null, _stat, type(class,_encl,0)) :-
    enclClass(_stat, _encl).
getTypeIfNullEnclClass(_id, _, _Type) :-
    getType(_id, _Type).

% optimiert für ident | selects
getRefType(_ident, _Type) :-
    identT(_ident,_,_,_,_ref),
    getType(_ref, _Type).

getRefType(_select, _Type) :-
    selectT(_select,_,_,_,_,_ref),
    getType(_ref, _Type).

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
comma_member(_member, ','(_h,_t)) :-
    comma_member(_member, _t).

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

getTypeName(type(class, _typeid, _), _Typename) :-
    class(_typeid, _, _Typename),
    !.
getTypeName(type(basic, _Typename, _), _Typename) :-
    !.
getTypeName(_typeid, _Typename) :-
    class(_typeid, _, _Typename),
    !.

% nach: Bob Morein, A.D.A.-Prolog 1985
uniquequicksort(_list, _List) :-
    quicksort(_list,_List, []).

quicksort([Head|Tail], SortierteListe, X ) :-
        split( Head, Tail, Kleiner, Groesser),
        quicksort(Kleiner, SortierteListe, [Head|Rest]),
        quicksort(Groesser, Rest, X ).
quicksort( [], Liste, Liste ).
        split(_, [], [], []):-!.
        split(Element, [Head|Tail], [Head|Kleiner], Groesser):-
                compare(<,Head,Element), !,
                split(Element, Tail, Kleiner, Groesser).
        split(Element, [Head|Tail], Kleiner, Groesser):-
                compare(=,Head,Element), !,
                split(Element, Tail, Kleiner, Groesser).
        split(Element, [Head|Tail], Kleiner, [Head|Groesser]):-
                split(Element, Tail, Kleiner, Groesser).

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



member_save(_m,[])      :- fail.
member_save(_m,[_h|_t]) :-
%     unify_non_shared(1, _m, _h),
     _m == _h.
member_save(_m,[_h|_t]) :-
%     unify_non_shared(1, _m, _h),
     member_save(_m, _t).

% only unifies, non-shared variables
% shared vairiables must be asserted beforehands as term
%test('member_save#1') :- retractall(sharedVar(_)), member_save(class(_A,_,_),[class(_A,_B,_C)]).
%test does work this way, because first occurence of _X is named '_L???' and second occurence is named '_G???'. on commandline it works
%test('member_save#2') :- retractall(sharedVar(_)), term_to_atom(_X, _t), assert(sharedVar(_t)), !, not(member_save(class(_X,_,_),[class(_A,_B,_C)])).

/**
  getRealParent(_id,_Parent)
  @descr Gibt "realen" parent des durch _id bestimmten
         trees zurück. <br>
         Wenn der tree durch eine Forwarding Methode gekapselt wurde
         so wird der ursprüngliche Parent zurückgegeben, sonst der
         im tree angegebene "low level" parent.
*/

getRealParent(_id, _,_Parent) :-
    forwards(_forwCall,_,_,_id),
    !,
    applyT(_forwCall,_Parent,_,_,_,_,_).

getRealParent(_,_parent,_parent).

/**
  @form getRealEncl(_meth,_Encl)
  @constraints
    @ground _meth
    @unrestricted _Encl)
  @descr Gibt die "real" umschließende Methode zurück.
         Wenn die durch _meth bestimmte Methode eine Forwarding Methode ist
         so wird die Methode zurückgegeben in der sie aufgerufen wird.
         getRealEncl geht hierbei rekursiv vor.
         Bestimmt _meth keine Methode, so wird eine Exception geworfen:
         'getRealEnclMethod: first Argument must be a method id'
*/

getRealEncl(_pc,_,_Encl) :-
    forwards(_forwCall,_,_,_pc),
    !,
    applyT(_forwCall,_,_Encl,_,_,_,_).
%    getRealEncl(_r_encl, _Encl).

getRealEncl(_pc,_encl,_Encl) :-
    forwarding(_encl, _,_stmt),
    forwards(_forwCall, _forwMethod, _type, _stmt),
    applyT(_forwCall,_,_Encl,_,_,_,_).

getRealEncl(_,_encl, _encl):-
    methodDefT(_encl,_,_,_,_,_,_).

getRealArgs(_pc,_args,_Args):-
    forwards(_forwCall,_,_,_pc),
    !,
    applyT(_forwCall,_,_,_,_,[_|[_|_Args]],_).

getRealArgs(_,_Args,_Args).

:- dynamic forwards/4.
setUp('getRealEncl'):-
    assert(methodDefT(methorig,class,methname,[],type(class,whatever,0),[],body)).

test('getRealEncl'):-
    assert_true('enclosing method of a method', getRealEncl(methorig,methorig,methorig)),
    assert(applyT(applyforw,parent,methorig,null,methname,args,methforw)),
    assert(applyT(applyorig,parent,encl2,null,methname,args,methorig)),
    assert(forwards(applyforw,forw,methodCall,applyorig)),
    assert_true('enclosing method of a forwarding method', getRealEncl(forw,methorig)).
    
    
tearDown('getRealEncl'):-
    retract(methodDefT(methorig,class,methname,[],type(class,whatever,0),[],body)),
    retract(applyT(applyforw,parent,methorig,null,methname,args,methforw)),
    retract(applyT(applyorig,parent,encl2,null,methname,args,methorig)),
    retract(forwards(applyforw,forw,methodCall,applyorig)).

%getRealEncl(_meth, _meth):-
%    throw('getRealEnclMethod: first Argument must be a method id').

/**
  getRealStat(Stat,RealStat)
  
  Binds RealStat to the "real" tree at a joint point.
  This is the first called forwarding method or the
  original element if it has not been wrapped by a
  forwarding method.
*/

getRealStat(_stat, _Apply) :-
    forwards(_apply,_forw,_,_stat),
    !,
    getRealStat(_apply, _Apply).
getRealStat(_stat, _stat).

/*
getRealStatRec(_stat, _Apply) :-
    forwards(_apply,_forw,_,_stat),
    !,
    getRealStatRec(_apply, _Apply).

getRealStatRec(_apply, _apply).
*/


/**
  enclMethod(?Id, ?MethodId)
  
  Unifies MethodId with the 
  enclosing method of the tree Id.
*/

enclMethod(_ID, _ID) :-
    methodDefT(_ID, _, _, _, _, _, _),
    !.
enclMethod(_id, _Encl) :-
    enclosing(_id, _encl),
    enclMethod(_encl, _Encl),
    !.
enclMethod(_id, 'null').
    
/**
  enclClass(?Id, ?ClassId)
  
  Unifies ClassId with the 
  enclosing class of the tree Id.
  Fails if Id is a package id.
*/

enclClass(Id, Id) :-
    classDefT(Id, _, _, _),
    !.
enclClass(Id, Id) :-
    packageT(Id, _),
    !,
    fail.
    
enclClass(Id, Encl) :-
    enclosing(Id, EnclRec),
    enclClass(EnclRec, Encl),
    !.
enclClass(_, 'null').

/**
  enclosing(?Id, ?EnclId)
  
  Unifies EnclId with the enclosing class
  element (method, constructor, initializer or field)
  of the tree Id.
  
  TODO: reimplement as a meta-predicate
*/
 
enclosing(_id, _Encl):- fieldDefT(_id, _Encl, _, _, _).
enclosing(_id, _Encl):-identT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-getFieldT(_id, _,_Encl,_,_,_), !.
enclosing(_id, _Encl):-selectT(_id, _,_Encl,_,_,_), !.
enclosing(_id, _Encl):-localDefT(_id, _,_Encl,_,_,_), !.
enclosing(_id, _Encl):-paramDefT(_id,_Encl,_,_), !.
enclosing(_id, _Encl):-applyT(_id, _,_Encl,_,_,_,_), !.
enclosing(_id, _Encl):-methodDefT(_id, _Encl,_,_,_,_,_), !.
enclosing(_id, _Encl):-execT(_id, _,_Encl,_), !.
enclosing(_id, _Encl):-operationT(_id, _,_Encl,_,_,_), !.
enclosing(_id, _Encl):-literalT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-blockT(_id, _,_Encl,_), !.
enclosing(_id, _Encl):-assignT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-conditionalT(_id, _,_Encl,_,_,_), !.
enclosing(_id, _Encl):-returnT(_id, _,_Encl,_), !.
enclosing(_id, _Encl):-ifT(_id, _,_Encl,_,_,_), !.
enclosing(_id, _Encl):-caseT(_id, _,_Encl,_), !.
enclosing(_id, _Encl):-indexedT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-typeCastT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-newClassT(_id, _,_Encl,_,_,_,_,_), !.
enclosing(_id, _Encl):-classDefT(_id, _Encl,_,_), !.
enclosing(_id, _id)  :- packageT(_id, _), !.
enclosing(_id, _Encl):-forLoopT(_id, _,_Encl,_,_,_,_), !.
enclosing(_id, _Encl):-breakT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-importT(_id, _,_Encl), !.
enclosing(_id, _Encl):-whileLoopT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-newArrayT(_id, _,_Encl,_,_,_), !.
enclosing(_id, _Encl):-throwT(_id, _,_Encl,_), !.
enclosing(_id, _Encl):-switchT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-toplevelT(_id, _Encl,_,_), !.
enclosing(_id, _Encl):-assignopT(_id, _,_Encl,_,_,_), !.
enclosing(_id, _Encl):-catchT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-tryT(_id, _,_Encl,_,_,_), !.
enclosing(_id, _Encl):-typeTestT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-doLoopT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-labelT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-continueT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-synchronizedT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-assertT(_id, _,_Encl,_,_), !.
enclosing(_id, _Encl):-precedenceT(_id, _,_Encl,_), !.
enclosing(_id, _Encl):-nopT(_id, _,_Encl), !.

/**
	rec_set_encl_method(+Id, +Encl)

	Set the enclosing element of the tree Id
	and all its sub trees to Encl. 
	The old facts will be retracted and
	new facts with Encl asserted.
	
	INFO: This predicate uses the add/1 and delete/1 
	predicates which track all changes to
	the factbase in the rollback functionality.
*/

rec_set_encl_method('null', _).
rec_set_encl_method([], _).
rec_set_encl_method([_H | _T], _encl) :-
    !,
    rec_set_encl_method(_H, _encl),
    rec_set_encl_method(_T, _encl).

rec_set_encl_method(_id, _encl) :-
    classDefT(_id,_,_,_).
    
rec_set_encl_method(_id, _encl) :-
    set_encl_method(_id, _encl),
    sub_trees(_id, _subs),
    rec_set_encl_method(_subs, _encl).

/**
	set_encl_method(+Id, +Encl)

	Set the enclosing element of the tree Id
	to Encl.
	The old fact will be retracted and
	a new fact with Encl asserted.
	
	INFO: This predicate uses the add/1 and delete/1 
	predicates which track all changes to
	the factbase in the rollback functionality.
*/

set_encl_method(_id,_new_encl):-getFieldT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(getFieldT(_id,_pid,_encl,_v1,_v2,_v3)),add(getFieldT(_id,_pid,_new_encl,_v1,_v2,_v3)).
set_encl_method(_id,_new_encl):-selectT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(selectT(_id,_pid,_encl,_v1,_v2,_v3)),add(selectT(_id,_pid,_new_encl,_v1,_v2,_v3)).
set_encl_method(_id,_new_encl):-identT(_id,_pid,_encl,_v1,_v2),!,delete(identT(_id,_pid,_encl,_v1,_v2)),add(identT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-localDefT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(localDefT(_id,_pid,_encl,_v1,_v2,_v3)),add(localDefT(_id,_pid,_new_encl,_v1,_v2,_v3)).
set_encl_method(_id,_new_encl):-paramDefT(_id,_encl,_v1,_v2),!,delete(paramDefT(_id,_encl,_v1,_v2)),add(paramDefT(_id,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-blockT(_id,_pid,_encl,_v1),!,delete(blockT(_id,_pid,_encl,_v1)),add(blockT(_id,_pid,_new_encl,_v1)).
set_encl_method(_id,_new_encl):-doLoopT(_id,_pid,_encl,_v1,_v2),!,delete(doLoopT(_id,_pid,_encl,_v1,_v2)),add(doLoopT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-whileLoopT(_id,_pid,_encl,_v1,_v2),!,delete(whileLoopT(_id,_pid,_encl,_v1,_v2)),add(whileLoopT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4),!,delete(forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4)),add(forLoopT(_id,_pid,_new_encl,_v1,_v2,_v3,_v4)).
set_encl_method(_id,_new_encl):-labelT(_id,_pid,_encl,_v1,_v2),!,delete(labelT(_id,_pid,_encl,_v1,_v2)),add(labelT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-switchT(_id,_pid,_encl,_v1,_v2),!,delete(switchT(_id,_pid,_encl,_v1,_v2)),add(switchT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-caseT(_id,_pid,_encl,_v1),!,delete(caseT(_id,_pid,_encl,_v1)),add(caseT(_id,_pid,_new_encl,_v1)).
set_encl_method(_id,_new_encl):-synchronizedT(_id,_pid,_encl,_v1,_v2),!,delete(synchronizedT(_id,_pid,_encl,_v1,_v2)),add(synchronizedT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-tryT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(tryT(_id,_pid,_encl,_v1,_v2,_v3)),add(tryT(_id,_pid,_new_encl,_v1,_v2,_v3)).
set_encl_method(_id,_new_encl):-catchT(_id,_pid,_encl,_v1,_v2),!,delete(catchT(_id,_pid,_encl,_v1,_v2)),add(catchT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-ifT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(ifT(_id,_pid,_encl,_v1,_v2,_v3)),add(ifT(_id,_pid,_new_encl,_v1,_v2,_v3)).
set_encl_method(_id,_new_encl):-conditionalT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(conditionalT(_id,_pid,_encl,_v1,_v2,_v3)),add(conditionalT(_id,_pid,_new_encl,_v1,_v2,_v3)).
set_encl_method(_id,_new_encl):-execT(_id,_pid,_encl,_v1),!,delete(execT(_id,_pid,_encl,_v1)),add(execT(_id,_pid,_new_encl,_v1)).
set_encl_method(_id,_new_encl):-returnT(_id,_pid,_encl,_v1),!,delete(returnT(_id,_pid,_encl,_v1)),add(returnT(_id,_pid,_new_encl,_v1)).
set_encl_method(_id,_new_encl):-breakT(_id,_pid,_encl,_v1,_v2),!,delete(breakT(_id,_pid,_encl,_v1,_v2)),add(breakT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-continueT(_id,_pid,_encl,_v1,_v2),!,delete(continueT(_id,_pid,_encl,_v1,_v2)),add(continueT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-throwT(_id,_pid,_encl,_v1),!,delete(throwT(_id,_pid,_encl,_v1)),add(throwT(_id,_pid,_new_encl,_v1)).
set_encl_method(_id,_new_encl):-applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4),!,delete(applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4)),add(applyT(_id,_pid,_new_encl,_v1,_v2,_v3,_v4)).
set_encl_method(_id,_new_encl):-newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5),!,delete(newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5)),add(newClassT(_id,_pid,_new_encl,_v1,_v2,_v3,_v4,_v5)).
set_encl_method(_id,_new_encl):-newArrayT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(newArrayT(_id,_pid,_encl,_v1,_v2,_v3)),add(newArrayT(_id,_pid,_new_encl,_v1,_v2,_v3)).
set_encl_method(_id,_new_encl):-assignT(_id,_pid,_encl,_v1,_v2),!,delete(assignT(_id,_pid,_encl,_v1,_v2)),add(assignT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-assignopT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(assignopT(_id,_pid,_encl,_v1,_v2,_v3)),add(assignopT(_id,_pid,_new_encl,_v1,_v2,_v3)).
set_encl_method(_id,_new_encl):-operationT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(operationT(_id,_pid,_encl,_v1,_v2,_v3)),add(operationT(_id,_pid,_new_encl,_v1,_v2,_v3)).
set_encl_method(_id,_new_encl):-typeCastT(_id,_pid,_encl,_v1,_v2),!,delete(typeCastT(_id,_pid,_encl,_v1,_v2)),add(typeCastT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-typeTestT(_id,_pid,_encl,_v1,_v2),!,delete(typeTestT(_id,_pid,_encl,_v1,_v2)),add(typeTestT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-indexedT(_id,_pid,_encl,_v1,_v2),!,delete(indexedT(_id,_pid,_encl,_v1,_v2)),add(indexedT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-literalT(_id,_pid,_encl,_v1,_v2),!,delete(literalT(_id,_pid,_encl,_v1,_v2)),add(literalT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-precedenceT(_id,_pid,_encl,_v1),!,delete(precedenceT(_id,_pid,_encl,_v1)),add(precedenceT(_id,_pid,_new_encl,_v1)).
set_encl_method(_id,_new_encl):-assertT(_id,_pid,_encl,_v1,_v2),!,delete(assertT(_id,_pid,_encl,_v1,_v2)),add(assertT(_id,_pid,_new_encl,_v1,_v2)).
set_encl_method(_id,_new_encl):-nopT(_id,_pid,_encl),!,delete(nopT(_id,_pid,_encl)),add(nopT(_id,_pid,_new_encl)). 

set_encl_method(_id,_):-
    atom_concat('set_encl_method: ', _id, _out),
    throw(_out).


/**
	rec_set_Parent(+Id, +Parent)

	Sets the parent of the tree Id or the list of 
	trees IdList to Parent.
	Then it recursive sets the parents of 
	the subtrees to their parent.
	
	INFO: This predicate uses the add/1 and delete/1 
	predicates which track all changes to
	the factbase in the rollback functionality.
*/

rec_set_parent('null', _).
rec_set_parent([], _).
rec_set_parent([_H | _T], _parent) :-
    !,
    rec_set_parent(_H, _parent),
    rec_set_parent(_T, _parent).
rec_set_parent(_id, _parent) :-
    set_parent(_id, _parent),
    sub_trees(_id, _subs),
    rec_set_parent(_subs, _id).

/*
	set_parent(+Id | +IdList, +Parent)
 
	Sets the parent of the tree Id or the list of 
	trees IdList to Parent.
*/
set_parent([], _).
set_parent([_h| _t], _parent) :-
    set_parent(_h,_parent),
    set_parent(_t,_parent).

set_parent(_id,_newParent):-getFieldT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(getFieldT(_id,_pid,_encl,_v1,_v2,_v3)),add(getFieldT(_id,_newParent,_encl,_v1,_v2,_v3)).
set_parent(_id,_newParent):-selectT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(selectT(_id,_pid,_encl,_v1,_v2,_v3)),add(selectT(_id,_newParent,_encl,_v1,_v2,_v3)).
set_parent(_id,_newParent):-identT(_id,_pid,_encl,_v1,_v2),!,delete(identT(_id,_pid,_encl,_v1,_v2)),add(identT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5),!,delete(methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5)),add(methodDefT(_id,_newParent,_v1,_v2,_v3,_v4,_v5)).
set_parent(_id,_newParent):-localDefT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(localDefT(_id,_pid,_encl,_v1,_v2,_v3)),add(localDefT(_id,_newParent,_encl,_v1,_v2,_v3)).
set_parent(_id,_newParent):-fieldDefT(_id,_pid,_v1,_v2,_v3),!,delete(fieldDefT(_id,_pid,_v1,_v2,_v3)),add(fieldDefT(_id,_newParent,_v1,_v2,_v3)).
set_parent(_id,_newParent):-paramDefT(_id,_pid,_v1,_v2),!,delete(paramDefT(_id,_pid,_v1,_v2)),add(paramDefT(_id,_newParent,_v1,_v2)).
set_parent(_id,_newParent):-blockT(_id,_pid,_encl,_v1),!,delete(blockT(_id,_pid,_encl,_v1)),add(blockT(_id,_newParent,_encl,_v1)).
set_parent(_id,_newParent):-doLoopT(_id,_pid,_encl,_v1,_v2),!,delete(doLoopT(_id,_pid,_encl,_v1,_v2)),add(doLoopT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-whileLoopT(_id,_pid,_encl,_v1,_v2),!,delete(whileLoopT(_id,_pid,_encl,_v1,_v2)),add(whileLoopT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4),!,delete(forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4)),add(forLoopT(_id,_newParent,_encl,_v1,_v2,_v3,_v4)).
set_parent(_id,_newParent):-labelT(_id,_pid,_encl,_v1,_v2),!,delete(labelT(_id,_pid,_encl,_v1,_v2)),add(labelT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-switchT(_id,_pid,_encl,_v1,_v2),!,delete(switchT(_id,_pid,_encl,_v1,_v2)),add(switchT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-caseT(_id,_pid,_encl,_v1),!,delete(caseT(_id,_pid,_encl,_v1)),add(caseT(_id,_newParent,_encl,_v1)).
set_parent(_id,_newParent):-synchronizedT(_id,_pid,_encl,_v1,_v2),!,delete(synchronizedT(_id,_pid,_encl,_v1,_v2)),add(synchronizedT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-tryT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(tryT(_id,_pid,_encl,_v1,_v2,_v3)),add(tryT(_id,_newParent,_encl,_v1,_v2,_v3)).
set_parent(_id,_newParent):-catchT(_id,_pid,_encl,_v1,_v2),!,delete(catchT(_id,_pid,_encl,_v1,_v2)),add(catchT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-ifT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(ifT(_id,_pid,_encl,_v1,_v2,_v3)),add(ifT(_id,_newParent,_encl,_v1,_v2,_v3)).
set_parent(_id,_newParent):-conditionalT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(conditionalT(_id,_pid,_encl,_v1,_v2,_v3)),add(conditionalT(_id,_newParent,_encl,_v1,_v2,_v3)).
set_parent(_id,_newParent):-execT(_id,_pid,_encl,_v1),!,delete(execT(_id,_pid,_encl,_v1)),add(execT(_id,_newParent,_encl,_v1)).
set_parent(_id,_newParent):-returnT(_id,_pid,_encl,_v1),!,delete(returnT(_id,_pid,_encl,_v1)),add(returnT(_id,_newParent,_encl,_v1)).
set_parent(_id,_newParent):-breakT(_id,_pid,_encl,_v1,_v2),!,delete(breakT(_id,_pid,_encl,_v1,_v2)),add(breakT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-continueT(_id,_pid,_encl,_v1,_v2),!,delete(continueT(_id,_pid,_encl,_v1,_v2)),add(continueT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-throwT(_id,_pid,_encl,_v1),!,delete(throwT(_id,_pid,_encl,_v1)),add(throwT(_id,_newParent,_encl,_v1)).
set_parent(_id,_newParent):-applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4),!,delete(applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4)),add(applyT(_id,_newParent,_encl,_v1,_v2,_v3,_v4)).
set_parent(_id,_newParent):-newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5),!,delete(newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5)),add(newClassT(_id,_newParent,_encl,_v1,_v2,_v3,_v4,_v5)).
set_parent(_id,_newParent):-newArrayT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(newArrayT(_id,_pid,_encl,_v1,_v2,_v3)),add(newArrayT(_id,_newParent,_encl,_v1,_v2,_v3)).
set_parent(_id,_newParent):-assignT(_id,_pid,_encl,_v1,_v2),!,delete(assignT(_id,_pid,_encl,_v1,_v2)),add(assignT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-classDefT(_id,_pid,_v1,_v2),!,delete(classDefT(_id,_pid,_v1,_v2)),add(classDefT(_id,_newParent,_v1,_v2)).
set_parent(_id,_newParent):-toplevelT(_id,_pid,_v1,_v2),!,delete(toplevelT(_id,_pid,_v1,_v2)),add(toplevelT(_id,_newParent,_v1,_v2)).
set_parent(_id,_newParent):-assignopT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(assignopT(_id,_pid,_encl,_v1,_v2,_v3)),add(assignopT(_id,_newParent,_encl,_v1,_v2,_v3)).
set_parent(_id,_newParent):-operationT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(operationT(_id,_pid,_encl,_v1,_v2,_v3)),add(operationT(_id,_newParent,_encl,_v1,_v2,_v3)).
set_parent(_id,_newParent):-typeCastT(_id,_pid,_encl,_v1,_v2),!,delete(typeCastT(_id,_pid,_encl,_v1,_v2)),add(typeCastT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-typeTestT(_id,_pid,_encl,_v1,_v2),!,delete(typeTestT(_id,_pid,_encl,_v1,_v2)),add(typeTestT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-indexedT(_id,_pid,_encl,_v1,_v2),!,delete(indexedT(_id,_pid,_encl,_v1,_v2)),add(indexedT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-literalT(_id,_pid,_encl,_v1,_v2),!,delete(literalT(_id,_pid,_encl,_v1,_v2)),add(literalT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-assertT(_id,_pid,_encl,_v1,_v2),!,delete(assertT(_id,_pid,_encl,_v1,_v2)),add(assertT(_id,_newParent,_encl,_v1,_v2)).
set_parent(_id,_newParent):-importT(_id,_pid,_v1),!,delete(importT(_id,_pid,_v1)),add(importT(_id,_newParent,_v1)).
set_parent(_id,_newParent):-precedenceT(_id,_pid,_encl,_v1),!,delete(precedenceT(_id,_pid,_encl,_v1)),add(precedenceT(_id,_newParent,_encl,_v1)).
set_parent(_id,_newParent):-nopT(_id,_pid,_encl),!,delete(nopT(_id,_pid,_encl)),add(nopT(_id,_newParent,_encl)). 

set_parent([],_).
set_parent([_H | _T],_newParent) :-
    set_parent(_H, _newParent),
    set_parent(_T, _newParent).

% eigene definitionen, die NICHT mit cond(...) markiert sind und deswegen
% auch NICHT expandiert werden sollen.
% ansonsten wurde replace(X) expandiert zu
%retract2(X) :- retract(X).
%assert2(X) :- assert(X).


/*
	Part of the action/1 definition.
*/
action(replace(selectT(_id,_pid,_encl,_v1,_v2,_v3))):-        retract(selectT(_id,_,_,_,_,_)), !,        assert(selectT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(identT(_id,_pid,_encl,_v1,_v2))):-             retract(identT(_id,_,_,_,_)), !,           assert(identT(_id,_pid,_encl,_v1,_v2)).
action(replace(methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5))):-   retract(methodDefT(_id,_,_,_,_,_,_)), !,   assert(methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5)).
action(replace(localDefT(_id,_pid,_encl,_v1,_v2,_v3))):-      retract(localDefT(_id,_,_,_,_,_)), !,        assert(localDefT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(fieldDefT(_id,_pid,_v1,_v2,_v3))):-        	retract(fieldDefT(_id,_,_,_,_)), !,        assert(fieldDefT(_id,_pid,_v1,_v2,_v3)).
action(replace(paramDefT(_id,_pid,_v1,_v2))):-        		retract(paramDefT(_id,_,_,_)), !,        assert(paramDefT(_id,_pid,_v1,_v2)).
action(replace(classDefT(_id,_pid,_v1,_v2))):-                retract(classDefT(_id,_,_,_)), !,          assert(classDefT(_id,_pid,_v1,_v2)).
action(replace(toplevelT(_id,_pid,_v1,_v2))):-                retract(toplevelT(_id,_,_,_)), !,          assert(toplevelT(_id,_pid,_v1,_v2)).
action(replace(blockT(_id,_pid,_encl,_v1))):-                 retract(blockT(_id,_,_,_)), !,             assert(blockT(_id,_pid,_encl,_v1)).
action(replace(doLoopT(_id,_pid,_encl,_v1,_v2))):-            retract(doLoopT(_id,_,_,_,_)), !,          assert(doLoopT(_id,_pid,_encl,_v1,_v2)).
action(replace(whileLoopT(_id,_pid,_encl,_v1,_v2))):-         retract(whileLoopT(_id,_,_,_,_)), !,       assert(whileLoopT(_id,_pid,_encl,_v1,_v2)).
action(replace(forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4))):-   retract(forLoopT(_id,_,_,_,_,_,_)), !,     assert(forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4)).
action(replace(labelT(_id,_pid,_encl,_v1,_v2))):-          retract(labelT(_id,_,_,_,_)), !,        assert(labelT(_id,_pid,_encl,_v1,_v2)).
action(replace(switchT(_id,_pid,_encl,_v1,_v2))):-            retract(switchT(_id,_,_,_,_)), !,          assert(switchT(_id,_pid,_encl,_v1,_v2)).
action(replace(caseT(_id,_pid,_encl,_v1))):-              retract(caseT(_id,_,_,_)), !,            assert(caseT(_id,_pid,_encl,_v1)).
action(replace(synchronizedT(_id,_pid,_encl,_v1,_v2))):-      retract(synchronizedT(_id,_,_,_,_)), !,    assert(synchronizedT(_id,_pid,_encl,_v1,_v2)).
action(replace(tryT(_id,_pid,_encl,_v1,_v2,_v3))):-           retract(tryT(_id,_,_,_,_,_)), !,           assert(tryT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(catchT(_id,_pid,_encl,_v1,_v2))):-             retract(catchT(_id,_,_,_,_)), !,           assert(catchT(_id,_pid,_encl,_v1,_v2)).
action(replace(ifT(_id,_pid,_encl,_v1,_v2,_v3))):-            retract(ifT(_id,_,_,_,_,_)), !,            assert(ifT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(conditionalT(_id,_pid,_encl,_v1,_v2,_v3))):-   retract(conditionalT(_id,_,_,_,_,_)), !,   assert(conditionalT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(execT(_id,_pid,_encl,_v1))):-                  retract(execT(_id,_,_,_)), !,              assert(execT(_id,_pid,_encl,_v1)).
action(replace(returnT(_id,_pid,_encl,_v1))):-                retract(returnT(_id,_,_,_)), !,            assert(returnT(_id,_pid,_encl,_v1)).
action(replace(breakT(_id,_pid,_encl,_v1,_v2))):-             retract(breakT(_id,_,_,_,_)), !,           assert(breakT(_id,_pid,_encl,_v1,_v2)).
action(replace(continueT(_id,_pid,_encl,_v1,_v2))):-          retract(continueT(_id,_,_,_,_)), !,        assert(continueT(_id,_pid,_encl,_v1,_v2)).
action(replace(throwT(_id,_pid,_encl,_v1))):-                 retract(throwT(_id,_,_,_)), !,             assert(throwT(_id,_pid,_encl,_v1)).
action(replace(applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4))):-             retract(applyT(_id,_,_,_,_,_,_)), !,           assert(applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4)).
action(replace(newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5))):-!, retract(newClassT(_id,_,_,_,_,_,_,_)), !,  assert(newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5)).
action(replace(newArrayT(_id,_pid,_encl,_v1,_v2,_v3))):-  retract(newArrayT(_id,_,_,_,_,_)), !,    assert(newArrayT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(assignT(_id,_pid,_encl,_v1,_v2))):-            retract(assignT(_id,_,_,_,_)), !,          assert(assignT(_id,_pid,_encl,_v1,_v2)).
action(replace(assignopT(_id,_pid,_encl,_v1,_v2,_v3))):-      retract(assignopT(_id,_,_,_,_,_)), !,      assert(assignopT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(operationT(_id,_pid,_encl,_v1,_v2,_v3))):-     retract(operationT(_id,_,_,_,_,_)), !,     assert(operationT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(typeCastT(_id,_pid,_encl,_v1,_v2))):-          retract(typeCastT(_id,_,_,_,_)), !,        assert(typeCastT(_id,_pid,_encl,_v1,_v2)).
action(replace(typeTestT(_id,_pid,_encl,_v1,_v2))):-          retract(typeTestT(_id,_,_,_,_)), !,        assert(typeTestT(_id,_pid,_encl,_v1,_v2)).
action(replace(indexedT(_id,_pid,_encl,_v1,_v2))):-           retract(indexedT(_id,_,_,_,_)), !,         assert(indexedT(_id,_pid,_encl,_v1,_v2)).
action(replace(literalT(_id,_pid,_encl,_v1,_v2))):-           retract(literalT(_id,_,_,_,_)), !,         assert(literalT(_id,_pid,_encl,_v1,_v2)).
action(replace(assertT(_id,_pid,_encl,_v1,_v2))):-           retract(assertT(_id,_,_,_,_)), !,         assert(assertT(_id,_pid,_encl,_v1,_v2)).
action(replace(importT(_id,_pid,_v1))):-                      retract(importT(_id,_,_)), !,              assert(importT(_id,_pid,_v1)).
action(replace(_tree)) :-
    !,
    arg(1, _tree, _id),
    retractTree(_id),
    assert(_tree).

action(replaceDiffTree(_tree)) :-
    !,
    arg(1, _tree, _id),
  %  format('rdt: ~a ~a', [_id, _tree]),
    retractTree(_id),
    assert(_tree).


:- dynamic cloned/2.
/**
cloneTree(+Id,-NewId)

Creates a deep copy of the tree id and 
binds it to newId.
*/

cloneTree(_id, _newTree) :-
    createCloneIDs(_id),
    tree(_id, _parent, _),
    enclosing(_id, _encl),
    clone(_id, _parent, _encl, _newTree),
    retractall(cloned(_, _)).

getCloneIfAvail(_id, _id) :-
    not(cloned(_id, _)), !.

getCloneIfAvail(_id, _newId) :-
    cloned(_id, _newId),!.

getCloneIfAvail(type(class, _id, _count), type(class, _newId, _count)) :-
    cloned(_id, _newId), !.

getCloneIfAvail(type(_type, _id, _count), type(_type, _id, _count)).

cloneModifier(_id, _new) :-
    findall(_mod, modifierT(_id, _mod), _list),
    addModifier(_new, _list).

cloneExtends(Id,NewId) :-
    extendsT(Id,Ext),
	getCloneIfAvail(Ext,NewExt),
	add(extendsT(NewId,NewExt)),
	!.
	
cloneExtends(_,_).	

cloneImplements(Id,NewId) :-
    findall(Impl,(
      implementsT(Id,Impl),
      getCloneIfAvail(Impl,NewImpl),
	  add(implementsT(NewId,NewImpl))
	 ),_),
	!.	
cloneImplements(_,_).	


/**
addModifier(Id,ModList)

Adds the modifier(s) ModList to
the the tree Id.
Instead of a one-element list a 
just one element may be
given as an argument.
*/

addModifier(_, []) :- !.
addModifier(Id, [H | T]) :-
    !,
    assert(modifierT(Id, H)),
    addModifier(Id, T).
addModifier(Id, Mod) :-
    !,
    assert(modifierT(Id, Mod)).

/**
removeModifier(+Id,+ModList)

Removes the list of modifiers ModList 
from the tree id.
*/
removeModifier(_, []) :- !.
removeModifier(Id, [H | T]) :-
    !,
    retract(modifierT(Id, H)),
    removeModifier(Id,T).
    
removeModifier(Id, Mod) :-
    !,
    retract(modifierT(Id, Mod)).


/**
  createCloneIDs(+id)

Asserts a clone/2 fact for every subtree of
the tree id (including id). 
The first argument is id,
the second one a new unique id.

Auxiliary predicate for cloneTree.
*/
createCloneIDs('null') :- !.
createCloneIDs([_h | _t]) :-
    createCloneIDs(_h),
    createCloneIDs(_t).
createCloneIDs([]).
createCloneIDs(_id) :-
    new_id(_new),
    !,
    assert(cloned(_id,_new)),
    sub_trees(_id, _subs),
    createCloneIDs(_subs).

/**
  clone(+id, +parent,+encl,-newId)

Creates a deep copy of the tree id and 
binds it to newId.

Auxiliary predicate for cloneTree.
*/
clone('null', _, _, 'null').
clone([], _, _, []).

clone([_h | _t], _parent, _encl, [_H | _T]) :-
    clone(_h, _parent, _encl, _H),
    clone(_t, _parent, _encl, _T).

clone(_id, _parent, _encl, _new) :-
    getFieldT(_id,_,_,_selected,_name,_sym),!,
    cloned(_id, _new),
    getCloneIfAvail(_sym, _newSym),
    clone(_selected, _new, _encl, _newSelected),
    assert(getFieldT(_new,_parent,_encl,_newSelected,_name,_newSym)).

clone(_id, _parent, _encl, _new) :-
    selectT(_id,_,_,_name,_selected,_sym),!,
    cloned(_id, _new),
    getCloneIfAvail(_sym, _newSym),
    clone(_selected, _new, _encl, _newSelected),
    assert(selectT(_new,_parent,_encl,_name,_newSelected,_newSym)).


clone(_id, _parent, _encl, _new) :-
    identT(_id,_,_,_name,_sym),!,
    cloned(_id, _new),
    getCloneIfAvail(_sym, _newSym),
    assert(identT(_new,_parent,_encl,_name,_newSym)).

clone(_id, _parent, _encl, _new) :-
    literalT(_id,_,_,_type, _value),!,
    cloned(_id, _new),
    getCloneIfAvail(_type, _newType),
    assert(literalT(_new, _parent, _encl, _newType, _value)).

clone(_id, _parent, _encl, _new) :-
    assertT(_id,_,_,_test, _msg),!,
    cloned(_id, _new),
    clone(_test, _new, _encl, _newTest),
    clone(_msg, _new, _encl, _newMsg),
    assert(assertT(_new, _parent, _encl, _newTest, _newMsg)).

clone(_id, _parent, _encl, _new) :-
    blockT(_id, _, _, _subtrees),
    !,
    cloned(_id, _new),
    clone(_subtrees, _new, _encl, _newSubtrees),
    assert(blockT(_new, _parent, _encl, _newSubtrees)).

clone(_id, _parent, _encl, _new) :-
    methodDefT(_id,_,_name,_params,_ret,_thrown,_body),
    !,
    cloned(_id, _new),
    cloneModifier(_id, _new),
    !,
    clone(_params, _new, _new, _newParams),
    clone(_body, _new, _new, _newBody),
    (add_to_class(_parent, _new);true),
    assert(methodDefT(_new, _parent, _name, _newParams, _ret, _thrown, _newBody)).

clone(_id, _parent, _encl, _new) :-
    execT(_id,_,_,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    assert(execT(_new,_parent,_encl,_newExpr)).

clone(_id, _parent, _encl, _new) :-
    localDefT(_id,_,_,_type,_name,_init),
    !,
    cloned(_id, _new),
    cloneModifier(_id, _new),
    getCloneIfAvail(_type, _newType),
    clone(_init, _new, _encl, _newInit),
    assert(localDefT(_new,_parent,_encl,_newType,_name,_newInit)).

clone(_id, _parent, _encl, _new) :-
    fieldDefT(_id,_,_type,_name,_init),
    !,
    cloned(_id, _new),
    cloneModifier(_id, _new),
    getCloneIfAvail(_type, _newType),
    clone(_init, _new, _id, _newInit),
    (add_to_class(_parent, _new);true),
    assert(fieldDefT(_new,_parent,_newType,_name,_newInit)).

clone(_id, _parent, _encl, _new) :-
    paramDefT(_id,_,_type,_name),
    !,
    cloned(_id, _new),
    cloneModifier(_id, _new),
    getCloneIfAvail(_type, _newType),
    assert(paramDefT(_new,_parent,_newType,_name)).

clone(_id, _parent, _encl, _new) :-
    doLoopT(_id,_,_,_cond,_body),!,
    cloned(_id, _new),
    clone(_cond, _new, _encl, _newCond),
    clone(_body, _new, _encl, _newBody),
    assert(doLoopT(_new,_parent,_encl,_newCond,_newBody)).

clone(_id, _parent, _encl, _new) :-
    whileLoopT(_id,_,_,_cond,_body),!,
    cloned(_id, _new),
    clone(_cond, _new, _encl, _newCond),
    clone(_body, _new, _encl, _newBody),
    assert(whileLoopT(_new,_parent,_encl,_newCond,_newBody)).

% body (cond | []) (inits | []) (steps | [])
clone(_id, _parent, _encl, _new) :-
    forLoopT(_id,_,_,_initList,_cond,_stepList,_body),!,
    cloned(_id, _new),
    clone(_initList, _new, _encl, _newInitList),
    clone(_stepList, _new, _encl, _newStepList),
    clone(_cond, _new, _encl, _newCond),
    
    clone(_body, _new, _encl, _newBody),
    assert(forLoopT(_new,_parent,_encl,_newInitList,_newCond,_newStepList,_newBody)).

clone(_id, _parent, _encl, _new) :-
    labelT(_id,_,_,_body,_label),!,
    cloned(_id, _new),
    clone(_body, _new, _encl, _newBoy),
    assert(labelT(_new,_parent,_encl,_newBody,_label)).

clone(_id, _parent, _encl, _new) :-
    switchT(_id,_,_,_selector,_cases),!,
    cloned(_id, _new),
    clone(_selector, _new, _encl, _newSelector),
    clone(_cases, _new, _encl, _newCases),
    assert(switchT(_new,_parent,_encl,_newSelector,_newCases)).

clone(_id, _parent, _encl, _new) :-
    caseT(_id,_,_,_pat),!,
    cloned(_id, _new),
    clone(_pat, _new, _encl, _newPat),
    assert(caseT(_new,_parent,_encl,_newPat)).

clone(_id, _parent, _encl, _new) :-
    synchronizedT(_id,_,_,_lock,_body),!,
    cloned(_id, _new),
    clone(_lock, _new, _encl, _newLock),
    clone(_body, _new, _encl, _newBody),
    assert(synchronizedT(_new,_parent,_encl,_newLock, _newBody)).

clone(_id, _parent, _encl, _new) :-
    tryT(_id,_,_,_body,_catch,_final),
    !,
    cloned(_id, _new),
    clone(_body, _new, _encl, _newBody),
    clone(_catch, _new, _encl, _newCatch),
    clone(_final, _new, _encl, _newFinal),
    assert(tryT(_new,_parent,_encl,_newBody, _newCatch, _newFinal)).

clone(_id, _parent, _encl, _new) :-
    catchT(_id,_,_,_param,_body),!,
    cloned(_id, _new),
    clone(_body, _new, _encl, _newBody),
    clone(_param, _new, _encl, _newParam),
    assert(catchT(_new,_parent,_encl, _newParam, _newBody)).

clone(_id, _parent, _encl, _new) :-
    ifT(_id,_,_,_cond,_then,_else),
    !,
    cloned(_id, _new),
    clone(_cond, _new, _encl, _newBody),
    clone(_then, _new, _encl, _newThen),
    clone(_else, _new, _encl, _newElse),
    assert(ifT(_new,_parent,_encl,_newBody,_newThen,_newElse)).

clone(_id, _parent, _encl, _new) :-
    conditionalT(_id,_,_,_cond,_then,_else),!,
    cloned(_id, _new),
    clone(_cond, _new, _encl, _newBody),
    clone(_then, _new, _encl, _newThen),
    clone(_else, _new, _encl, _newElse),
    assert(conditionalT(_new,_parent,_encl,_newBody,_newThen,_newElse)).


clone(_id, _parent, _encl, _new) :-
    returnT(_id,_,_,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    assert(returnT(_new,_parent,_encl,_newExpr)).

clone(_id, _parent, _encl, _new) :-
    breakT(_id,_,_,_label,_target),!,
    cloned(_id, _new),
    getCloneIfAvail(_target, _newTarget),
    assert(breakT(_new,_parent,_encl,_label, _newTarget)).

clone(_id, _parent, _encl, _new) :-
    continueT(_id,_,_,_target,_),!,
    cloned(_id, _new),
    getCloneIfAvail(_target,_newTarget),
    assert(continueT(_new,_parent,_encl,_label, _newTarget)).

clone(_id, _parent, _encl, _new) :-
    throwT(_id,_,_,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    assert(throwT(_new,_parent,_encl,_newExpr)).

clone(_id, _parent, _encl, _new) :-
    applyT(_id,_,_,_recv,_name,_args,_method),!,
    cloned(_id, _new),
    clone(_recv, _new, _encl, _newRecv),
    clone(_args, _new, _encl, _newArgs),
    getCloneIfAvail(_method,_newMethod),
    assert(applyT(_new,_parent,_encl,_newRecv,_name,_newArgs,_newMethod)).


clone(_id, _parent, _encl, _new) :-
    newClassT(_id,_,_,_constr,_args,_clazz,_def,_enclClazz),
    !,
    cloned(_id, _new),
    clone(_args, _new, _encl, _newArgs),
%    getCloneIfAvail(_clazz, _newClazz),
    getCloneIfAvail(_constr,_newConstr),
    clone(_def, _new, _encl, _newDef),
    clone(_clazz, _new, _encl, _newClazz),    
    clone(_enclClazz, _new, _encl, _newEnclClazz),
%    getClonedIfAvail(_enclClazz, _newEnclClazz),
    assert(newClassT(_new,_parent,_encl,_newConstr, _newArgs,_newClazz, _newDef, _newEnclClazz)). %FIXME

clone(_id, _parent, _encl, _new) :-
    newArrayT(_id,_,_,_dims,_elems,_type),
    !,
    cloned(_id, _new),
    getCloneIfAvail(_type, _newType),
    clone(_dims, _new, _encl, _newDims),
    clone(_elems, _new, _encl, _newElems),
    assert(newArrayT(_new,_parent,_encl,_newDims,_newElems, _newType)).

clone(_id, _parent, _encl, _new) :-
    assignT(_id,_,_,_lhs,_rhs),!,
    cloned(_id, _new),
    clone(_lhs, _new, _encl, _newLhs),
    clone(_rhs, _new, _encl, _newRhs),
    assert(assignT(_new,_parent,_encl,_newLhs,_newRhs)).

clone(_id, _parent, _encl, _new) :-
    assignopT(_id,_,_,_lhs,_opname,_rhs),!,
    cloned(_id, _new),
    clone(_lhs, _new, _encl, _newLhs),
    clone(_rhs, _new, _encl, _newRhs),
    assert(assignopT(_new,_parent,_encl,_newLhs,_opname,_newRhs)).

clone(_id, _parent, _encl, _new) :-
    operationT(_id,_,_,_args,_opname,_pos),!,
    cloned(_id, _new),
    clone(_args, _new, _encl, _newArgs),
    assert(operationT(_new,_parent,_encl,_newArgs,_opname,_pos)).

clone(_id, _parent, _encl, _new) :-
    typeCastT(_id,_,_,_clazz,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    getCloneIfAvail(_clazz, _newClazz),
    assert(typeCastT(_new,_parent,_encl,_newClazz, _newExpr)).

clone(_id, _parent, _encl, _new) :-
    typeTestT(_id,_,_,_,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    getCloneIfAvail(_clazz, _newClazz),
    assert(typeTestT(_new,_parent,_encl,_newClazz, _newExpr)).

clone(_id, _parent, _encl, _new) :-
    indexedT(_id,_,_,_index, _indexed),!,
    cloned(_id, _new),
    clone(_index, _new, _encl, _newIndex),
    clone(_indexed, _new, _encl, _newIndexed),
    assert(indexedT(_new,_parent,_encl,_newIndex, _newIndexed)).


clone(_id, _parent, _encl, _new) :-
    importT(_id,_,_name),!,
    cloned(_id, _new),
    assert(importT(_new,_parent,_name)).

clone(_id, _parent, _encl, _new) :-
    packageT(_id,_name),
    !,
    findall(_sub, toplevelT(_sub, _id, _,_), _subtrees),
    cloned(_id, _new),
    assert(package(_new, _name)),
    clone(_subtrees, _new, _new, _toplevels).

clone(_id, _parent, _encl, _new) :-
    toplevelT(_id,_,_filename,_subtrees),
    cloned(_id, _new),
    clone(_subtrees, _new, _encl, _newSubtrees),
    assert(toplevelT(_id, _parent, _filename, _newSubtrees)).

clone(_id, _parent, _encl, _new) :-
    classDefT(_id,Package,_name,Subtrees),!,
    cloned(_id, _new),
    cloneModifier(_id, _new),
    cloneExtends(_id,_new),
    cloneImplements(_id,_new),
    !,
	(
		add_to_class(_parent, _new)      % member class
		;
		true
	),(	
    	toplevelT(Toplevel,Package,_,Members), % toplevel class
    	member(_id, Members),
    	addToToplevel(Toplevel, _new) 
    	;
    	true
    ), 
    clone(Subtrees, _new, _new, NewSubtrees),
    assert(classDefT(_new, _parent, _name, NewSubtrees)),
    add_to_class(_new, NewSubtrees).    

clone(_id, _parent, _encl, _new) :-
    nopT(_id,_,_),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _new),
    assert(nopT(_new,_parent,_encl)).

clone(_id, _parent, _encl, _new) :-
    precedenceT(_id,_,_,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    assert(precedenceT(_new,_parent,_encl,_newExpr)).

clone(_id, _, _, _) :-
    tree(_id, _p, _name),
    !,
    format('ERROR: clone: ~a, ~a, ~a~n', [_id, _p, _name]).

clone(_id, _, _, 'null') :-
    not(tree(_id, _, _)),!.

/**
 replaceId(+Term, +oldId, +newId)
 
 Replaces the
*/

replaceId(_id, _oldId, _newId) :-
    getTerm(_id, _oldTerm),
    replaceIdInTerm(_oldTerm, _newTerm, _oldId, _newId),
    retract(_oldTerm),
    assert(_newTerm).

getTerm(_id,packageT(_id,_name)) :-         				  packageT(_id,_name).
getTerm(_id,getFieldT(_id,_pid,_encl,_v1,_v2,_v3)) :-         getFieldT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,selectT(_id,_pid,_encl,_v1,_v2,_v3)) :-           selectT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,identT(_id,_pid,_encl,_v1,_v2)) :-                identT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5)) :-      methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5).
getTerm(_id,localDefT(_id,_pid,_encl,_v1,_v2,_v3)) :-         localDefT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,fieldDefT(_id,_pid,_v1,_v2,_v3)) :-         	  fieldDefT(_id,_pid,_v1,_v2,_v3).
getTerm(_id,paramDefT(_id,_pid,_v1,_v2)) :-                   paramDefT(_id,_pid,_v1,_v2).
getTerm(_id,classDefT(_id,_pid,_v1,_v2)) :-                   classDefT(_id,_pid,_v1,_v2).
getTerm(_id,toplevelT(_id,_pid,_v1,_v2)) :-                   toplevelT(_id,_pid,_v1,_v2).
getTerm(_id,blockT(_id,_pid,_encl,_v1)) :-                    blockT(_id,_pid,_encl,_v1).
getTerm(_id,doLoopT(_id,_pid,_encl,_v1,_v2)) :-               doLoopT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,whileLoopT(_id,_pid,_encl,_v1,_v2)) :-            whileLoopT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4)) :-      forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4).
getTerm(_id,labelT(_id,_pid,_encl,_v1,_v2)) :-             labelT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,switchT(_id,_pid,_encl,_v1,_v2)) :-               switchT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,caseT(_id,_pid,_encl,_v1)) :-                     caseT(_id,_pid,_encl,_v1).
getTerm(_id,synchronizedT(_id,_pid,_encl,_v1,_v2)) :-         synchronizedT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,tryT(_id,_pid,_encl,_v1,_v2,_v3)) :-              tryT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,catchT(_id,_pid,_encl,_v1,_v2)) :-                catchT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,ifT(_id,_pid,_encl,_v1,_v2,_v3)) :-               ifT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,conditionalT(_id,_pid,_encl,_v1,_v2,_v3)) :-      conditionalT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,execT(_id,_pid,_encl,_v1)) :-                     execT(_id,_pid,_encl,_v1).
getTerm(_id,returnT(_id,_pid,_encl,_v1)) :-                   returnT(_id,_pid,_encl,_v1).
getTerm(_id,breakT(_id,_pid,_encl,_v1,_v2)) :-                breakT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,continueT(_id,_pid,_encl,_v1,_v2)) :-             continueT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,throwT(_id,_pid,_encl,_v1)) :-                    throwT(_id,_pid,_encl,_v1).
getTerm(_id,applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4)) :-        applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4).
getTerm(_id,newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5)) :- newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5).
getTerm(_id,newArrayT(_id,_pid,_encl,_v1,_v2,_v3)) :-         newArrayT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,assignT(_id,_pid,_encl,_v1,_v2)) :-               assignT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,assignopT(_id,_pid,_encl,_v1,_v2,_v3)) :-         assignopT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,operationT(_id,_pid,_encl,_v1,_v2,_v3)) :-        operationT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,typeCastT(_id,_pid,_encl,_v1,_v2)) :-             typeCastT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,typeTestT(_id,_pid,_encl,_v1,_v2)) :-             typeTestT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,indexedT(_id,_pid,_encl,_v1,_v2)) :-              indexedT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,literalT(_id,_pid,_encl,_v1,_v2)) :-              literalT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,assertT(_id,_pid,_encl,_v1,_v2)) :-               assertT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,importT(_id,_pid,_v1)) :-                         importT(_id,_pid,_v1).
getTerm(_id,precedenceT(_id,_pid,_encl,_v1)) :-         	  precedenceT(_id,_pid,_encl,_v1).
getTerm(_id,nopT(_id,_pid,_encl)) :-         				  	  nopT(_id,_pid,_encl).

test(getTermT01) :-
    assert(methodDefT(testId,1,2,3,type(1,class, 0),5,6)),
    getTerm(testId, methodDefT(testId,1,2,3,type(1,class, 0),5,6)),
    retract(methodDefT(testId,1,2,3,type(1,class, 0),5,6)).

replaceIdInTerm(_term, _Translated,_oldId, _newId) :-
    _term =.. [_name | _args],
    replaceIdInList(_args, _translatedArgs,_oldId, _newId),
    _Translated =.. [_name | _translatedArgs].

replaceIdInList([], [],_,_) :- !.
replaceIdInList([_oldId | _t], [_newId | _rest], _oldId, _newId) :-
    replaceIdInList(_t, _rest, _oldId, _newId).

replaceIdInList([_term | _t], [_Term | _rest],_oldId, _newId) :-
    not(atomic(_term)),
    !,
    replaceIdInTerm(_term,_Term,_oldId, _newId),
    replaceIdInList(_t, _rest, _oldId, _newId).

replaceIdInList([_term | _t], [_term | _rest],_oldId, _newId) :-
    replaceIdInList(_t, _rest, _oldId, _newId).

test(replaceIdInTermT01) :-
    replaceIdInTerm(methodDefT(1,2,3,4,type(1,a,0), 5,6), methodDefT(new_id,2,3,4,type(new_id,a,0), 5,6), 1, new_id).
    
createVarDefIdents(_, [], []).
createVarDefIdents(_newParent, _oldList, _newList) :-
    reccreateVarDefIdents(_newParent, _oldList, _newList).
reccreateVarDefIdents(_newParent, [], []).
reccreateVarDefIdents(_newParent, [_varDef | _varDefs], [_Ident | _Idents]) :-
    createIdentRefParam(_varDef,_newParent, _Ident),
    reccreateVarDefIdents(_newParent, _varDefs, _Idents).


createIdentRefParam(_param,_parent, _Ident) :-
    paramDefT(_param, _encl, _, _name),
    new_id(_Ident),
    assert(identT(_Ident, _parent, _encl, _name, _param)).

createThisIdent(_Ident,_parent, _encl, _class) :-
    new_id(_Ident),
    assert(identT(_Ident, _parent, _encl, 'this', _class)).

%createIdent(_parent, _encl, _ref, _Ident) :-
%    getIdentName(_ref,_name),
%    new_id(_Ident),
%    assert(identT(_Ident, _parent, _encl, _name, _ref)).

cloneParams(_newParent, _oldList, _newList) :-
    recCloneParams(_newParent, _oldList, _newList, 0).
recCloneParams(_, [], [], _).
recCloneParams(_newParent, [_varDef | _varDefs], [_Copy | _Copies], _counter) :-
    paramDefT(_varDef, _, _retType, _),
    new_id(_Copy),
    appendNum('x', _counter, _name),
    assert(paramDefT(_Copy, _newParent,_retType, _name)),
    plus(_counter, 1, _next),
    recCloneParams(_newParent, _varDefs, _Copies, _next).

appendNum(_str, _num, _Strnum) :-
    int2string(_num, _num_s),
    stringAppend(_str, _num_s, _Strnum).

removeFromClass(_class, _id) :- not(tree(_class, _, classDefT)).
removeFromClass(_class, _id) :-
    classDefT(_class, _p,_n,_members),
    findall(_m, (member(_m, _members), _m \= _id ), _newMembers),
    retractT(classDefT(_class, _p,_n,_members)),
    assert1(classDefT(_class, _p,_n,_newMembers)).

%add_to_class(_class, _) :- not(classDefT(_class, _, _, _)), !.
/*
  add_to_class(+Class,+Member|+MemberList)
  
  Adds Member(s) to the class, if the Member is not already in the 
  member list.
  Fails if Class or Member is not bound and if Class is not a
  of type classDefT.
*/

add_to_class(_, []):- !.
add_to_class(Class, [Member|Rest]) :-
    add_to_class(Class,Member),
    add_to_class(Class,Rest).

add_to_class(_class, _id) :-
    nonvar(_class),
    nonvar(_id),
    classDefT(_class, _, _, _members),
    member(_id, _members),
    !.
add_to_class(_class, _id) :-
    nonvar(_class),
    nonvar(_id),
    classDefT(_class, _p,_n,_members),
    delete(classDefT(_class, _p,_n,_members)),
    append(_members, [_id], _newMembers),
    add(classDefT(_class, _p, _n, _newMembers)).

add_body(_elem, _body):-
    methodDefT(_elem,_parent,_name,_parm,_type,_exc, _),
    action(replace(methodDefT(_elem,_parent,_name,_parm,_type,_exc, _body))).
    
add_body(_elem, _init):-
    localDefT(_elem, _class, _class, _RetType, _name, _),
    action(replace(localDefT(_elem, _class, _class, _RetType, _name, _init))).

add_body(_elem, _init):-
    fieldDefT(_elem, _class, _RetType, _name, _),
    action(replace(fieldDefT(_elem, _class, _RetType, _name, _init))).


addToToplevel(_tl, _id) :- not(toplevelT(_tl, _, _, _)), !.
addToToplevel(_tl, _id) :-
    toplevelT(_tl, _, _, _members),
    member(_id, _members),
    !.
addToToplevel(_tl, _id) :-
    toplevelT(_tl, _p,_n,_members),
    retractT(toplevelT(_tl, _p,_n,_members)),
    append(_members, [_id], _newMembers),
    assert1(toplevelT(_tl, _p, _n, _newMembers)).


removeFromBlock(_block, _id) :- not(blockT(_block, _, _, _)), !.
removeFromBlock(_block, _id) :-
    blockT(_block, _p, _e, _members),
    findall(_m, (member(_m, _members), _m \= _id ), _newMembers),
    delete(blockT(_block, _p, _e, _members)),
    add(blockT(_block, _p, _e, _newMembers)).

addToBlock(_block, []) :- !.
addToBlock(_block, [_H, _T]) :-
    addToBlock(_block, _H),
    !,
    addToBlock(_block, _T).
addToBlock(_block, _id) :- not(blockT(_block, _, _, _)), !.
addToBlock(_block, _id) :-
    blockT(_block, _, _, _members),
    member(_id, _members),
    !.
addToBlock(_block, _id) :-
    blockT(_block, _p, _e, _members),
    retractT(blockT(_block, _p, _e, _members)),
    append(_members, [_id], _newMembers),
    assert1T(blockT(_block, _p, _e, _newMembers)).

removeFromMethodArgs(_method, _id) :- not(methodDefT(_method, _, _, _, _, _, _)), !.
removeFromMethodArgs(_method, _id) :-
    methodDefT(_method, _p, _n, _members, _r, _e, _b),
    findall(_m, (member(_m, _members), _m \= _id ), _newMembers),
    retract(methodDefT(_method, _p, _n, _members, _r, _e, _b)),
    assert1(methodDefT(_method, _p, _n, _newMembers, _r, _e, _b)).

addToMethodArgs(_method, _id) :- not(methodDefT(_method, _, _, _, _, _, _)), !.
addToMethodArgs(_method, _id) :-
    methodDefT(_method, _p, _n, _members, _r, _e, _b),
    member(_id, _members),
    !.
addToMethodArgs(_method, _id) :-
    methodDefT(_method, _p, _n, _members, _r, _e, _b),
    retract(methodDefT(_method, _p, _n, _members, _r, _e, _b)),
    append(_members, [_id], _newMembers),
    assert1(methodDefT(_method, _p, _n, _newMembers, _r, _e, _b)).

getToplevel(Id, _) :-
    not(tree(Id,_,_)),
    !,
    fail.

getToplevel(Id, Tl) :-
    class(Id,Pckg,_),
    toplevelT(Tl, Pckg, _, Classes),
    member(Id, Classes),
    !.

getToplevel(Id, TL) :-
    class(Id,Outer,_),
    class(Outer,_,_),
    !,
    getToplevel(Outer, TL).

getToplevel(Id, TL) :-
    not(class(Id,_,_)),
    enclClass(Id,Class),
    !,
    getToplevel(Class, TL).


can_modify(_id) :-
    enclClass(_id, _class),
    not(externT(_class)).
% TODO  forall(reference(_id, _ref), (enclClass(_ref, _c2), not(externT(_c2)))).

% todo TEUER : effizientere lösung
reference(_id, _Ref) :-
    % look at all facts
    clause(_c, true),
    % that describe tree's
    arg(1, _c, _Ref),
    tree(_Ref, _, _),
    % and reference _id
    arg(_, _c, _id).
    
getPackage(_pckg,_pckg):-
    packageT(_pckg, _),
    !.

getPackage(Class,null):-
    classDefT(Class, null,_,_),
    !.

getPackage(_id,null):-
    enclosing(_id, null).

getPackage(_id,_pckg):-
    enclosing(_id, _encl),
    getPackage(_encl,_pckg).

print_list([]).
print_list([_head|[]]) :-
    !,
    write(_head).

print_list([_head|_tail]) :-
    format('~a, ',_head),
    print_list(_tail).

find_predicates(_namepattern,_list,_length) :-
   findall((_name,_num), (current_predicate(_name,_term), functor(_term,_,_num), pattern(_namepattern,_,_name)),_list),length(_list,_length).

   
/*
    Löscht rekursiv alle Subtrees eines trees, bzw einer Liste von trees.
    Ausnahme sind die "targets" von break und continue.
*/
/*
	Löscht (sofern vorhanden) folgende 'tags' eines gegebenen baumknotens:
	slT - die Sourcelocation
	modifierT - alle modifier,
	extendsT - alle hinweise auf superklassen (für classDefT),
	implementsT - alle hinweise auf implementierte interfaces (für classDefT),
	externT - falls gesetzt, das externT tag.,
	interfaceT -falls gesetzt, das interfaceT tag
*/

removeTags(DeletionKind, ID):-
    removeTagKind(DeletionKind, slT(ID,_start,_length)),
    removeTagKind(DeletionKind, modifierT(ID,_mod)),
    removeTagKind(DeletionKind, implementsT(ID,_iface)),
    removeTagKind(DeletionKind, extendsT(ID,_super)),
    removeTagKind(DeletionKind, externT(ID)),
    removeTagKind(DeletionKind, interfaceT(ID)).
    
removeTagKind(DeletionKind,Tag) :-
    forall(
		Tag,
		(
		   Call =.. [DeletionKind,Tag],
		   call(Call)
		)
	).

deepDelete([]).
deepDelete([_head | _tail]) :-
    sub_trees(_head, _subtrees),
    deepDelete(_subtrees),
    deleteTree(_head),
	removeTags(delete, _head),	
    deepDelete(_tail).

deepDelete(_id) :-
    tree(_id, _,_),
    !,
    deepDelete([_id]).

deepRetract([]).
deepRetract([_head | _tail]) :-
    sub_trees(_head, _subtrees),
    deepRetract(_subtrees),
    retractTree(_head),
	removeTags(retract, _head),	
    deepRetract(_tail).

deepRetract(_id) :-
    tree(_id, _,_),
    !,
    deepRetract([_id]).

listOrEmptyListIfNull(_elem, []) :-
    equals(_elem, 'null').
listOrEmptyListIfNull(_elem, [_elem]) :-
    not(equals(_elem, 'null')).

emptyListIfNull(_elem, []) :-
    equals(_elem, 'null').
emptyListIfNull(_elem, _elem) :-
    not(equals(_elem, 'null')).

/*
    Gibt alle referenzierten Subtrees zurück.
    Ausnahme ist
*/

sub_trees(_id, _subtrees) :-
    blockT(_id,_, _, _subtrees),!.
sub_trees(_id, _subtrees) :-
    packageT(_id,_),
    !,
    findall(_sub,(classDefT(_sub, _id, _, _) ; toplevelT(_sub, _id, _,_)), _subtrees).

sub_trees(_id, [Expr]) :-
    precedenceT(_id,_, _, Expr),!.

sub_trees(_id, _subtrees) :-
    toplevelT(_id,_,_,_subtrees),!.
sub_trees(_id, _subtrees) :-
    classDefT(_id,_,_,_subtrees),!.
%    format('sub_trees:classDef: ~a~a~n', [_id, _name]),
%    classMembersT(_id, _subtrees),!.

sub_trees(_id, _subtrees) :-
    methodDefT(_id,_,_name,_params,_,_,_body),
    !,
%    format('sub_trees:methodDef: ~a~a~n', [_id, _name]),
    listOrEmptyListIfNull(_body, _bodyList),
    append(_bodyList, _params, _subtrees).

sub_trees(_id, _subtrees) :-
    fieldDefT(_id,_,_,_name,_init),
    !,
    listOrEmptyListIfNull(_init, _subtrees).

sub_trees(_id, []) :-
    paramDefT(_id,_,_,_),
    !.

sub_trees(_id, _subtrees) :-
    localDefT(_id,_,_,_,_name,_init),
    !,
    listOrEmptyListIfNull(_init, _subtrees).


sub_trees(_id, [_cond ,_body]) :-
    doLoopT(_id,_,_,_cond,_body),!.

sub_trees(_id, [_cond ,_body]) :-
    whileLoopT(_id,_,_,_cond,_body),!.

% body (cond | []) (inits | []) (steps | [])
sub_trees(_id, [_body | _subtrees]) :-
    forLoopT(_id,_,_,_initList,_cond,_stepList,_body),
    !,
    listOrEmptyListIfNull(_cond, _condList),
    append(_initList, _stepList, _dummyList),
    append(_condList, _dummyList, _subtrees).

sub_trees(_id, [_body]) :-
    labelT(_id,_,_,_body,_),!.

sub_trees(_id, [_selector | _cases]):-
    switchT(_id,_,_,_selector,_cases),!.


sub_trees(_id, List) :-
    caseT(_id,_,_,_pat),
    !,
    listOrEmptyListIfNull(_pat,List).

sub_trees(_id, [_lock, _body]) :-
    synchronizedT(_id,_,_,_lock,_body),!.

sub_trees(_id, [_body | _catchfinal]) :-
    tryT(_id,_,_,_body,_catch,_final),
    !,
    emptyListIfNull(_catch, _catchNonNull),
    emptyListIfNull(_final, _finalNonNull),
    append(_catchNonNull, _finalNonNull, _catchfinal).

sub_trees(_id, [_param, _body]) :-
    catchT(_id,_,_,_param,_body),!.

sub_trees(_id, [_test, _msg]) :-
    assertT(_id,_,_,_test,_msg),!.

sub_trees(_id, [_cond |_thenelse]) :-
    ifT(_id,_,_,_cond,_then,_else),
    !,
    listOrEmptyListIfNull(_else, _elseList),
    append([_then], _elseList, _thenelse).


sub_trees(_id, [_cond,_then,_else]) :-
    conditionalT(_id,_,_,_cond,_then,_else),!.

sub_trees(_id, [_expr]) :-
    execT(_id,_,_,_expr),!.

sub_trees(_id, _list) :-
    returnT(_id,_,_,_expr), !,
    listOrEmptyListIfNull(_expr, _list).


sub_trees(_id, []) :-
       breakT(_id,_,_,_,_),!.

sub_trees(_id, []) :-
    continueT(_id,_,_,_,_),!.

sub_trees(_id, [_expr]) :-
    throwT(_id,_,_,_expr),!.

sub_trees(_id, SubTrees) :-
    applyT(_id,_,_,_recv,_,_args,_),!,
    listOrEmptyListIfNull(_recv,List),
    append(List,_args,SubTrees).

sub_trees(_id, [_clazz | _dummyList]) :-
    newClassT(_id,_,_,_,_args,_clazz,_def,_encl),
    !,
    listOrEmptyListIfNull(_encl, _enclList),
    listOrEmptyListIfNull(_def, _defList),
    append(_enclList, _defList, _args, _dummyList).

sub_trees(_id, _subtrees) :-
    newArrayT(_id,_,_,_dims,_elems,_),
    !,
    emptyListIfNull(_dims, _dimsList),
    emptyListIfNull(_elems, _elemsList),
    append(_dimsList, _elemsList, _subtrees).

sub_trees(_id, [_lhs, _rhs]) :-
    assignT(_id,_,_,_lhs,_rhs),!.

sub_trees(_id, [_lhs, _rhs]) :-
    assignopT(_id,_,_,_lhs,_,_rhs),!.

sub_trees(_id, _args) :-
    operationT(_id,_,_,_args,_,_),!.

sub_trees(_id, [_expr]) :-
    typeCastT(_id,_,_,_,_expr),!.

sub_trees(_id, [_expr]) :-
    typeTestT(_id,_,_,_,_expr),!.

sub_trees(_id, [_index, _indexed]) :-
    indexedT(_id,_,_,_index, _indexed),!.

sub_trees(_id, [_selected]) :-
    selectT(_id,_,_,_,_selected,_),!.

sub_trees(_id, _subtrees) :-
    getFieldT(_id,_,_,Expr,_,_),
    !,
    (Expr == 'null' ->
    	(_subtrees = []);
    	(_subtrees = [Expr])
    ).

sub_trees(_id, []) :-
    importT(_id,_,_),!.
%    format('sub_trees:import: ~a, ~a~n', [_id, _imp]).

sub_trees(_id, []) :-
    literalT(_id,_,_,_,_),!.

sub_trees(_id, []) :-
    identT(_id,_,_,_,_),!.

sub_trees(_id, []) :-
    nopT(_id,_,_),
    !.

sub_trees(_id, []) :-
    tree(_id, _p, _name),
    !,
    format('ERROR: sub_trees: ~a, ~a, ~a~n', [_id, _p, _name]).

sub_trees(_id, 'null') :-
    not(tree(_id, _, _)),!.
% ld:
% contains_type tests wether a classDefT is declared 
% in the respective topLevelT,named file or definition list.
% 
% the test descents to reach  innner classes aswell.
%
% i am using this predicate to collect the fqns to be retracted from
% globalIds when a toplevel is deleted. (not changed!)
contains_type(_tl,_type):-
    toplevelT(_tl,_,_,_defs),
    !,
    contains_type(_defs,_type).

contains_type(_filename,_type):-    
    toplevelT(_,_,_filename,_defs),
    !,
    contains_type(_defs,_type).
    
contains_type(_list,_type):-
    member(_type,_list),
    classDefT(_type,_,_,_).
    
contains_type(_list,_type):-
    member(_node,_list),
    classDefT(_node,_,_,_defs),
    contains_type(_defs,_type).
 
discard_permanently(Id):-
    toplevelT(Id,_,_,_),
    !,
    forall(
    	contains_type(Id,Type),
    	(
    		globalIds(FQN,Type),    	
	    	retractall(globalIds(FQN,Type))
    		%%format('i would retract ~a, but...~n',FQN)
    	)
    ),
    deepDelete(Id).

    
discard_permanently(_fileName):-
    toplevelT(Id,_,_fileName,_),
    !,
    forall(
    	contains_type(Id,Type),
    	(
	    	globalIds(FQN,Type),
    		retractall(globalIds(FQN,Type)) 
    		%%format('i would retract ~a, but...~n',FQN)
    	)
    ),
    deepDelete(Id).
    
   
sourceLocation(Tree,File,Start,End):-
    slT(Tree,Start,End),
    getToplevel(Tree,TL),
    toplevelT(TL,_,File,_).
    
    
    

/********************************************
 ************* PEF independend **************
 ********************************************/
:- dynamic countvar/1.
:- multifile countvar/1.
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
    
/**
	stringAppend(?Atom1, ?Atom2, ?Atom3, ?Atom4)
	
	Atom4 forms the concatination of Atom1, Atom2 and Atom3.
	
	TODO: specify possible binding combinations
*/    
    
stringAppend(_str1, _str2, _str3, _Ret) :-
    stringAppend(_str1, _str2, _tmpName),
    stringAppend(_tmpName, _str3, _Ret).


/**
	stringAppend(?Atom1, ?Atom2, ?Atom3, ?Atom4, ?Atom5)
	
	Atom5 forms the concatination of Atom1, Atom2, Atom3 and Atom4.
	TODO: specify possible binding combinations
*/    

stringAppend(_str1, _str2, _str3, _str4, _Ret) :-
    stringAppend(_str1, _str2, _str3, _tmpName),
    stringAppend(_tmpName, _str4, _Ret).

/**
	append(?List1, ?List2, ?List3, ?List4)
	
	Succeeds  when List4 unifies with  the concatenation of List1, List2 and
    List3. The  predicate can be used with any instantiation pattern
    (even four variables).
*/

append(_first, _second, _third, _Ret) :-
    append(_first, _second, _dummyList),
    append(_dummyList, _third, _Ret).

/**
	append(?List1, ?List2, ?List3, ?List4, ?List5)
	
	Succeeds  when List5 unifies with  the concatenation of List1, List2, List3 and
    List4. The  predicate can be used with any instantiation pattern
    (even five variables).
*/

append(_first, _second, _third, _fourth, _Ret) :-
    append(_first, _second, _third, _dummyList),
    append(_dummyList, _fourth, _Ret).


/*
	prepend(+List, +Elem, -NewList)
*/

prepend(_list, _elem, _return) :-
    append([_elem],_list,_return).

/*
    insertBefore(+List, +TargetMember, +NewMember, -NewList)
    
    Inserts NewMember before TargetMember.
*/

insertBefore([_Head | _Tail], _elem, _newElem, _RetList) :-
    equals(_Head, _elem),
    append([_Head], [_newElem], _Tail, _RetList).

insertBefore([_Head | _Tail], _elem, _newElem, _RetList) :-
    not(equals(_Head, _elem)),
    insertBefore(_Tail, _elem, _newElem, _NewTail),
    append([_Head], _NewTail, _RetList).

insertBefore([], _elem, _newElem, _RetList) :- equals(_RetList, []).

/*
	count(+pred)
	
	Counts all binding for the predicate pred.
*/
	
count(_pred) :-
    findall(_pred, call(_pred), _list),
    length(_list, _len),
    writef("found %d results.\n",[_len]),
    fail.
    
/*
count_bag(_pred) :-
    bagof(_pred, call(_pred), _list),
    length(_list, _len),
    writef("found %d results.\n",[_len]),
    fail.
*/	

/*
	first_char_up(?LowerAtom, ?UpperAtom)
	
	At least one of the arguments must be bound to
    to a atom where the first character is from 
    the domain [a-z,A-Z].
	Binds UpperAtom to an atom which is LowerAtom
	with an uppercase first char, respectively
	Binds LowerAtom to an atom which is UpperAtom
	with an lowercase first char.
	
	e.g.
	?- first_char_up(name, Name).
	 Yes
*/
first_char_up(Name, UName) :-
    nonvar(Name),
  	atom_length(Name, Len),
    Prec is Len - 1,
	sub_atom(Name, 1,Prec,_,Rest),
	atom_concat(FirstChar,Rest,Name),
	upcase_atom(FirstChar,UpperFirstChar),
	atom_concat(UpperFirstChar,Rest,UName),
	!.
	
first_char_up(Name, UName) :-
    nonvar(UName),
   	atom_length(UName, Len),
    Prec is Len - 1,
	sub_atom(UName, 1,Prec,_,Rest),
	atom_concat(FirstChar,Rest,UName),
	downcase_atom(FirstChar,LowerFirstChar),
	atom_concat(LowerFirstChar,Rest,Name),
	!.	
	
/**
	uniqueArgumentList(+Arity,-Arguments)
*/
	
uniqueArgumentList(0,[]):-!.
    
uniqueArgumentList(Arity,[Argument| Arguments]):-
    atom_concat('A', Arity,Atom),
    atom_to_term(Atom,Argument,_),
    plus(Prec, 1, Arity),
    uniqueArgumentList(Prec, Arguments).
    
test(uniqueArgumentList) :-
    uniqueArgumentList(5,[_,_,_,_,_]).