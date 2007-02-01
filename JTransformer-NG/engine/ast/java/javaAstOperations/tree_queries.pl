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
 * getTerm(?Id, ?Term)
 *
 * binds Term to the fact represented by the id, e.g.:
 *
 * getTerm(1, classDefT(1,2,'Test',[3,4])
 */
getTerm(_id,packageT(_id,_name)) :-                           packageT(_id,_name).
getTerm(_id,getFieldT(_id,_pid,_encl,_v1,_v2,_v3)) :-         getFieldT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,selectT(_id,_pid,_encl,_v1,_v2,_v3)) :-           selectT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,identT(_id,_pid,_encl,_v1,_v2)) :-                identT(_id,_pid,_encl,_v1,_v2).
getTerm(_id,methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5)) :-      methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5).
getTerm(_id,localDefT(_id,_pid,_encl,_v1,_v2,_v3)) :-         localDefT(_id,_pid,_encl,_v1,_v2,_v3).
getTerm(_id,fieldDefT(_id,_pid,_v1,_v2,_v3)) :-                   fieldDefT(_id,_pid,_v1,_v2,_v3).
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
getTerm(_id,precedenceT(_id,_pid,_encl,_v1)) :-                   precedenceT(_id,_pid,_encl,_v1).
getTerm(_id,nopT(_id,_pid,_encl)) :-                                              nopT(_id,_pid,_encl).

test(getTermT01) :-
    assert(methodDefT(testId,1,2,3,type(1,class, 0),5,6)),
    getTerm(testId, methodDefT(testId,1,2,3,type(1,class, 0),5,6)),
    retract(methodDefT(testId,1,2,3,type(1,class, 0),5,6)).

    
/**
 * getToplevel(+Tree, ?Toplevel)
 *
 * binds Toplevel to the enclosing 
 * toplevelT (file) of Tree.
 */
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
    class(Id,Parent,_),
    enclClass(Parent,Outer),
    !,
    getToplevel(Outer, TL).

getToplevel(Id, Id) :-
    toplevelT(Id,_,_,_).

getToplevel(Id, TL) :-
    not(class(Id,_,_)),
    enclClass(Id,Class),
    !,
    getToplevel(Class, TL).
    

/**
 * can_modify(+Id)
 *
 * checks if Id is a pef defined in a
 * source class. 
 * The enclosing class is not marked with externT/1.
 */
can_modify(_id) :-
    enclClass(_id, _class),
    not(externT(_class)).
% TODO  forall(reference(_id, _ref), (enclClass(_ref, _c2), not(externT(_c2)))).

% todo TEUER : effizientere loesung
reference(_id, _Ref) :-
    % look at all facts
    clause(_c, true),
    % that describe tree's
    arg(1, _c, _Ref),
    tree(_Ref, _, _),
    % and reference _id
    arg(_, _c, _id).
    
    
/**
 * getPackage(+Tree,?Package)
 *
 * binds Package to the enclosing 
 * package id of Tree.
 */
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


/**
 * tree_attributes(ID, Functor, [AttributeVal1,...])
 *
 * e.g. for the class id 10001 (java.lang.Object):
 * 
 * tree_attributes(10001,  classDefT, ['Object']).
 */
    
tree_attributes(ID, Functor, Attributes) :-
    tree(ID,_, Functor),
    getTerm(ID,Term),
    Term =.. [Functor|Args],
    ast_node_def('Java', Functor,AttributeDescr),
    extract_attributes_(AttributeDescr,Args, Attributes).

extract_attributes_(_,[],[]).
extract_attributes_([ast_arg(_,_, attr, _)|Descrs],[Arg|Args],[Arg|Attribs]) :-
    !,
	extract_attributes_(Descrs,Args,Attribs).
         
extract_attributes_([_|Descrs],[_|Args],Attribs) :-
    !,
	extract_attributes_(Descrs,Args,Attribs).
    
test(tree_attributes) :-
    fullQualifiedName(ID,'java.lang.Object'),
    tree_attributes(ID, Kind, Attributes),
    assert_true(Kind = classDefT),
    assert_true(Attributes = ['Object']).

test(tree_attributes2) :-
    fullQualifiedName(ID,'java.lang.Object'),
    tree_attributes(MID,methodDefT,[wait, type(basic, void, 0)]),
    methodDefT(MID,ID,_,_,_,_,_).
    
    
/**
 * sub_trees(_id, _subtrees)
 * Binds all sub trees of the id to Subtrees
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
%    format('sub_trees:classDef: ~w~w~n', [_id, _name]),
%    classMembersT(_id, _subtrees),!.

sub_trees(_id, _subtrees) :-
    methodDefT(_id,_,_name,_params,_,_,_body),
    !,
%    format('sub_trees:methodDef: ~w~w~n', [_id, _name]),
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
%    format('sub_trees:import: ~w, ~w~n', [_id, _imp]).

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
    format('ERROR: sub_trees: ~w, ~w, ~w~n', [_id, _p, _name]).

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

/**    
 * findId(+Root, ?Id)
 *
 * looks up the tree Id
 * in the sub tree of Root.
 */ 
findId(_root, _id) :-
    sub_trees(_root, _subs),
    !,
    findId(_root, _subs, _id).
findId(_root, _id) :-
    format("Tree node for ~d does not exist!~n", [_root]).

/**    
 * findId(+Root, +[Subtree1,...], ?Id)
 *
 * looks up the tree Id
 * in the sub tree of Root.
 *
 * private predicate (TODO: will be hidden in a module)
 */ 
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

/**
 * new_ids_by_list(+List,-IdList)
 *
 * Creates a list of Ids of the same lenght as List. 
 */

new_ids_by_list([],[]).
new_ids_by_list([_ | _t],[_id | _T]) :-
    new_id(_id),
    new_ids_by_list(_t,_T).

/**
 * new_ids([-id1,-id2,...])
 * 
 * Binds the variables id1, ... to new Ids.
*/
 
new_ids([]).
new_ids([_H | _T]) :-
    new_id(_H),
    new_ids(_T).

/**
 * getSymbol(?id, ?symbol)
 *       
 * Binds ?symbol to the resolved class (member)
 * of a getFieldT, applyT, identT of selectT.
*/

getSymbol(_id, _Symbol) :- getFieldT(_id, _, _, _, _, _Symbol).
getSymbol(_id, _Symbol) :- applyT(_id, _, _, _, _, _,_Symbol).
getSymbol(_id, _Symbol) :- selectT(_id, _, _, _, _, _Symbol).
getSymbol(_id, _Symbol) :- identT(_id, _, _, _, _Symbol).

/**
 *  getSymbolName(?id, ?name)
 *        
 *  Binds ?name to the name of the program element ?id.
 *  If the tree ?id has no name argument ?name will be bound
 *  to 'null'. 
 *  Following trees have a name attribute:
 *
 *  local variable (localDefT/6)
 *  field          (fieldDefT/5)
 *  parameter      (paramDefT/4)
 *  method         (methodDefT/7)
 *  class          (classDefT/4)
 *  package        (packageT/3)
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


/**
 * beforeInBlock(+Elem, +NewElem, -EnclBlock, -NewList)
 * 
 * insert the pef NewElem into the enclosing block of Elem
 * at the position before Elem. If the parent of Elem is not
 * a block (parent is a nested structure like an expression) 
 * NewElem will be added before the first ancestor pef which 
 * a member of a block.
 * 
 */
beforeInBlock(_elem, _newElem, _EnclBlock, _NewList) :-
    enclBlockMember(_elem, _BlockMember),
    tree(_BlockMember, _EnclBlock, _),
    blockT(_EnclBlock, _BlockParent, _, _List),
    insertBefore(_List, _BlockMember, _newElem, _NewList).


/**
 * enclBlockMember(+Tree, +Anchestor)
 *
 * Ancestor has a block as its parent.
 *
 * private predicate (TODO: will be hidden in a module)
 */
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

/**
 * enclBlock(?Tree, ?Block)
 *
 * Block is the enclosing block of Tree.
 */
enclBlock(_tree, _tree) :- 
	tree(_tree, _p, blockT).
enclBlock(_tree, _Block) :-
   enclBlockMember(_tree, _BlockMember), 
   tree(_BlockMember, _Block, _).


/**
 * getReceiver(?Expr, ?Receiver)
 *
 * Receiver is the expression on which
 * Expr is evaluated. E.g. f is a field access
 * expression on the receiver r in the expression " r.f ".
 * Receiver is null, if the receiver is implicit (this).
 * 
 * Only implemented for identT, selectT, getFieldT and applyT.
 */
getReceiver(_ident, 'null') :-     identT(_ident, _, _, _, _),!.
getReceiver(_select, _Receiver) :- selectT(_select, _, _, _, _Receiver, _).
getReceiver(_select, _Receiver) :- getFieldT(_select, _, _, _Receiver, _, _).
getReceiver(_select, _Receiver) :- setField(_select,_,_,_Receiver,_,_).
getReceiver(_select, _Receiver) :- methodCall(_select, _, _, _Receiver, _, _,_).


/*
    getType(localDefT | fieldDefT | paramDefT | methodDefT, _Type)

    gibt im Param2 den type type((class | basic tpyename), classID, ArrayCount)
    der *Def zur�ck

    Param1 type
        identT
            gibt im Param2 den type den umschlie�enden Klasse zur�ck
        selectT
            gibt im Param2 den return type zur�ck
*/

getType(type(_kind,_class,_dim),type(_kind,_class,_dim)) :-  !.

getType(Var,type(basic,null,0)) :- 
    nonvar(Var),
    Var = null.

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
 
% has setField a type? type(basic,void,0) ?     
getType(_select, _Type) :-
    setField(_select,_,_,_,_ref,_),
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

getType(Array, Type) :-
    newArrayT(Array,_,_,_,_,Type).

%TODO: weiter Expressions
getType(null, type(class,null,0)).

% optimiert fuer ident | selects
/**
 * getRefType(?Tree, ?Type)
 *
 * TODO: to be removed
 */
getRefType(_ident, _Type) :-
    identT(_ident,_,_,_,_ref),
    getType(_ref, _Type).

getRefType(_select, _Type) :-
    selectT(_select,_,_,_,_,_ref),
    getType(_ref, _Type).

/**
  nullIfThis(+Expr, -NullOrExpr)

   Returns null if the id refers to an this ident,
   else it returns id
*/
nullIfThis(_this, 'null') :-
    identT(_this, _, _, 'this', _),!.
nullIfThis(_id, _id).

/*
	packagePath(+Pckgname,-PckgPath)
*/

packagePath(Pckgname,PckgPath) :-
    atom_concat(First,'.',RestName, Pckgname),
    !,
  	packagePath(RestName,RestPath),
    atom_concat(First,'/',Rest1),
    atom_concat(Rest1, RestPath, PckgPath).
  

packagePath(Name,Name).
    
test(packagePath):-
    packagePath('pckg1.pckg2.Name','pckg1/pckg2/Name').

/**
 * types(?ExprOrDeclarationList, ?typeTermList)
 */
	
types([],[]).
types([Expr|Exprs], [Type|Types]):-   
    getType(Expr,Type),
    types(Exprs, Types).


/**
 * subtype(?Sub, ?Super)
 *
 * Binds Super to any direct or 
 * indirect super type of Sub.
 * 
 * Only for object types.
 */
   
subtype(_sub, _sub).
subtype(_sub, _super) :-
    extendsT(_sub,_super).
subtype(_sub, _super) :-
    implementsT(_sub,_super).
subtype(_sub, _super) :-
    extendsT(_sub,_subsuper),
    subtype(_subsuper, _super).
subtype(_sub, _super) :-
    implementsT(_sub,_subsuper),
    subtype(_subsuper, _super).
subtype(Var, _):-
    nonvar(Var),
    Var = null.

/**
 * stringType(StringTypeTerm)
 *
 * Binds StringTypeTerm to the term 
 * referencing java.lang.String.
 */

stringType(type(class, _id, 0)) :-
    stringClass(_id).

/**
 * stringClass(-Id)
 *
 * Binds Id to the java.lang.String
 * class Id.
 */
stringClass(_ID) :-
    packageT(_syspid, 'java.lang'),
    classDefT(_ID, _syspid, 'String', _).               

/**
 * param_names(ParamList,ParamNameList)
 */
param_names([],[]).
param_names([Param|Params],[Name|Names]) :- 
    paramDefT(Param, _, _, Name),
	param_names(Params,Names).

	
/**
 * param_typenames(ParamList,ParamTypeNameList)
 */
param_typenames([],[]).
param_typenames([Param|Params],[Type|Types]) :- 
    java_fq(paramDefT(Param, _, Type, _)),
	param_typenames(Params,Types).
	
	
/**	
 * type_exists(+Type)
 *
 * Type is a type term.
 * This predicate checks if the type exists.
 */
type_exists(type(basic,Type,_)) :-
    basicType(Type).
type_exists(type(class,Type,_)) :-
    classDefT(Type,_,_,_).