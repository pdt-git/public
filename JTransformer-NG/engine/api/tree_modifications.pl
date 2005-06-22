/*
 * This module provides predicates for 
 * the traversal of the PEFs and general 
 * queries on the tree structure.
 * May be splitted in the future (traversal/general, or s.th.).
 */


/*
  remove_from_class(+Class,+Member)
  
  Removed a member from the class, if Member is
  an element of the class.
  Fails if Class or Member is not bound and if Class is not a
  of type classDefT.
*/

remove_from_class(_class, _id) :- not(tree(_class, _, classDefT)).
remove_from_class(_class, _id) :-
    classDefT(_class, _p,_n,_members),
    findall(_m, (member(_m, _members), _m \= _id ), _newMembers),
    delete(classDefT(_class, _p,_n,_members)),
    add(classDefT(_class, _p,_n,_newMembers)).


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



/**
 replaceId(+Term, +oldId, +newId)
 
 Replaces the
*/

replaceId(_id, _oldId, _newId) :-
    getTerm(_id, _oldTerm),
    replaceIdInTerm(_oldTerm, _newTerm, _oldId, _newId),
    delete(_oldTerm),
    add(_newTerm).


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

/**
 * createIdentRefParam(+Param,+Parent, -Ident)
 */
createIdentRefParam(_param,_parent, _Ident) :-
    paramDefT(_param, _encl, _, _name),
    new_id(_Ident),
    add(identT(_Ident, _parent, _encl, _name, _param)).

createThisIdent(_Ident,_parent, _encl, _class) :-
    new_id(_Ident),
%    debugme,
    add(identT(_Ident, _parent, _encl, 'this', _class)).


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
    delete(toplevelT(_tl, _p,_n,_members)),
    append(_members, [_id], _newMembers),
    add(toplevelT(_tl, _p, _n, _newMembers)).


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
    delete(blockT(_block, _p, _e, _members)),
    append(_members, [_id], _newMembers),
    add(blockT(_block, _p, _e, _newMembers)).

removeFromMethodArgs(_method, _id) :- not(methodDefT(_method, _, _, _, _, _, _)), !.
removeFromMethodArgs(_method, _id) :-
    methodDefT(_method, _p, _n, _members, _r, _e, _b),
    findall(_m, (member(_m, _members), _m \= _id ), _newMembers),
    delete(methodDefT(_method, _p, _n, _members, _r, _e, _b)),
    add(methodDefT(_method, _p, _n, _newMembers, _r, _e, _b)).

addToMethodArgs(_method, _id) :- not(methodDefT(_method, _, _, _, _, _, _)), !.
addToMethodArgs(_method, _id) :-
    methodDefT(_method, _p, _n, _members, _r, _e, _b),
    member(_id, _members),
    !.
addToMethodArgs(_method, _id) :-
    methodDefT(_method, _p, _n, _members, _r, _e, _b),
    delete(methodDefT(_method, _p, _n, _members, _r, _e, _b)),
    append(_members, [_id], _newMembers),
    add(methodDefT(_method, _p, _n, _newMembers, _r, _e, _b)).

    
       
/** 
 * removeTags(DeletionKind, ID)
    Deletes rekursively all sub trees of a tree, or of a list of trees.
    Exception ar the targets of break and continue.

    Removes  (if they exist) the following 'tags' of a node:
	slT - the  Sourcelocation
	modifierT - all modifiers,
	extendsT - the reference to a super class (for classDefT),
	implementsT - all references to implemented interfaces (for classDefT),
	externT - set, if the class is extern
	interfaceT - exists if the PEF reprecents an interface 
*/

removeTags(DeletionKind, ID):-
    removeTagKind(DeletionKind, slT(ID,_start,_length)),
    removeTagKind(DeletionKind, modifierT(ID,_mod)),
    removeTagKind(DeletionKind, implementsT(ID,_iface)),
    removeTagKind(DeletionKind, extendsT(ID,_super)),
    removeTagKind(DeletionKind, externT(ID)),
    removeTagKind(DeletionKind, projectLocationT(ID,_,_)),
    removeTagKind(DeletionKind, sourceLocation(ID,_,_,_)),
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
deepRetract([Tree | _]) :-
    not(tree(Tree,_,_)),
    sformat(S,'deepRetract: tree not found: ~w',[Tree]),
    throw(S).
    
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
    
    

/**
 * discard_permanently(Toplevel)
 *
 * removes the toplevel permanently.
 * Also the contained types are removed form
 * the symtable (globalIds/2).
 *
 * Be sure you know what you are doing!
 */
 
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


/**
  * createReturnOrExec(+Parent, +Encl,+Type,+Expr,+ReturnOrExecStmt)
  * 
  * Creates a new execT/4 fact (with id ReturnOrExecStmt) around the expression Expr,
  * if Type is void. Otherwise a returnT/4 fact is generated.
  */

createReturnOrExec(_parent, _encl, type(basic, void, 0), _stat, _exec) :-
    !,
    add(execT(_exec, _parent, _encl, _stat)).

createReturnOrExec(_parent, _encl, _type, _stat, _return) :-
    add(returnT(_return, _parent, _encl, _stat)).

    
deleteToplevelOfClass(Id) :-
    modifierT(Id,public),
    getToplevel(Id, Tl),
    !,
    findall(Import,(importT(Import,Tl,ClassPckg),delete(importT(Import,Tl,ClassPckg))),_),
%    findall(Class,(classDefT(Class,Package,Name,List),delete(classDefT(Class,Package,Name,List))),_),
    toplevelT(Tl,TlPackage,Filename, Defs),
    assert(deleted_file(Filename)),
    delete(toplevelT(Tl,TlPackage,Filename, Defs)).

deleteToplevelOfClass(_).   