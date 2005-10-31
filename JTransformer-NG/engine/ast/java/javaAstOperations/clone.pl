:- dynamic cloned/2.
/**
cloneTree(+Id,-NewId)

Creates a deep copy of the tree id and 
binds it to newId.
*/

cloneTree(_id, _newTree) :-
    tree(_id, _parent, _),
    enclosing(_id, _encl),
    cloneTree(_id,_parent,_encl,_newTree).

/**
cloneTree(+Id,-NewId)

Creates a deep copy of the tree id, 
binds it to newId and sets the
new parent to Parent and the Enclosing
tree to Encl.
*/

cloneTree(_id, _parent, _encl, _newTree) :-
    createCloneIDs(_id,_newTree),
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
    add(modifierT(Id, H)),
    addModifier(Id, T).
addModifier(Id, Mod) :-
    !,
    add(modifierT(Id, Mod)).

/**
removeModifier(+Id,+ModList)

Removes the list of modifiers ModList 
from the tree id.
*/
removeModifier(_, []) :- !.
removeModifier(Id, [H | T]) :-
    !,
    delete(modifierT(Id, H)),
    removeModifier(Id,T).
    
removeModifier(Id, Mod) :-
    !,
    delete(modifierT(Id, Mod)).


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

createCloneIDs(_id,_new) :-
    ( var(_new)->
       new_id(_new);true),
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
    add(getFieldT(_new,_parent,_encl,_newSelected,_name,_newSym)).

clone(_id, _parent, _encl, _new) :-
    selectT(_id,_,_,_name,_selected,_sym),!,
    cloned(_id, _new),
    getCloneIfAvail(_sym, _newSym),
    clone(_selected, _new, _encl, _newSelected),
    add(selectT(_new,_parent,_encl,_name,_newSelected,_newSym)).


clone(_id, _parent, _encl, _new) :-
    identT(_id,_,_,_name,_sym),!,
    cloned(_id, _new),
    getCloneIfAvail(_sym, _newSym),
    add(identT(_new,_parent,_encl,_name,_newSym)).

clone(_id, _parent, _encl, _new) :-
    literalT(_id,_,_,_type, _value),!,
    cloned(_id, _new),
    getCloneIfAvail(_type, _newType),
    add(literalT(_new, _parent, _encl, _newType, _value)).

clone(_id, _parent, _encl, _new) :-
    assertT(_id,_,_,_test, _msg),!,
    cloned(_id, _new),
    clone(_test, _new, _encl, _newTest),
    clone(_msg, _new, _encl, _newMsg),
    add(assertT(_new, _parent, _encl, _newTest, _newMsg)).

clone(_id, _parent, _encl, _new) :-
    blockT(_id, _, _, _subtrees),
    !,
    cloned(_id, _new),
    clone(_subtrees, _new, _encl, _newSubtrees),
    add(blockT(_new, _parent, _encl, _newSubtrees)).

clone(_id, _parent, _encl, _new) :-
    methodDefT(_id,_,_name,_params,_ret,_thrown,_body),
    !,
    cloned(_id, _new),
    cloneModifier(_id, _new),
    !,
    clone(_params, _new, _new, _newParams),
    clone(_body, _new, _new, _newBody),
    (add_to_class(_parent, _new);true),
    add(methodDefT(_new, _parent, _name, _newParams, _ret, _thrown, _newBody)).

clone(_id, _parent, _encl, _new) :-
    execT(_id,_,_,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    add(execT(_new,_parent,_encl,_newExpr)).

clone(_id, _parent, _encl, _new) :-
    localDefT(_id,_,_,_type,_name,_init),
    !,
    cloned(_id, _new),
    cloneModifier(_id, _new),
    getCloneIfAvail(_type, _newType),
    clone(_init, _new, _encl, _newInit),
    add(localDefT(_new,_parent,_encl,_newType,_name,_newInit)).

clone(_id, _parent, _encl, _new) :-
    fieldDefT(_id,_,_type,_name,_init),
    !,
    cloned(_id, _new),
    cloneModifier(_id, _new),
    getCloneIfAvail(_type, _newType),
    clone(_init, _new, _id, _newInit),
    (add_to_class(_parent, _new);true),
    add(fieldDefT(_new,_parent,_newType,_name,_newInit)).

clone(_id, _parent, _encl, _new) :-
    paramDefT(_id,_,_type,_name),
    !,
    cloned(_id, _new),
    cloneModifier(_id, _new),
    getCloneIfAvail(_type, _newType),
    add(paramDefT(_new,_parent,_newType,_name)).

clone(_id, _parent, _encl, _new) :-
    doLoopT(_id,_,_,_cond,_body),!,
    cloned(_id, _new),
    clone(_cond, _new, _encl, _newCond),
    clone(_body, _new, _encl, _newBody),
    add(doLoopT(_new,_parent,_encl,_newCond,_newBody)).

clone(_id, _parent, _encl, _new) :-
    whileLoopT(_id,_,_,_cond,_body),!,
    cloned(_id, _new),
    clone(_cond, _new, _encl, _newCond),
    clone(_body, _new, _encl, _newBody),
    add(whileLoopT(_new,_parent,_encl,_newCond,_newBody)).

% body (cond | []) (inits | []) (steps | [])
clone(_id, _parent, _encl, _new) :-
    forLoopT(_id,_,_,_initList,_cond,_stepList,_body),!,
    cloned(_id, _new),
    clone(_initList, _new, _encl, _newInitList),
    clone(_stepList, _new, _encl, _newStepList),
    clone(_cond, _new, _encl, _newCond),
    
    clone(_body, _new, _encl, _newBody),
    add(forLoopT(_new,_parent,_encl,_newInitList,_newCond,_newStepList,_newBody)).

clone(_id, _parent, _encl, _new) :-
    labelT(_id,_,_,_body,_label),!,
    cloned(_id, _new),
    clone(_body, _new, _encl, _newBoy),
    add(labelT(_new,_parent,_encl,_newBody,_label)).

clone(_id, _parent, _encl, _new) :-
    switchT(_id,_,_,_selector,_cases),!,
    cloned(_id, _new),
    clone(_selector, _new, _encl, _newSelector),
    clone(_cases, _new, _encl, _newCases),
    add(switchT(_new,_parent,_encl,_newSelector,_newCases)).

clone(_id, _parent, _encl, _new) :-
    caseT(_id,_,_,_pat),!,
    cloned(_id, _new),
    clone(_pat, _new, _encl, _newPat),
    add(caseT(_new,_parent,_encl,_newPat)).

clone(_id, _parent, _encl, _new) :-
    synchronizedT(_id,_,_,_lock,_body),!,
    cloned(_id, _new),
    clone(_lock, _new, _encl, _newLock),
    clone(_body, _new, _encl, _newBody),
    add(synchronizedT(_new,_parent,_encl,_newLock, _newBody)).

clone(_id, _parent, _encl, _new) :-
    tryT(_id,_,_,_body,_catch,_final),
    !,
    cloned(_id, _new),
    clone(_body, _new, _encl, _newBody),
    clone(_catch, _new, _encl, _newCatch),
    clone(_final, _new, _encl, _newFinal),
    add(tryT(_new,_parent,_encl,_newBody, _newCatch, _newFinal)).

clone(_id, _parent, _encl, _new) :-
    catchT(_id,_,_,_param,_body),!,
    cloned(_id, _new),
    clone(_body, _new, _encl, _newBody),
    clone(_param, _new, _encl, _newParam),
    add(catchT(_new,_parent,_encl, _newParam, _newBody)).

clone(_id, _parent, _encl, _new) :-
    ifT(_id,_,_,_cond,_then,_else),
    !,
    cloned(_id, _new),
    clone(_cond, _new, _encl, _newBody),
    clone(_then, _new, _encl, _newThen),
    clone(_else, _new, _encl, _newElse),
    add(ifT(_new,_parent,_encl,_newBody,_newThen,_newElse)).

clone(_id, _parent, _encl, _new) :-
    conditionalT(_id,_,_,_cond,_then,_else),!,
    cloned(_id, _new),
    clone(_cond, _new, _encl, _newBody),
    clone(_then, _new, _encl, _newThen),
    clone(_else, _new, _encl, _newElse),
    add(conditionalT(_new,_parent,_encl,_newBody,_newThen,_newElse)).


clone(_id, _parent, _encl, _new) :-
    returnT(_id,_,_,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    add(returnT(_new,_parent,_encl,_newExpr)).

clone(_id, _parent, _encl, _new) :-
    breakT(_id,_,_,_label,_target),!,
    cloned(_id, _new),
    getCloneIfAvail(_target, _newTarget),
    add(breakT(_new,_parent,_encl,_label, _newTarget)).

clone(_id, _parent, _encl, _new) :-
    continueT(_id,_,_,_target,_),!,
    cloned(_id, _new),
    getCloneIfAvail(_target,_newTarget),
    add(continueT(_new,_parent,_encl,_label, _newTarget)).

clone(_id, _parent, _encl, _new) :-
    throwT(_id,_,_,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    add(throwT(_new,_parent,_encl,_newExpr)).

clone(_id, _parent, _encl, _new) :-
    applyT(_id,_,_,_recv,_name,_args,_method),!,
    cloned(_id, _new),
    clone(_recv, _new, _encl, _newRecv),
    clone(_args, _new, _encl, _newArgs),
    getCloneIfAvail(_method,_newMethod),
    add(applyT(_new,_parent,_encl,_newRecv,_name,_newArgs,_newMethod)).


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
    add(newClassT(_new,_parent,_encl,_newConstr, _newArgs,_newClazz, _newDef, _newEnclClazz)). %FIXME

clone(_id, _parent, _encl, _new) :-
    newArrayT(_id,_,_,_dims,_elems,_type),
    !,
    cloned(_id, _new),
    getCloneIfAvail(_type, _newType),
    clone(_dims, _new, _encl, _newDims),
    clone(_elems, _new, _encl, _newElems),
    add(newArrayT(_new,_parent,_encl,_newDims,_newElems, _newType)).

clone(_id, _parent, _encl, _new) :-
    assignT(_id,_,_,_lhs,_rhs),!,
    cloned(_id, _new),
    clone(_lhs, _new, _encl, _newLhs),
    clone(_rhs, _new, _encl, _newRhs),
    add(assignT(_new,_parent,_encl,_newLhs,_newRhs)).

clone(_id, _parent, _encl, _new) :-
    assignopT(_id,_,_,_lhs,_opname,_rhs),!,
    cloned(_id, _new),
    clone(_lhs, _new, _encl, _newLhs),
    clone(_rhs, _new, _encl, _newRhs),
    add(assignopT(_new,_parent,_encl,_newLhs,_opname,_newRhs)).

clone(_id, _parent, _encl, _new) :-
    operationT(_id,_,_,_args,_opname,_pos),!,
    cloned(_id, _new),
    clone(_args, _new, _encl, _newArgs),
    add(operationT(_new,_parent,_encl,_newArgs,_opname,_pos)).

clone(_id, _parent, _encl, _new) :-
    typeCastT(_id,_,_,_clazz,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    getCloneIfAvail(_clazz, _newClazz),
    add(typeCastT(_new,_parent,_encl,_newClazz, _newExpr)).

clone(_id, _parent, _encl, _new) :-
    typeTestT(_id,_,_,_,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    getCloneIfAvail(_clazz, _newClazz),
    add(typeTestT(_new,_parent,_encl,_newClazz, _newExpr)).

clone(_id, _parent, _encl, _new) :-
    indexedT(_id,_,_,_index, _indexed),!,
    cloned(_id, _new),
    clone(_index, _new, _encl, _newIndex),
    clone(_indexed, _new, _encl, _newIndexed),
    add(indexedT(_new,_parent,_encl,_newIndex, _newIndexed)).


clone(_id, _parent, _encl, _new) :-
    importT(_id,_,_name),!,
    cloned(_id, _new),
    add(importT(_new,_parent,_name)).

clone(_id, _parent, _encl, _new) :-
    packageT(_id,_name),
    !,
    findall(_sub, toplevelT(_sub, _id, _,_), _subtrees),
    cloned(_id, _new),
    add(package(_new, _name)),
    clone(_subtrees, _new, _new, _toplevels).

clone(_id, _parent, _encl, _new) :-
    toplevelT(_id,_,_filename,_subtrees),
    cloned(_id, _new),
    clone(_subtrees, _new, _encl, _newSubtrees),
    add(toplevelT(_id, _parent, _filename, _newSubtrees)).

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
    add(classDefT(_new, _parent, _name, NewSubtrees)),
    add_to_class(_new, NewSubtrees).    

clone(_id, _parent, _encl, _new) :-
    nopT(_id,_,_),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _new),
    add(nopT(_new,_parent,_encl)).

clone(_id, _parent, _encl, _new) :-
    precedenceT(_id,_,_,_expr),!,
    cloned(_id, _new),
    clone(_expr, _new, _encl, _newExpr),
    add(precedenceT(_new,_parent,_encl,_newExpr)).

clone(_id, _, _, _) :-
    tree(_id, _p, _name),
    !,
    format('ERROR: clone: ~a, ~a, ~a~n', [_id, _p, _name]).

clone(_id, _, _, 'null') :-
    not(tree(_id, _, _)),!.


cloneParams(_newParent, _oldList, _newList) :-
    recCloneParams(_newParent, _oldList, _newList, 0).
recCloneParams(_, [], [], _).
recCloneParams(_newParent, [_varDef | _varDefs], [_Copy | _Copies], _counter) :-
    paramDefT(_varDef, _, _retType, _),
    new_id(_Copy),
    append_num('x', _counter, _name),
    add(paramDefT(_Copy, _newParent,_retType, _name)),
    plus(_counter, 1, _next),
    recCloneParams(_newParent, _varDefs, _Copies, _next).
