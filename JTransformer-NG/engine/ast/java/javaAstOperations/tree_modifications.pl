%TRHO: TODO: make generic:
:- multifile set_parent/2.
:- multifile set_encl_method/2.

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
        the factbase in the rollback facility.
        Use rollback/0 to undo this operation.
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
   rec_set_parent(+Id, +Parent)

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


/**
 * replaceId(+PefId, +OldId, +NewId)
 *
 * Replaces OldId with NewId in the fact 
 * represented by PefId.
 */

replaceId(_id, _oldId, _newId) :-
    getTerm(_id, _oldTerm),
    replaceIdInTerm(_oldTerm, _newTerm, _oldId, _newId),
    delete(_oldTerm),
    add(_newTerm).


/**
 * replaceIdInTerm(+Term, -TranslatedTerm,+OldId, +NewId)
 *
 * Replaces OldId with NewId in Term an binds the resulting
 * Term to TranslatedTerm.
 */
replaceIdInTerm(_term, _Translated,_oldId, _newId) :-
    _term =.. [_name | _args],
    replaceIdInList(_args, _translatedArgs,_oldId, _newId),
    _Translated =.. [_name | _translatedArgs].

/**
 * replaceIdInList(+List, -TranslatedList,+OldMember, +MewMember)
 *
 * Replaces the member OldMember with NewMember in the 
 * list List an binds the resulting List to TranslatedList.
 * Term to TranslatedTerm.
 */
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

/**
 * createThisIdent(-Ident,+Parent, +Encl, +Class)
 *
 * Create new this identT fact with Parent as its parent
 * Encl as its enclosing element and Class as its
 * instance type.
 */
createThisIdent(_Ident,_parent, _encl, _class) :-
    new_id(_Ident),
%    debugme,
    add(identT(_Ident, _parent, _encl, 'this', _class)).


/**
 * add_body(+Method, +Body)
 *
 * replaces the current body of Method with Body
 */
add_body(_elem, _body):-
    methodDefT(_elem,_parent,_name,_parm,_type,_exc, _),
    action(replace(methodDefT(_elem,_parent,_name,_parm,_type,_exc, _body))).
    
add_body(_elem, _init):-
    localDefT(_elem, _class, _class, _RetType, _name, _),
    action(replace(localDefT(_elem, _class, _class, _RetType, _name, _init))).

add_body(_elem, _init):-
    fieldDefT(_elem, _class, _RetType, _name, _),
    action(replace(fieldDefT(_elem, _class, _RetType, _name, _init))).

/**
 * addToToplevel(+Toplevel, +Member)
 * 
 * appends Member to Toplevel.
 * No validity check is performed on Member.
 */

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

/**
 * removeFromBlock(+Block, +Pef)
 *
 * Removes Pef from Block.
 * This operation is logged and will be undone
 * 
 */
removeFromBlock(_block, _id) :- not(blockT(_block, _, _, _)), !.
removeFromBlock(_block, _id) :-
    blockT(_block, _p, _e, _members),
    findall(_m, (member(_m, _members), _m \= _id ), _newMembers),
    delete(blockT(_block, _p, _e, _members)),
    add(blockT(_block, _p, _e, _newMembers)).

/**
 * addToBlock(+Block, +Pef | +[Pef1,..])
 *
 * appends a pef or a pef list to a block.
 */
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

/**
 * removeFromMethodArgs(+Method, +Pef)
 *
 * removes Pef from the parameters of Method.
 */
removeFromMethodArgs(_method, _id) :- not(methodDefT(_method, _, _, _, _, _, _)), !.
removeFromMethodArgs(_method, _id) :-
    methodDefT(_method, _p, _n, _members, _r, _e, _b),
    findall(_m, (member(_m, _members), _m \= _id ), _newMembers),
    delete(methodDefT(_method, _p, _n, _members, _r, _e, _b)),
    add(methodDefT(_method, _p, _n, _newMembers, _r, _e, _b)).

/**
 * addToMethodArgs(+Method, +Pef)
 *
 * adds Pef to the parameters of Method.
 */
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
 * walkTags(Predicate, ID)
    Deletes rekursively all sub trees of a tree, or of a list of trees.
    Exceptions are the targets of break and continue.

    Removes  (if they exist) the following 'tags' of a node:
	slT - the  Sourcelocation
	modifierT - all modifiers,
	extendsT - the reference to a super class (for classDefT),
	implementsT - all references to implemented interfaces (for classDefT),
	externT - set, if the class is extern
	interfaceT - exists if the PEF reprecents an interface 
*/

walkTags(Predicate, ID):-
    forall(
      (
         attribSignature(Functor,Arity),
         functor(Term, Functor, Arity), Term =.. [Functor, ID|_]
      ),
      (
         forall(sub_tree_of_attributes(Functor, ID, Subs), 
                walkTree(Predicate,Subs)),
         walkTagKind(Predicate, Term)
      )
     ).
/*
    walkTagKind(DeletionKind, slT(ID,_start,_length)),
    walkTagKind(DeletionKind, modifierT(ID,_mod)),
    walkTagKind(DeletionKind, implementsT(ID,_iface)),
    walkTagKind(DeletionKind, extendsT(ID,_super)),
    walkTagKind(DeletionKind, externT(ID)),
    walkTagKind(DeletionKind, projectLocationT(ID,_,_)),
    walkTagKind(DeletionKind, sourceLocation(ID,_,_,_)),
    walkTagKind(DeletionKind, interfaceT(ID)).
*/    
walkTagKind(Predicate,Tag) :-
    forall(
                Tag,
                (
                   Call =.. [Predicate,Tag],
                   call(Call)
                )
        ).

/**
 * walkTree(predicate, ID(s))
 */
walkTree(_Kind, []):-
    !.

walkTree(Kind, [Head | Tail]) :-
    !,
    walkTree(Kind,Head),
    walkTree(Kind,Tail).

walkTree(Kind,Tree) :-
    sub_trees(Tree, Subtrees),
    walkTree(Kind,Subtrees),
    getTerm(Tree,Term),
    actionOnTreeReverseIndexes(Kind, Term),
    Call=..[Kind,Term],
    call(Call),
    walkTags(Kind, Tree).    

/**
 * deepDelete(+Pef | +[Pef1,...])
 *
 * deeply deletes Pef or a list of pefs.
 * To remove the element delete/1 is used.
 * Therefore the deletions are logged and
 * will be undone in the next rollback.
 */

deepDelete([]).
deepDelete([_head | _tail]) :-
    sub_trees(_head, _subtrees),
    deepDelete(_subtrees),
    getTerm(_head,Term),
    deleteTreeReverseIndexes(Term),
    deleteTree(_head),
    walkTags(delete, _head),      
    deepDelete(_tail).

deepDelete(_id) :-
    tree(_id, _,_),
    !,
    deepDelete([_id]).

/**
 * deepRetract(+Pef | +[Pef1,...])
 *
 * deeply retracts Pef or a list of pefs.
 * In contrast to deepDelete/1 the pefs
 * are simply retracted from the factbase
 * without logging the operation for future
 * rollback operations.
 */

deepRetract([]).
deepRetract([Tree | _]) :-
    not(tree(Tree,_,_)),
    sformat(S,'deepRetract: tree not found: ~w',[Tree]),
    throw(S).
    
deepRetract([_head | _tail]) :-
    sub_trees(_head, _subtrees),
    deepRetract(_subtrees),
    getTerm(_head,Term),
    retractTreeReverseIndexes(Term),
    retractTree(_head),
        walkTags(retract, _head),     
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
                retractall(globalIds(FQN,Type)),
                retractall(globalIds(Type,FQN))
                %%format('i would retract ~w, but...~n',FQN)
        )
    ),
    deepDelete(Id).

    
discard_permanently(FileName):-
    toplevelT(Id,_,FileName,_),
    !,
    forall(
        contains_type(Id,Type),
        (
                globalIds(FQN,Type),
                retractall(globalIds(FQN,Type)),
                retractall(ri_globalIds(Type,FQN)) 
                %%format('i would retract ~w, but...~n',FQN)
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

/**
 * deleteToplevelOfClass(+Toplevel)
 * 
 * delete the complete toplevel and all imports.
 * The deletion operation is logged and
 * will be undone in the next rollback/0 operation.
 */

deleteToplevelOfClass(Id) :-
    modifierT(Id,public),
    getToplevel(Id, Tl),
    !,
    findall(Import,(importT(Import,Tl,ClassPckg),delete(importT(Import,Tl,ClassPckg))),_),
%    findall(Class,(classDefT(Class,Package,Name,List),delete(classDefT(Class,Package,Name,List))),_),
    toplevelT(Tl,TlPackage,Filename, Defs),
    file_on_disk(Tl,RealFilename),
    assert(deleted_file(RealFilename)),
    delete(toplevelT(Tl,TlPackage,Filename, Defs)).
deleteToplevelOfClass(_).   

/**
 * deepRetractToplevel(+Toplevel)
 * 
 * delete the complete toplevel with all
 * descendant classes and Files.
 * The deletion operation is logged and
 * will be undone in the next rollback/0 operation.
 */
deepRetractToplevel(Toplevel) :-
%    findall(Class,(classDefT(Class,Package,Name,List),delete(classDefT(Class,Package,Name,List))),_),
	assert(deleted_toplevel(Toplevel)),
    toplevelT(Toplevel,_TlPackage,_Filename, _Defs),
    file_on_disk(Toplevel,RealFilename),
	exec_with_delta(assert,deleted_file(RealFilename)),
	exec_with_delta(retractall,modified_toplevel(Toplevel)),
	walkTree(retract_with_delta, Toplevel).
%    deepRetract(Toplevel).
