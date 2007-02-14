/** 
	deleteTree(+#pef)
	retract #pef and protcolls this for rollback.
	Uses delete/1 to retract the tree.
	
	expand to: 
	deleteTree(_id):-getFieldT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(getFieldT(_id,_pid,_encl,_v1,_v2,_v3)).
	...
	deleteTree(_id) :-
    not(tree(_id, _,_)),
    format('could not retract id: ~w~n', [_id]), !.
*/

deleteTree(ID):-
    getTerm(ID,Term),
    delete(Term).

    
/*
    Löscht eine tree definiert durch seine id.
    TODO: expand to clauses:
    
    retractTree(_id):-getFieldT(_id,_pid,_encl,_v1,_v2,_v3),!,retract(getFieldT(_id,_pid,_encl,_v1,_v2,_v3)).
	...
	retractTree(_id) :-
    not(tree(_id, _,_)),
    format('could not retract id: ~w~n', [_id]), !. 
*/
retractTree(ID):-
    getTerm(ID,Term),
    retract(Term).

/**
 * getTerm(?Id, ?Term)
 *
 * binds Term to the fact represented by the id, e.g.:
 *
 * getTerm(1, classDefT(1,2,'Test',[3,4])
 *
 * TODO: expand to clauses
 */
getTerm(ID,Term):-
    nonvar(Term),
    arg(1, Term,ID),
    nonvar(ID).  
 
getTerm(ID,Term):-
    tree(ID,_,Functor),
    ast_node_def('Java',Functor,List),
    length(List,Len),
    functor(Term,Functor,Len),
    arg(1,Term,ID),
    call(Term).


 /**
  enclosing(?Id, ?EnclId)
  
  Unifies EnclId with the enclosing class
  element (method, constructor, initializer or field)
  of the tree Id.
  
  TODO: expand at startup to clauses 
*/

enclosing(ID,Encl):-
    ast_node_def('Java',Name,List),
    length(List,Len),
    functor(Term,Name,Len),
    arg(1,Term,ID),
	resolve_enclosing(List,Encl,Term),
	call(Term).

resolve_enclosing(
     [ast_arg(id, _, id, _), 
      ast_arg(parent,_,id, _), 
      ast_arg(encl, _,id, _)|_],
     Encl,
     Term):-
     !,
     arg(3,Term,Encl).
resolve_enclosing(
     [ast_arg(id, _, id, _), 
      ast_arg(parent,_,id, _)|_],
     Encl,
     Term):-
     !,
     arg(2,Term,Encl).
resolve_enclosing(_,Encl, Term):-
     !,
     arg(1,Term,Encl).

 
 /*
        set_parent(+Id | +IdList, +Parent)
 
        Sets the parent of the tree Id or the list of 
        trees IdList to Parent.
*/

set_parent([], _).
set_parent([Id| Rest], _parent) :-
    set_parent(Id,_parent),
    set_parent(Rest,_parent).
    
set_parent(ID,NewParent):-
    set_pef_arg_(ID, parent, NewParent).
        
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


set_encl_method(ID,NewEncl):-
    set_pef_arg_(ID, encl, NewEncl).
    
set_pef_arg_(ID, ArgName, NewArg):-
    tree(ID,_,Functor),
    ast_node_def('Java',Functor,List),
	build_arg_terms_(ArgName,_OldArg,NewArg,List,OldArgs,NewArgs),
	OldTerm =.. [Functor|OldArgs],
	NewTerm =.. [Functor|NewArgs],
	arg(1,OldTerm,ID),
	call(OldTerm),
	delete(OldTerm),
	add(NewTerm).

build_arg_terms_(_Kind, _Old, _New,[],[],[]).

build_arg_terms_(Kind, Old, New,[ast_arg(Kind, _, id, _)|RestArgs],[Old|RestOld],[New|RestNew]):-
    !,
    build_arg_terms_(Kind, Old, New,RestArgs,RestOld,RestNew).

build_arg_terms_(Kind,Old, New,[_|RestArgs],[Arg|RestOld],[Arg|RestNew]):-
    !,
    build_arg_terms_(Kind,Old, New,RestArgs,RestOld,RestNew).

/******* Test Cases *************/

setUp(deleteTree):-
    add(applyT(applyT1,2,3,4,5,6,7)).
test(deleteTree):-
    deleteTree(applyT1),
    not(applyT(applyT1,2,3,4,5,6,7)).
tearDown(deleteTree):-
    rollback.

setUp(set_encl_method):-
    add(applyT(apply1,2,3,4,5,6,7)).
test(set_encl_method):-
    set_encl_method(apply1,newEncl),
    applyT(apply1,2,newEncl,4,5,6,7).
tearDown(set_encl_method):-
    rollback.

setUp(set_parent):-
    add(applyT(apply1,2,3,4,5,6,7)).
test(set_parent):-
    set_parent(apply1,newParent),
    applyT(apply1,newParent,3,4,5,6,7).
tearDown(set_parent):-
    rollback.

test(build_arg_terms_):-
    ast_node_def('Java',applyT,List),
    build_arg_terms_(encl, old,new,List,OldArgs,NewArgs),
	OldArgs = [G2865, G2871, old, G2883, G2889, G2895, G2901],
	NewArgs = [G2865, G2871, new, G2883, G2889, G2895, G2901].    

test(getTermT01) :-
    assert(methodDefT(testId,1,2,3,type(1,class, 0),5,6)),
    getTerm(testId, methodDefT(testId,1,2,3,type(1,class, 0),5,6)),
    retract(methodDefT(testId,1,2,3,type(1,class, 0),5,6)).
    
