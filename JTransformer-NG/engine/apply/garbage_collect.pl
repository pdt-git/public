% Author: Tobias
% Date: 29.08.02

% TRHO: TODO: replace this with the generic version.

:- multifile deleteTree/1.
:- multifile retractTree/1.
/*
    garbageCollection
    
    Deletes all trees that are not referenced by their parent node.
    On this trees 'deepDeleteIfParentCorrect' is called, which runs a  deep
    search and deletes every subtree which is correctly referenced by its parent.
*/

garbageCollection :-
    findall(_id, (tree(_id, _parent, _), deepDeleteIfNotReferenced(_id, _parent)), _alltrees).

deepDeleteIfNotReferenced(_id, 'null') :-
    !.
deepDeleteIfNotReferenced(_id, _parent) :-
    not(sub_trees(_parent, _subtrees)),
    !,
    format('gc: deleted ~w, _parent ~w has no subtrees~n', [_id, _parent]),
    deepDeleteIfParentCorrect(_id, _parent).

deepDeleteIfNotReferenced(_id, _parent) :-
    sub_trees(_parent, _subtrees),
    equals(_subtrees, 'null'),
    !,
    tree(_id, _, _t),
    format('gc: deleted ~w, sub_trees of parent ~w is null~n', [_id, _parent]),
    deepDeleteIfParentCorrect(_id, _parent).
    
deepDeleteIfNotReferenced(_id, _parent) :-
    sub_trees(_parent, _subtrees),
    not(equals(_parent, 'null')),
    not(equals(_subtrees, 'null')),
    not(member(_id, _subtrees)),
    !,
    tree(_id, _, _t),
    format('gc: deleted ~w, not a member of the subtrees of parent ~w~n', [_id, _parent]),
    deepDeleteIfParentCorrect(_id, _parent).


/*
 * deleteSubElements
 * 
 * Löscht _num Subelemente von _parent
 * Die Subelemente werden durch den Rückverweis parent in
 * den Subtrees bestimmt.
 * 
 * Diese Clause dient nur zu DEBUG Zwecken!
 * Die Auswahl der Element wird durch Prolog bestimmt.
 *
 */

deleteSubElements(_parent, _type, _num) :-
    counter(0),
    findall(_id, (tree(_id, _parent, _type),
                  incCounter(_c), _c =< _num,
                  deepDelete(_id)),
            _list).

/*
 *  Löscht einen tree und alle seine subtrees wenn der übergebene parent tree
 *  mit dem im tree gesetzten parent übereinstimmt.
 *  Rekursive wird an die Methode immer der aktuelle tree und die liste der subtrees übergeben.
 *  Die Funktion bricht jeweils in den trees ab, in denen die Bedingung nicht erfüllt ist.
 */
deepDeleteIfParentCorrect([], _).
deepDeleteIfParentCorrect([_head | _tail], _parent) :-
    tree(_head, _parent, _),
    !,
    sub_trees(_head, _subtrees),
    deepDeleteIfParentCorrect(_subtrees, _head),
    retractTree(_head),
    deepDeleteIfParentCorrect(_tail, _parent).
deepDeleteIfParentCorrect([_head | _tail], _parent) :-
    tree(_head, _headparent, _),
    not(equals(_headparent, _parent)),
    !.
deepDeleteIfParentCorrect(_id, _parent) :-
    tree(_id, _,_),
    !,
    deepDeleteIfParentCorrect([_id], _parent).
deepDeleteIfParentCorrect([_head | _], _parent) :-
    not(tree(_head, _parent, _)),
    !.

/* 
	deleteTree(+#pef)
	retract #pef and protcolls this for rollback.
	Uses delete/1 to retract the tree.
*/

deleteTree(_id):-getFieldT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(getFieldT(_id,_pid,_encl,_v1,_v2,_v3)).
deleteTree(_id):-selectT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(selectT(_id,_pid,_encl,_v1,_v2,_v3)).
deleteTree(_id):-identT(_id,_pid,_encl,_v1,_v2),!,delete(identT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5),!,delete(methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5)).
deleteTree(_id):-localDefT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(localDefT(_id,_pid,_encl,_v1,_v2,_v3)).
deleteTree(_id):-fieldDefT(_id,_pid,_v1,_v2,_v3),!,delete(fieldDefT(_id,_pid,_v1,_v2,_v3)).
deleteTree(_id):-paramDefT(_id,_pid,_v1,_v2),!,delete(paramDefT(_id,_pid,_v1,_v2)).
deleteTree(_id):-classDefT(_id,_pid,_v1,_v2),!,delete(classDefT(_id,_pid,_v1,_v2)).
deleteTree(_id):-packageT(_id,_v1),!,delete(packageT(_id,_v1)).
deleteTree(_id):-toplevelT(_id,_pid,_v1,_v2),!,delete(toplevelT(_id,_pid,_v1,_v2)).
deleteTree(_id):-blockT(_id,_pid,_encl,_v1),!,delete(blockT(_id,_pid,_encl,_v1)).
deleteTree(_id):-doLoopT(_id,_pid,_encl,_v1,_v2),!,delete(doLoopT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-whileLoopT(_id,_pid,_encl,_v1,_v2),!,delete(whileLoopT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4),!,delete(forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4)).
deleteTree(_id):-labelT(_id,_pid,_encl,_v1,_v2),!,delete(labelT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-switchT(_id,_pid,_encl,_v1,_v2),!,delete(switchT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-caseT(_id,_pid,_encl,_v1),!,delete(caseT(_id,_pid,_encl,_v1)).
deleteTree(_id):-synchronizedT(_id,_pid,_encl,_v1,_v2),!,delete(synchronizedT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-tryT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(tryT(_id,_pid,_encl,_v1,_v2,_v3)).
deleteTree(_id):-catchT(_id,_pid,_encl,_v1,_v2),!,delete(catchT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-ifT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(ifT(_id,_pid,_encl,_v1,_v2,_v3)).
deleteTree(_id):-conditionalT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(conditionalT(_id,_pid,_encl,_v1,_v2,_v3)).
/*format('rt:exec: ~w~n', [_id]),*/
deleteTree(_id):-execT(_id,_pid,_encl,_v1),!,delete(execT(_id,_pid,_encl,_v1)).
deleteTree(_id):-returnT(_id,_pid,_encl,_v1),!,delete(returnT(_id,_pid,_encl,_v1)).
deleteTree(_id):-breakT(_id,_pid,_encl,_v1,_v2),!,delete(breakT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-continueT(_id,_pid,_encl,_v1,_v2),!,delete(continueT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-throwT(_id,_pid,_encl,_v1),!,delete(throwT(_id,_pid,_encl,_v1)).
deleteTree(_id):-applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4),!,delete(applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4)).
deleteTree(_id):-newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5),!,delete(newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5)).
deleteTree(_id):-newArrayT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(newArrayT(_id,_pid,_encl,_v1,_v2,_v3)).
deleteTree(_id):-assignT(_id,_pid,_encl,_v1,_v2),!,delete(assignT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-assignopT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(assignopT(_id,_pid,_encl,_v1,_v2,_v3)).
deleteTree(_id):-operationT(_id,_pid,_encl,_v1,_v2,_v3),!,delete(operationT(_id,_pid,_encl,_v1,_v2,_v3)).
deleteTree(_id):-typeCastT(_id,_pid,_encl,_v1,_v2),!,delete(typeCastT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-typeTestT(_id,_pid,_encl,_v1,_v2),!,delete(typeTestT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-indexedT(_id,_pid,_encl,_v1,_v2),!,delete(indexedT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-literalT(_id,_pid,_encl,_v1,_v2),!,delete(literalT(_id,_pid,_encl,_v1,_v2)).
deleteTree(_id):-assertT(_id,_pid,_encl,_v1,_v2),!,delete(assertT(_id,_pid,_encl,_v1,_v2)).
%deleteTree(_id):-classMembersT(_id,_v1),!,delete(classMembersT(_id,_v1)).
deleteTree(_id):-externT(_id),!,delete(externT(_id)).
deleteTree(_id):-importT(_id,_pid,_v1),!,delete(importT(_id,_pid,_v1)).
deleteTree(_id):-nopT(_id,_pid,_encl),!,delete(nopT(_id,_pid,_encl)).
deleteTree(_id):-precedenceT(_id,_pid,_encl,_v1),!,delete(precedenceT(_id,_pid,_encl,_v1)). 
deleteTree(_id) :-
    not(tree(_id, _,_)),
    format('could not retract id: ~w~n', [_id]), !.

/*
    Löscht eine tree definiert durch seine id.
*/

retractTree(_id):-getFieldT(_id,_pid,_encl,_v1,_v2,_v3),!,retract(getFieldT(_id,_pid,_encl,_v1,_v2,_v3)).
retractTree(_id):-selectT(_id,_pid,_encl,_v1,_v2,_v3),!,retract(selectT(_id,_pid,_encl,_v1,_v2,_v3)).
retractTree(_id):-identT(_id,_pid,_encl,_v1,_v2),!,retract(identT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5),!,retract(methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5)).
retractTree(_id):-localDefT(_id,_pid,_encl,_v1,_v2,_v3),!,retract(localDefT(_id,_pid,_encl,_v1,_v2,_v3)).
retractTree(_id):-fieldDefT(_id,_pid,_v1,_v2,_v3),!,retract(fieldDefT(_id,_pid,_v1,_v2,_v3)).
retractTree(_id):-paramDefT(_id,_pid,_v1,_v2),!,retract(paramDefT(_id,_pid,_v1,_v2)).
retractTree(_id):-classDefT(_id,_pid,_v1,_v2),!,retract(classDefT(_id,_pid,_v1,_v2)).
retractTree(_id):-packageT(_id,_v1),!,retract(packageT(_id,_v1)).
retractTree(_id):-toplevelT(_id,_pid,_v1,_v2),!,retract(toplevelT(_id,_pid,_v1,_v2)).
retractTree(_id):-blockT(_id,_pid,_encl,_v1),!,retract(blockT(_id,_pid,_encl,_v1)).
retractTree(_id):-doLoopT(_id,_pid,_encl,_v1,_v2),!,retract(doLoopT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-whileLoopT(_id,_pid,_encl,_v1,_v2),!,retract(whileLoopT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4),!,retract(forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4)).
retractTree(_id):-labelT(_id,_pid,_encl,_v1,_v2),!,retract(labelT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-switchT(_id,_pid,_encl,_v1,_v2),!,retract(switchT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-caseT(_id,_pid,_encl,_v1),!,retract(caseT(_id,_pid,_encl,_v1)).
retractTree(_id):-synchronizedT(_id,_pid,_encl,_v1,_v2),!,retract(synchronizedT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-tryT(_id,_pid,_encl,_v1,_v2,_v3),!,retract(tryT(_id,_pid,_encl,_v1,_v2,_v3)).
retractTree(_id):-catchT(_id,_pid,_encl,_v1,_v2),!,retract(catchT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-ifT(_id,_pid,_encl,_v1,_v2,_v3),!,retract(ifT(_id,_pid,_encl,_v1,_v2,_v3)).
retractTree(_id):-conditionalT(_id,_pid,_encl,_v1,_v2,_v3),!,retract(conditionalT(_id,_pid,_encl,_v1,_v2,_v3)).
/*format('rt:exec: ~w~n', [_id]),*/
retractTree(_id):-execT(_id,_pid,_encl,_v1),!,retract(execT(_id,_pid,_encl,_v1)).
retractTree(_id):-returnT(_id,_pid,_encl,_v1),!,retract(returnT(_id,_pid,_encl,_v1)).
retractTree(_id):-breakT(_id,_pid,_encl,_v1,_v2),!,retract(breakT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-continueT(_id,_pid,_encl,_v1,_v2),!,retract(continueT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-throwT(_id,_pid,_encl,_v1),!,retract(throwT(_id,_pid,_encl,_v1)).
retractTree(_id):-applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4),!,retract(applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4)).
retractTree(_id):-newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5),!,retract(newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5)).
retractTree(_id):-newArrayT(_id,_pid,_encl,_v1,_v2,_v3),!,retract(newArrayT(_id,_pid,_encl,_v1,_v2,_v3)).
retractTree(_id):-assignT(_id,_pid,_encl,_v1,_v2),!,retract(assignT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-assignopT(_id,_pid,_encl,_v1,_v2,_v3),!,retract(assignopT(_id,_pid,_encl,_v1,_v2,_v3)).
retractTree(_id):-operationT(_id,_pid,_encl,_v1,_v2,_v3),!,retract(operationT(_id,_pid,_encl,_v1,_v2,_v3)).
retractTree(_id):-typeCastT(_id,_pid,_encl,_v1,_v2),!,retract(typeCastT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-typeTestT(_id,_pid,_encl,_v1,_v2),!,retract(typeTestT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-indexedT(_id,_pid,_encl,_v1,_v2),!,retract(indexedT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-literalT(_id,_pid,_encl,_v1,_v2),!,retract(literalT(_id,_pid,_encl,_v1,_v2)).
retractTree(_id):-assertT(_id,_pid,_encl,_v1,_v2),!,retract(assertT(_id,_pid,_encl,_v1,_v2)).
%retractTree(_id):-classMembersT(_id,_v1),!,retract(classMembersT(_id,_v1)).
retractTree(_id):-externT(_id),!,retract(externT(_id)).
retractTree(_id):-importT(_id,_pid,_v1),!,retract(importT(_id,_pid,_v1)).
retractTree(_id):-nopT(_id,_pid,_encl),!,retract(nopT(_id,_pid,_encl)).
retractTree(_id):-precedenceT(_id,_pid,_encl,_v1),!,retract(precedenceT(_id,_pid,_encl,_v1)). 
retractTree(_id) :-
    not(tree(_id, _,_)),
    format('could not retract id: ~w~n', [_id]), !.


retractTrees([]).
retractTrees([_H, _T]) :-
    retractTree(_H),
    retractTrees(_T).
    

    