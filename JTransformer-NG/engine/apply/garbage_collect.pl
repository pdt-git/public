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

retractTrees([]).
retractTrees([_H, _T]) :-
    retractTree(_H),
    retractTrees(_T).
    

    