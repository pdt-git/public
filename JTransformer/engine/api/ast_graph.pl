% Author:
% Date: 12.11.2002

/*
AST-Graph:

node(_type, _id).

attr(_type, _node, _attr).

edge(_type, _startnode, _endnode).
*/

% markiere folgende Regeln als Abstraktionen, die in Condition-Teil der CT's
% verwendet werden können und dort expandiert werden
cond(node(_type, _id)).
cond(attr(_type, _node, _attr)).
cond(edge(_type, _startnode, _endnode)).


/************************* nodes ***************************/
%node(_type, _id) :- tree(_id, _, _type).
node(toplevelT, _id):- toplevelT(_id, _, _, _).
node(classDefT, _id):- classDefT(_id, _, _, _).
node(methodDefT, _id):- methodDefT(_id, _, _, _, _, _, _).
node(blockT, _id):- blockT(_id, _, _, _).
node(forLoopT, _id):- forLoopT(_id, _, _, _, _, _, _).
node(switchT, _id):- switchT(_id, _, _, _, _).
node(tryT, _id):- tryT(_id, _, _, _, _, _).
node(applyT, _id):- applyT(_id, _, _, _, _,_,_).
node(newClassT, _id):- newClassT(_id, _, _, _, _, _, _, _).
node(newArrayT, _id):- newArrayT(_id, _, _, _, _, _).
node(operationT, _id):- operationT(_id, _, _, _, _, _).

node(paramDefT, _id):- paramDefT(_id, _, _, _).
node(localDefT, _id):- localDefT(_id, _, _, _,_,_).
node(fieldDefT, _id):- fieldDefT(_id, _, _, _,_).
node(identT, _id):- identT(_id, _, _, _, _).
node(selectT, _id):- selectT(_id, _, _, _, _, _).
node(getFieldT, _id):- getFieldT(_id, _, _, _, _, _).
node(execT, _id):- execT(_id, _, _, _).
node(breakT, _id):- breakT(_id, _, _, _, _).
node(continueT, _id):- continueT(_id, _, _, _, _).
node(returnT, _id):- returnT(_id, _, _, _).
node(doLoopT, _id):- doLoopT(_id, _, _, _, _).
node(whileLoopT, _id):- whileLoopT(_id, _, _, _, _).
node(labelT, _id):- labelT(_id, _, _, _, _).
node(switchT, _id):- switchT(_id, _, _, _, _).
node(caseT, _id):- caseT(_id, _, _, _).
node(synchronizedT, _id):- synchronizedT(_id, _, _, _, _).
node(tryT, _id):- tryT(_id, _, _, _, _, _).
node(catchT, _id):- catchT(_id, _, _, _, _).
node(ifT, _id):- ifT(_id, _, _, _, _, _).
node(conditionalT, _id):- conditionalT(_id, _, _, _, _, _).
node(throwT, _id):- throwT(_id, _, _, _).
node(applyT, _id):- applyT(_id, _, _, _, _,_,_).
node(newClassT, _id):- newClassT(_id, _, _, _, _, _, _, _).
node(assignT, _id):- assignT(_id, _, _, _, _).
node(typeCastT, _id):- typeCastT(_id, _, _, _, _).
node(typeTestT, _id):- typeTestT(_id, _, _, _, _).
node(indexedT, _id):- indexedT(_id, _, _, _, _).

/************************* edges ***************************/


edge(parent, _id, _eid):-identT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-execT(_id, _eid,_,_).
edge(parent, _id, _eid):-methodDefT(_id, _eid,_,_,_,_,_).
edge(parent, _id, _eid):-blockT(_id, _eid,_,_).
edge(parent, _id, _eid):-localDefT(_id, _eid,_,_,_,_).
edge(parent, _id, _eid):-fieldDefT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-paramDefT(_id, _eid,_,_).
edge(parent, _id, _eid):-newClassT(_id, _eid,_,_,_,_,_,_).
edge(parent, _id, _eid):-assignT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-importT(_id, _eid,_).
edge(parent, _id, _eid):-classDefT(_id, _eid,_,_).
edge(parent, _id, _eid):-applyT(_id, _eid,_,_,_,_,_).
edge(parent, _id, _eid):-packageT(_id, _eid).
edge(parent, _id, _eid):-toplevelT(_id, _eid,_,_).
edge(parent, _id, _eid):-selectT(_id, _eid,_,_,_,_).
edge(parent, _id, _eid):-getFieldT(_id, _eid,_,_,_,_).
edge(parent, _id, _eid):-switchT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-typeCastT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-tryT(_id, _eid,_,_,_,_).
edge(parent, _id, _eid):-whileLoopT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-newArrayT(_id, _eid,_,_,_,_).
edge(parent, _id, _eid):-continueT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-doLoopT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-literalT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-assertT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-indexedT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-throwT(_id, _eid,_,_).
edge(parent, _id, _eid):-forLoopT(_id, _eid,_,_,_,_,_).
edge(parent, _id, _eid):-synchronizedT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-conditionalT(_id, _eid,_,_,_,_).
edge(parent, _id, _eid):-labelT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-breakT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-typeTestT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-assignopT(_id, _eid,_,_,_,_).
edge(parent, _id, _eid):-ifT(_id, _eid,_,_,_,_).
edge(parent, _id, _eid):-operationT(_id, _eid,_,_,_,_).
edge(parent, _id, _eid):-caseT(_id, _eid,_,_).
edge(parent, _id, _eid):-catchT(_id, _eid,_,_,_).
edge(parent, _id, _eid):-returnT(_id, _eid,_,_).

edge(encl, _id, _eid):-identT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-execT(_id,_,_eid,_).
% encl is = parent
edge(encl, _id, _eid):-methodDefT(_id,_eid,_,_,_,_,_).
edge(encl, _id, _eid):-blockT(_id,_,_eid,_).
edge(encl, _id, _eid):-localDefT(_id,_,_eid,_,_,_).
edge(encl, _id, _eid):-newClassT(_id,_,_eid,_,_,_,_,_).
edge(encl, _id, _eid):-assignT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-importT(_id,_,_eid).
edge(encl, _id, _eid):-applyT(_id,_,_eid,_,_,_,_).
%edge(encl, _id, _eid):-classDefT(_id,_eid,_,_).
%edge(encl, _id, _eid):-packageT(_id,_,_eid).
%edge(encl, _id, _eid):-toplevelT(_id,_,_eid,_).
edge(encl, _id, _eid):-selectT(_id,_,_eid,_,_,_).
edge(encl, _id, _eid):-getFieldT(_id,_,_eid,_,_,_).
edge(encl, _id, _eid):-switchT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-typeCastT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-tryT(_id,_,_eid,_,_,_).
edge(encl, _id, _eid):-whileLoopT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-newArrayT(_id,_,_eid,_,_,_).
edge(encl, _id, _eid):-continueT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-doLoopT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-literalT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-assertT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-indexedT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-throwT(_id,_,_eid,_).
edge(encl, _id, _eid):-forLoopT(_id,_,_eid,_,_,_,_).
edge(encl, _id, _eid):-synchronizedT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-conditionalT(_id,_,_eid,_,_,_).
edge(encl, _id, _eid):-labelT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-breakT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-typeTestT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-assignopT(_id,_,_eid,_,_,_).
edge(encl, _id, _eid):-ifT(_id,_,_eid,_,_,_).
edge(encl, _id, _eid):-operationT(_id,_,_eid,_,_,_).
edge(encl, _id, _eid):-caseT(_id,_,_eid,_).
edge(encl, _id, _eid):-catchT(_id,_,_eid,_,_).
edge(encl, _id, _eid):-returnT(_id,_,_eid,_).

% references
edge(reference, _id, _refid) :- identT(_id, _, _, _, _refid).
edge(reference, _id, _refid) :- selectT(_id, _, _, _, _, _refid).
edge(reference, _id, _refid) :- getFieldT(_id, _, _, _, _, _refid).
edge(reference, _id, _refid) :- newClassT(_id, _, _, _refid, _, _, _, _).

% child edges
% 1 to n : end with s
edge(extends, _id, _eid) :- extendsT(_id, _eid).
edge(implements, _id, _eid) :- implementsT(_id, _eid).
edge(defs, _id, _eid) :- toplevelT(_id, _, _, _eid).
edge(defs, _id, _eid) :- classDefT(_id, _, _, _eid).
edge(args, _id, _eid) :- methodDefT(_id, _, _, _eid, _, _, _).
edge(excs, _id, _eid) :- methodDefT(_id, _, _, _, _, _eid, _).
edge(stats, _id, _eid) :- blockT(_id, _, _, _eid).
edge(inits, _id, _eid) :- forLoopT(_id, _, _, _eid, _, _, _).
edge(steps, _id, _eid) :- forLoopT(_id, _, _, _, _, _eid, _).
edge(cases, _id, _eid) :- switchT(_id, _, _, _, _eid).
edge(catchers, _id, _eid) :- tryT(_id, _, _, _, _eid, _).
edge(args, _id, _eid) :- applyT(_id, _, _, _, _,_eid,_).
edge(args, _id, _eid) :- newClassT(_id, _, _, _, _eid, _, _, _).
edge(dims, _id, _eid) :- newArrayT(_id, _, _, _eid, _, _).
edge(elems, _id, _eid) :- newArrayT(_id, _, _, _, _eid, _).
edge(args, _id, _eid) :- operationT(_id, _, _, _eid, _, _).
% 1 to 1
edge(body, _id, _eid) :- methodDefT(_id, _, _, _, _, _, _eid).
edge(init, _id, _eid) :- localDefT(_id, _, _, _, _, _eid).
edge(init, _id, _eid) :- fieldDefT(_id, _, _, _,_eid).
edge(selected, _id, _eid) :- selectT(_id, _, _, _, _eid, _).
edge(selected, _id, _eid) :- getFieldT(_id, _, _, _, _eid, _).
edge(exec, _id, _eid) :- execT(_id, _, _, _eid).
edge(target, _id, _eid) :- breakT(_id, _, _, _, _eid).
edge(label, _id, _eid) :- continueT(_id, _, _, _eid, _).
edge(target, _id, _eid) :- continueT(_id, _, _, _, _eid).
edge(exec, _id, _eid) :- returnT(_id, _, _, _eid).
edge(cond, _id, _eid) :- doLoopT(_id, _, _, _eid, _).
edge(body, _id, _eid) :- doLoopT(_id, _, _, _, _eid).
edge(cond, _id, _eid) :- whileLoopT(_id, _, _, _eid, _).
edge(body, _id, _eid) :- whileLoopT(_id, _, _, _, _eid).
edge(cond, _id, _eid) :- forLoopT(_id, _, _, _, _eid, _, _).
edge(body, _id, _eid) :- forLoopT(_id, _, _, _, _, _, _eid).
edge(body, _id, _eid) :- labelT(_id, _, _, _eid, _).
edge(selector, _id, _eid) :- switchT(_id, _, _, _eid, _).
edge(label, _id, _eid) :- caseT(_id, _, _, _eid).
edge(body, _id, _eid) :- synchronizedT(_id, _, _, _, _eid).
edge(lock, _id, _eid) :- synchronizedT(_id, _, _, _eid, _).
edge(body, _id, _eid) :- tryT(_id, _, _, _eid, _, _).
edge(finalize, _id, _eid) :- tryT(_id, _, _, _, _, _eid).
edge(param, _id, _eid) :- catchT(_id, _, _, _eid, _).
edge(body, _id, _eid) :- catchT(_id, _, _, _, _eid).
edge(cond, _id, _eid) :- ifT(_id, _, _, _eid, _, _).
edge(if, _id, _eid) :- ifT(_id, _, _, _, _eid, _).
edge(then, _id, _eid) :- ifT(_id, _, _, _, _, _eid).
edge(cond, _id, _eid) :- conditionalT(_id, _, _, _eid, _, _).
edge(if, _id, _eid) :- conditionalT(_id, _, _, _, _eid, _).
edge(then, _id, _eid) :- conditionalT(_id, _, _, _, _, _eid).
edge(exec, _id, _eid) :- throwT(_id, _, _, _eid).
edge(recv, _id, _eid) :- applyT(_id, _, _, _eid, _,_,_).
edge(def, _id, _eid) :- newClassT(_id, _, _, _, _, _, _eid, _).
edge(enclosing, _id, _eid) :- newClassT(_id, _, _, _, _, _, _, _eid).
edge(rh, _id, _eid) :- assignT(_id, _, _, _, _eid).
edge(lh, _id, _eid) :- assignT(_id, _, _, _eid, _).
edge(expr, _id, _eid) :- typeCastT(_id, _, _, _, _eid).
edge(expr, _id, _eid) :- typeTestT(_id, _, _, _, _eid).
edge(index, _id, _eid) :- indexedT(_id, _, _, _eid, _).
edge(test, _id, _eid) :- assertT(_id, _, _, _eid,_).
edge(msg, _id, _eid) :- assertT(_id, _, _, _, _eid).


/************************* attributes ***************************/

edge(name, _id, _attr) :- applyT(_id, _, _, _, _attr,_,_).
attr(name, _id, _attr) :- packageT(_id, _attr).
attr(name, _id, _attr) :- classDefT(_id, _, _attr, _).
attr(name, _id, _attr) :- methodDefT(_id, _, _attr, _, _, _, _).
attr(name, _id, _attr) :- localDefT(_id, _, _, _, _attr, _).
attr(name, _id, _attr) :- paramDefT(_id, _, _, _attr).
attr(name, _id, _attr) :- fieldDefT(_id, _, _, _attr, _).
attr(name, _id, _attr) :- selectT(_id, _, _, _attr, _, _).
attr(name, _id, _attr) :- getFieldT(_id, _, _, _attr, _, _).
attr(name, _id, _attr) :- identT(_id, _, _, _attr, _).

attr(type, _id, _attr) :- methodDefT(_id, _, _, _, _attr, _, _).
attr(type, _id, _attr) :- localDefT(_id, _, _, _attr, _, _).
attr(type, _id, _attr) :- paramDefT(_id, _, _attr, _).
attr(type, _id, _attr) :- fieldDefT(_id, _,  _attr, _, _).
attr(type, _id, _attr) :- literalT(_id, _, _, _attr, _).
attr(type, _id, _attr) :- newArrayT(_id, _, _, _, _, _attr).
attr(type, _id, _attr) :- typeCastT(_id, _, _, _attr, _).
attr(type, _id, _attr) :- typeTestT(_id, _, _, _attr, _).


attr(modifier, _id, _attr) :- modifierT(_id, _attr).
attr(modifier, _id, extern) :- externT(_id).
attr(modifier, _id, interface) :- interfaceT(_id).

attr(label, _id, _attr) :- labelT(_id, _, _, _, _attr).
attr(label, _id, _attr) :- breakT(_id, _, _, _attr, _).
attr(filename, _id, _attr) :- toplevelT(_id, _, _attr, _).
attr(import, _id, _attr) :- importT(_id, _, _attr).
attr(value, _id, _attr) :- literalT(_id, _, _, _, _attr).
attr(indexed, _id, _attr) :- indexedT(_id, _, _, _, _attr).
attr(operator, _id, _attr) :- operationT(_id, _, _, _, _attr, _).
attr(pos, _id, _attr) :- operationT(_id, _, _, _, _, _attr).


/*
% bagof has problems: multiple solutions
attr(type, _id, type(_form, _ref, _dim)) :- methodDefT(_id, _, _, _, type(_form, _ref, _dim), _, _).
attr(type, _id, type(_form, _ref, _dim)) :- localDefT(_id, _, _, type(_form, _ref, _dim), _, _).
attr(type, _id, type(_form, _ref, _dim)) :- paramDefT(_id, _, type(_form, _ref, _dim), _).
attr(type, _id, type(_form, _ref, _dim)) :- fieldDefT(_id, _, type(_form, _ref, _dim), _, _).
attr(type, _id, type(_form, _ref, _dim)) :- literalT(_id, _, _, type(_form, _ref, _dim), _).
attr(type, _id, type(_form, _ref, _dim)) :- newArrayT(_id, _, _, _, _, type(_form, _ref, _dim), _).
attr(type, _id, type(_form, _ref, _dim)) :- typeCastT(_id, _, _, type(_form, _ref, _dim), _).
attr(type, _id, type(_form, _ref, _dim)) :- typeTestT(_id, _, _, type(_form, _ref, _dim), _).
*/

collectSharedVariables(_list, _shared) :-
    collectSharedVariable(_list, [], [], _all, _shared).
%    list_to_set_save(_shared, _sharedSet).

collectSharedVariable([], _all, _shared, _all, _shared).
collectSharedVariable([_h|_t], _all, _shared, _newAll, _newShared) :-
    free_variables(_h,_free),
    intersection_save(_free, _all, _s),
    append(_all, _free, _all2),
    append(_shared, _s, _shared2),
    collectSharedVariable(_t, _all2, _shared2, _newAll, _newShared).

intersection_save([],_set,[]).
intersection_save([_h|_t],_set, [_h|_T]) :-
    member_save(_h, _set),
    !,
    intersection_save(_t, _set,_T).
intersection_save([_h|_t],_set, _T) :-
    intersection_save(_t, _set,_T).

    
    
    
    
    
expandTrees(_treeList,_GraphElemList) :-
    maplist(expandTree, _treeList, _graphElemList),
    flatten(_graphElemList, _GraphElemList).

expandTree(not(_trees),not(_trees2)) :-
    is_list(_trees),
    !,
    expandTrees(_trees, _trees2).
expandTree(not(_tree),not(_tree2)) :-
    !,
    expandTree(_tree, _tree2).
expandTree(replace(_tree),replace(_tree2)) :-
    !,
    expandTree(_tree, _tree2).
expandTree(spezialization(_tree,_l),spezialization(_tree2,_l2)) :-
    !,
    expandTree(_tree, _tree2),
    expandTrees(_l, _l2).



expandTree(_tree,_GraphElemList) :-
    tree(_tree),
    !,
    expandNode(_tree, _l1),
    expandAttr(_tree,_l2),
    expandEdge(_tree,_l4),
    append(_l1, _l2, _l3),
    append(_l3, _l4, _GraphElemList).
expandTree(_nontree,_nontree).

expandAttr(_tree, _list) :- bagof(attr(F,G,H), clause(attr(F,G,H),_tree), _list), !.
expandAttr(_, []).
expandNode(_tree, _list) :- bagof(node(A,B)  , clause(node(A,B),_tree)  , _list), !.
expandNode(_, []).
expandEdge(_tree, _list) :-
    bagof(edge(C,D,E), clause(edge(C,D,E),_tree), _l3),
    % expand a list of edges to many edges
    maplist(expandEdges, _l3, _l4),
    flatten(_l4, _list),
    !.
expandEdge(_, []).


expandEdges(edge(_type,_start,_end), ExpandedList) :-
    nonvar(_end),
    is_list(_end),
    !,
    expandEdge(_type,_start,_end,ExpandedList).
expandEdges(edge(_type,_start,_end), edge(_type,_start,_end)).

expandEdge(_type,_start,[],[]).
expandEdge(_type,_start,[_h|_t],[edge(_type,_start,_h)|_T]) :-
    expandEdge(_type,_start,_t,_T).


printList([]).
printList([_h|_t]) :-
    term_to_atom(_h,_a),
    format('~a~n',[_a]),
    printList(_t).

filterDontCares(_sharedVars, _graphElemList, _FilteredGraphElemList) :-

    maplist(filterNots, _graphElemList, _graphElemList2),
    flatten(_graphElemList2, _graphElemList3),

    maplist(filterDontCare(_sharedVars, _graphElemList3), _graphElemList, _filteredGraphElemList),
    flatten(_filteredGraphElemList, _FilteredGraphElemList),
    !.


filterNots(not(spezialization(_s,_l)), _list) :-
    append(_s, _l, _list),
    !.
filterNots(spezialization(_s,_l), _list) :-
    append(_s, _l, _list),
    !.
filterNots(not(_list), _list) :- !.
filterNots(_elem, _elem) :- !.

% process terms inside not recursively
filterDontCare(_sharedVars, _graphElemList, not(_elems), not(_felems)) :-
    is_list(_elems),
    maplist(filterDontCare(_sharedVars, _graphElemList), _elems, _elems2),
    _elems2 \= [],
    !,
    flatten(_elems2, _felems).
filterDontCare(_, _graphElemList, not(_elems), []) :- !.

/*
filterDontCare(_sharedVars, _graphElemList, spezialization(_s,_l), spezialization(_fs,_fl)) :-
    maplist(filterDontCare(_sharedVars, _graphElemList), _s, _s2),
    flatten(_s2, _fs),
    maplist(filterDontCare(_sharedVars, _graphElemList), _l, _l2),
    flatten(_l2, _fl),
    !.
*/

filterDontCare(_, _graphElemList, node(_type, _node), node(_type, _node)) :- !.
% filter out unconnected edges
filterDontCare(_, _graphElemList, edge(_type, _startnode, _endnode), edge(_type, _startnode, _endnode)) :-
    member(node(_, _startnode2), _graphElemList),
    _startnode == _startnode2,
    member(node(_, _endnode2), _graphElemList),
    _endnode == _endnode2,
    !.
filterDontCare(_, _graphElemList, edge(_type, _startnode, _endnode),[]) :- !.
% filter out unused attributes
filterDontCare(_, _graphElemList, attr(_type, _node, _attr), attr(_type, _node, _attr)) :-
    member(node(_, _node2), _graphElemList),
    _node2 == _node,
    % todo: zu restriktiv: variable könnte woanders benutzt werden
    nonvar(_attr),
    !.
filterDontCare(_sharedVars, _graphElemList, attr(_type, _node, _attr), attr(_type, _node, _attr)) :-
    member(node(_, _node2), _graphElemList),
    _node2 == _node,
    % todo: zu restriktiv: variable könnte woanders benutzt werden
    var(_attr),
    member_save(_attr,_sharedVars),
    !.
filterDontCare(_, _graphElemList, attr(_type, _node, _attr),[]) :- !.
filterDontCare(_, _,_nontree, _nontree) :- !.

% todo: [] == not(edge(xxx))       ????????

% todo: reicht so nicht aus
%filterDontCare(_graphElemList, edge(_type, _startnode, [_h|_t])).
%    is_list(_endnodes),
%    _endnodes \== [],

 /*
graphCond(_cond, _GraphCond) :-
    expandTrees(_cond, _graphCond),
    filterDontCares(_graphCond, _GraphCond2),
    list_to_set_save(_GraphCond2, _GraphCond).
    
graphAct(_action, _graphCond, _GraphAction) :-
    expandTrees(_action, _graphAction),
%    filterDontCares(_graphCond, _GraphCond).
    maplist(filterReplace(_graphCond), _graphAction, _GraphAction2),
    flatten(_GraphAction2, _GraphAction).
%    list2set_save(_GraphCond2, _GraphCond).
    % filtering nicht nötig, da in action ALLES gebunden sein sollte
   */

filterReplace(_preCond, replace(_list), _list2) :-
    !,
    maplist(filterReplace_(_preCond), _list, _list2).
filterReplace(_preCond, _x, _x).

filterReplace_(_pre, node(_Type, ID), [not([node(OldType, ID)]), node(Type, ID)]) :-
    member(node(OldType, OldID), _pre),
    OldID == ID,
    OldType \== Type,
    !.
filterReplace_(_pre, node(_Type, _ID),[]) :- !.
filterReplace_(_pre, edge(_Type, SID, EID), [not([edge(Type, SID, OldEID)]), edge(Type, SID, EID)]) :-
    member(edge(Type, OldSID, OldEID), _pre),
    OldSID == SID,
    OldEID \== EID,
    !.
filterReplace_(_pre, edge(_Type, _SID, _EID),[]) :- !.
filterReplace_(_pre, attr(_Type, ID, ATTR), [not([attr(Type, ID, OldATTR)]), attr(Type, ID, ATTR)]) :-
    member(attr(Type, OldID, OldATTR), _pre),
    OldID == ID,
    OldATTR \== ATTR,
    !.
filterReplace_(_pre, attr(_Type, _ID, _ATTR),[]) :- !.
filterReplace_(_,_x,_x).


    


/*
graphAct2(_action, _graphCond, _GraphAction) :-
    expandTrees(_action, _graphAction),
    expandReplaces(_graphCond, _graphAction, _GraphAction).

graphExpandReplaces(_graphCond, _graphAction, _GraphAction) :-
    maplist(graphExpandReplace(_graphCond), _treeList, _graphElemList),
    flatten(_graphElemList, _GraphElemList).
*/

graphFilterAndExpand(_ct1, _ct2, _pre1ExpGGG, _pre2ExpGGG, _act2ReplaceGGG, _post2ExpGGG, _NE1, _NE2, _PATT1, _PATT2) :-
    % todo : provisorisch, vieles passiert doppelt
    filterAndExpand(_ct1, _ct2, _pre1Exp, _pre2Exp, _act2Replace, _post2Exp, _NE1, _NE2, _PATT1, _PATT2),
    
    graphCond(_pre1Exp, _pre1ExpGGG),
    graphCond(_pre2Exp, _pre2ExpGGG),
    graphAct(_act2Replace, _pre2ExpGGG, _act2ReplaceGGG),
    postCondition(_pre2ExpGGG, _act2ReplaceGGG, _post2ExpGGG).


ct_graphFilter(_ct, _GraphCond, _GraphAct, _NE, _PATT) :-

    % Old CT API
    ct_filter(_ct, _pre, _act, _NE, _PATT),
    append([_ct|_pre], _act, _NE, _PATT, _all),
    collectSharedVariables(_all, _shared),
    expandConditions(_pre, _preExpTree),
    expandActions(_act, _actExpTree),

    % map CT's to graph transformations
    expandTrees(_preExpTree, _preExp),
    expandTrees(_actExpTree, _actExp),
    
    % Postprocess Action: substiture replaces
    maplist(filterReplace(_preExp), _actExp, _graphAct),
%    filterDontCares(_shared, _graphAct, _graphAct2),
    list_to_set_save(_graphAct, _GraphAct),

    % PostProcess Condition: remove unused (_) facts
    filterDontCares(_shared, _preExp, _preExp2),
    list_to_set_save(_preExp2, _GraphCond).
    

/*
graphCT(_ct, _GraphCond, _GraphAction) :-
    ct(_ct, _pre, _act),

    expandCondition(_pre, _preExp),
    _GraphCond = _preExp,
%    graphCond(_preExp, _GraphCond),
    
    expandAction(_act, _actExp),
    _GraphAction = _actExp.
%    graphAct(_actExp, _GraphCond, _GraphAction).
*/
    
    

    


