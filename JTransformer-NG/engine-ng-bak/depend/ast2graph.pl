/*****************************************************************
 * Graph Sicht auf Programmelemente (unabhängig von Faktenbasis)
 *  ast_node(?type, +term, ?id)
 *  ast_edge(?type, +term, ?eid)
 *  ast_attr(?type, +term, ?attr)
 *****************************************************************/

:- multifile ast_node/3.
:- multifile ast_edge/3.
:- multifile ast_attr/3.



% TODO: neue AST Elements

/***************************** provisorisch **********************
* solange expansion von high level nicht klappt:
*  highlevel elemente = low level elemente
******************************************************************/

ast_node(T, class(A,B,C), X) :- ast_node(T,classDefT(A,B,C,_), X) .
ast_node(T, method(A,B,C,D,E,F,G), X) :- ast_node(T,methodDefT(A,B,C,D,E,F,G), X) .
ast_node(T, field(A,B,D,E,F), X) :- ast_node(T,fieldDefT(A,B,D,E,F), X) .
ast_node(T, paramDefT(A,B,D,E), X) :- ast_node(T,paramDefT(A,B,D,E), X) .
ast_node(T, localDefT(A,B,C,D,E,F), X) :- ast_node(T,localDefT(A,B,C,D,E,F), X) .
ast_node(T, getFieldT(A,B,C,D,E,F), X) :- ast_node(T,getFieldT(A,B,C,D,E,F), X) .
%ToWi: ast_node(T, getField(A,B,C,D,E), X) :- ast_node(T,selectT(A,B,C,_,D,E), X) .
%ToWi: ast_node(T, setField(A,B,C,_,D,E), X) :- ast_node(T,assignT(A,B,C,D,E), X) .
%ToWi: ast_node(T, methodCall(A,B,C,_,D,E), X) :- ast_node(T,applyT(A,B,C,D,E), X) .

ast_edge(T, class(A,B,C), X) :- ast_edge(T,classDefT(A,B,C,_), X) .
ast_edge(T, method(A,B,C,D,E,F,G), X) :- ast_edge(T,methodDefT(A,B,C,D,E,F,G), X) .
ast_edge(T, field(A,B,D,E,F), X) :- ast_edge(T,fieldDefT(A,B,D,E,F), X) .
%ast_edge(T, param(A,B,D,E), X) :- ast_edge(T,paramDefT(A,B,D,E), X) .
%ast_edge(T, localVar(A,B,C,D,E,F), X) :- ast_edge(T,localDefT(A,B,C,D,E,F), X) .
%ast_edge(T, getField(A,B,C,_,E), X) :- ast_edge(T,identT(A,B,C,_,E), X) .
%ToWi: ast_edge(T, getField(A,B,C,D,E), X) :- ast_edge(T,selectT(A,B,C,_,D,E), X) .
%ToWi: ast_edge(T, setField(A,B,C,_,D,E), X) :- ast_edge(T,assignT(A,B,C,D,E), X) . % ohne recv
%ToWi: ast_edge(T, methodCall(A,B,C,_,D,E), X) :- ast_edge(T,applyT(A,B,C,D,E), X) . % ohne recv

ast_attr(T, class(A,B,C), X) :- ast_attr(T,classDefT(A,B,C,_), X) .
ast_attr(T, method(A,B,C,D,E,F,G), X) :- ast_attr(T,methodDefT(A,B,C,D,E,F,G), X) .
ast_attr(T, field(A,B,D,E,F), X) :- ast_attr(T,fieldDefT(A,B,D,E,F), X) .
%ast_attr(T, param(A,B,D,E), X) :- ast_attr(T,paramDefT(A,B,D,E), X) .
%ast_attr(T, localVar(A,B,C,D,E,F), X) :- ast_attr(T,localDefT(A,B,C,D,E,F), X) .
%ast_attr(T, getField(A,B,C,_,E), X) :- ast_attr(T,identT(A,B,C,_,E), X) .
%ToWi: ast_attr(T, getField(A,B,C,D,E), X) :- ast_attr(T,selectT(A,B,C,_,D,E), X) .
%ToWi: ast_attr(T, setField(A,B,C,_,D,E), X) :- ast_attr(T,assignT(A,B,C,D,E), X) .
%ToWi: ast_attr(T, methodCall(A,B,C,_,D,E), X) :- ast_attr(T,applyT(A,B,C,D,E), X) .


/************************* nodes ***************************
*  ast_node(?type, +term, ?id)
*
* Keine Knoten: implementsT, extendsT, modifierT, externT
*****************************************************************/

ast_node(applyT, applyT(_id, _, _, _, _,_,_), _id).
ast_node(assignT, assignT(_id, _, _, _, _), _id).
ast_node(assignopT, assignopT(_id, _, _, _, _,_), _id).    %%% GK++

ast_node(blockT, blockT(_id, _, _, _), _id).
ast_node(breakT, breakT(_id, _, _, _, _), _id).
ast_node(caseT, caseT(_id, _, _, _), _id).
ast_node(catchT, catchT(_id, _, _, _, _), _id).
ast_node(literalT, catchT(_id, _, _, _, _), _id).
ast_node(assertT, assertT(_id, _, _, _, _), _id).
ast_node(classDefT, classDefT(_id, _, _, _), _id).
ast_node(conditionalT, conditionalT(_id, _, _, _, _, _), _id).
ast_node(continueT, continueT(_id, _, _, _, _), _id).
ast_node(doLoopT, doLoopT(_id, _, _, _, _), _id).
ast_node(execT, execT(_id, _, _, _), _id).
ast_node(forLoopT, forLoopT(_id, _, _, _, _, _, _), _id).
ast_node(getFieldT, getFieldT(_id, _, _, _, _,_), _id).    %%% GK++
ast_node(identT, identT(_id, _, _, _, _), _id).
ast_node(ifT, ifT(_id, _, _, _, _, _), _id).
ast_node(indexedT, indexedT(_id, _, _, _, _), _id).
ast_node(labelT, labelT(_id, _, _, _, _), _id).
ast_node(methodDefT, methodDefT(_id, _, _, _, _, _, _), _id).
ast_node(newArrayT, newArrayT(_id, _, _,  _, _, _), _id).
ast_node(newClassT, newClassT(_id, _, _, _, _, _, _, _), _id).
ast_node(operationT, operationT(_id, _, _, _, _, _), _id).
ast_node(packageT, packageT(_id,_), _id).
ast_node(returnT, returnT(_id, _, _, _), _id).
ast_node(selectT, selectT(_id, _, _, _, _, _), _id).
ast_node(switchT, switchT(_id, _, _, _, _), _id).
ast_node(synchronizedT, synchronizedT(_id, _, _, _, _), _id).
ast_node(throwT, throwT(_id, _, _, _), _id).
ast_node(toplevelT, toplevelT(_id, _, _, _), _id).
ast_node(tryT, tryT(_id, _, _, _, _, _), _id).
ast_node(typeCastT, typeCastT(_id, _, _, _, _), _id).
ast_node(whileLoopT, whileLoopT(_id, _, _, _, _), _id).
ast_node(fieldDefT, fieldDefT(_id, _, _, _, _), _id).
ast_node(localDefT, localDefT(_id, _, _, _, _, _), _id).
ast_node(paramDefT, paramDefT(_id, _, _, _), _id).
ast_node(whileLoopT, whileLoopT(_id, _, _, _, _), _id).
ast_node(nopT, nopT(_id, _, _), _id).
% Next clause commented out since it produces infinite many results of
% the form "not(not(...(not(PEF))...))" by backtracking -- GK, 3.9.2004:
% ast_node(_type, not(_tree), _id) :- !, ast_node(_type, _tree, _id).
% CHECK: Is this better?
% ast_node(_type, not(_tree), _id) :- var(_tree), !, ast_node(_type, _tree, _id).
% ast_node(_type, not(_tree), _id) :- nonvar(_tree), !, ast_node(_type, _tree, _id).


/* 
  Könnte man eigentlich alles ersetzen durch:
  
  astNodeSignature(label,arity). ... u.s.w. 
  
  ast_label_node_id(_label,_node,_id) :-
      astNodeSignature(_label,_arity),           
      functor(_node, _label,_arity),     % make term with right label and arity
      _node =.. [_label, _id|_].         % return its first argument as the id
      
*/

/************************* edges ***************************
*  ast_edge(?type,+term,?edge)
*****************************************************************/

% Next clause commented out since it produces infinite many results of
% the form "not(not(...(not(PEF))...))" by backtracking -- GK, 3.9.2004:
%ast_edge(_type, not(_tree), _eid) :- !, ast_edge(_type, _tree, _eid).

% parent edge
ast_edge(parent, applyT(_, _id, _,_,_, _, _), _id).
ast_edge(parent, assignT(_, _id, _, _, _), _id).
ast_edge(parent, assignopT(_, _id, _, _, _,_), _id).    %%% GK++
ast_edge(parent, blockT(_, _id, _, _), _id).
ast_edge(parent, breakT(_, _id, _, _, _), _id).
ast_edge(parent, caseT(_, _id, _, _), _id).
ast_edge(parent, catchT(_, _id, _, _, _), _id).
ast_edge(parent, literalT(_, _id, _, _, _), _id).
ast_edge(parent, assertT(_, _id, _, _, _), _id).
ast_edge(parent, classDefT(_, _id, _, _), _id).
ast_edge(parent, conditionalT(_, _id, _, _, _, _), _id).
ast_edge(parent, continueT(_, _id, _, _, _), _id).
ast_edge(parent, doLoopT(_, _id, _, _, _), _id).
ast_edge(parent, execT(_, _id, _, _), _id).
ast_edge(parent, forLoopT(_, _id, _, _, _, _, _), _id).
ast_edge(parent, getFieldT( _, _id, _, _, _, _), _id).    %%% GK++
ast_edge(parent, identT(_, _id, _, _, _), _id).
ast_edge(parent, ifT(_, _id, _, _, _, _), _id).
ast_edge(parent, indexedT(_, _id, _, _, _), _id).
ast_edge(parent, labelT(_, _id, _, _, _), _id).
ast_edge(parent, methodDefT(_, _id, _, _, _, _, _), _id).
ast_edge(parent, newArrayT(_, _id, _, _, _, _), _id).
ast_edge(parent, newClassT(_, _id, _, _, _, _, _, _), _id).
ast_edge(parent, operationT(_, _id, _, _, _, _), _id).
%ast_edge(parent, packageT(_id,_), _id).
ast_edge(parent, returnT(_, _id, _, _), _id).
ast_edge(parent, selectT(_, _id, _, _, _, _), _id).
ast_edge(parent, switchT(_, _id, _, _, _), _id).
ast_edge(parent, synchronizedT(_, _id, _, _, _), _id).
ast_edge(parent, throwT(_, _id, _, _), _id).
ast_edge(parent, toplevelT(_, _id, _, _), _id).
ast_edge(parent, tryT(_, _id, _, _, _, _), _id).
ast_edge(parent, typeCastT(_, _id, _, _, _), _id).
ast_edge(parent, typeTestT(_, _id, _, _, _), _id).
ast_edge(parent, fieldDefT(_, _id,  _, _, _), _id).
ast_edge(parent, localDefT(_, _id, _, _, _, _), _id).
ast_edge(parent, paramDefT(_, _id, _, _), _id).
ast_edge(parent, whileLoopT(_, _id, _, _, _), _id).
ast_edge(parent, precedenceT(_, _id, _, _), _id).
ast_edge(parent, nopT( _, _id,_), _id).               %%% GK++
% enclosing method
ast_edge(encl, applyT(_, _, _id, _,_,_, _), _id).
ast_edge(encl, assignT(_, _, _id, _, _), _id).
ast_edge(encl, assignopT(_, _, _id, _, _,_), _id).    %%% GK++
ast_edge(encl, blockT(_, _, _id, _), _id).
ast_edge(encl, breakT(_, _, _id, _, _), _id).
ast_edge(encl, caseT(_, _, _id, _), _id).
ast_edge(encl, catchT(_, _, _id, _, _), _id).
ast_edge(encl, literalT(_, _, _id, _, _), _id).
ast_edge(encl, assertT(_, _, _id, _, _), _id).
%ast_edge(encl, classDefT(_, _, _id, _), _id).
ast_edge(encl, conditionalT(_, _, _id, _, _, _), _id).
ast_edge(encl, continueT(_, _, _id, _, _), _id).
ast_edge(encl, doLoopT(_, _, _id, _, _), _id).
ast_edge(encl, execT(_, _, _id, _), _id).
ast_edge(encl, forLoopT(_, _, _id, _, _, _, _), _id).
ast_edge(encl, getFieldT( _, _, _id, _, _, _), _id).    %%% GK++
ast_edge(encl, identT(_, _, _id, _, _), _id).
ast_edge(encl, ifT(_, _, _id, _, _, _), _id).
ast_edge(encl, indexedT(_, _, _id, _, _), _id).
ast_edge(encl, labelT(_, _, _id, _, _), _id).
%ast_edge(encl, methodDefT(_, _, _id, _, _, _, _), _id).
ast_edge(encl, newArrayT(_, _, _id, _, _, _), _id).
ast_edge(encl, newClassT(_, _, _id, _, _, _, _, _), _id).
ast_edge(encl, operationT(_, _, _id, _, _, _), _id).
%ast_edge(encl, packageT(_id,_), _id).
ast_edge(encl, returnT(_, _, _id, _), _id).
ast_edge(encl, selectT(_, _, _id, _, _, _), _id).
ast_edge(encl, switchT(_, _, _id, _, _), _id).
ast_edge(encl, synchronizedT(_, _, _id, _, _), _id).
ast_edge(encl, throwT(_, _, _id, _), _id).
ast_edge(encl, toplevelT(_, _, _id, _), _id).
ast_edge(encl, tryT(_, _, _id, _, _, _), _id).
ast_edge(encl, typeCastT(_, _, _id, _, _), _id).
ast_edge(encl, typeTestT(_, _, _id, _, _), _id).
ast_edge(encl, localDefT(_, _, _id, _, _, _), _id).
ast_edge(encl, whileLoopT(_, _, _id, _, _), _id).
ast_edge(encl, precedenceT(_, _, _id, _), _id).
ast_edge(encl, nopT( _, _, _id), _id).                %%% GK++

% references
ast_edge(reference, identT(_id, _, _, _, _refid), _refid).
ast_edge(reference, selectT(_id, _, _, _, _, _refid),_refid).
ast_edge(reference, applyT(_id,_, _, _, _, _, _refid),_refid).
ast_edge(reference, getFieldT(_id, _, _, _, _, _refid),_refid).
ast_edge(reference, newClassT(_id, _, _, _refid, _, _, _, _),_refid).

% child edges
% 1 to n : end with s
ast_edge(implements, implementsT(_id, _eid), _eid1) :- list_member(_eid1,_eid).
ast_edge(defs, toplevelT(_id, _, _, _eid), _eid1) :- list_member(_eid1,_eid).
ast_edge(defs, classDefT(_id, _, _, _eid), _eid1) :- list_member(_eid1,_eid).
ast_edge(args, methodDefT(_id, _, _, _eid, _, _, _), _eid1) :- list_member(_eid1,_eid).
ast_edge(args, applyT(_id, _, _, _, _,_eid,_), _eid1) :- list_member(_eid1,_eid).
ast_edge(args, newClassT(_id, _, _, _, _eid, _, _, _), _eid1) :- list_member(_eid1,_eid).
ast_edge(args, operationT(_id, _, _, _eid, _, _), _eid1) :- list_member(_eid1,_eid).
ast_edge(excs, methodDefT(_id, _, _, _, _, _eid, _), _eid1) :- list_member(_eid1,_eid).
ast_edge(stats, blockT(_id, _, _, _eid), _eid1) :- list_member(_eid1,_eid).
ast_edge(inits, forLoopT(_id, _, _, _eid, _, _, _), _eid1) :- list_member(_eid1,_eid).
ast_edge(steps, forLoopT(_id, _, _, _, _, _eid, _), _eid1) :- list_member(_eid1,_eid).
ast_edge(cases, switchT(_id, _, _, _, _eid), _eid1) :- list_member(_eid1,_eid).
ast_edge(catchers, tryT(_id, _, _, _, _eid, _), _eid1) :- list_member(_eid1,_eid).
ast_edge(dims, newArrayT(_id, _, _, _eid, _, _), _eid1) :- list_member(_eid1,_eid).
ast_edge(elems, newArrayT(_id, _, _, _, _eid, _), _eid1) :- list_member(_eid1,_eid).
% 1 to 1
ast_edge(method, applyT(_id, _, _, _, _,_,_method), _method).
ast_edge(extend, extendsT(_id, _eid), _eid).
ast_edge(body, methodDefT(_id, _, _, _, _, _, _eid), _eid).
ast_edge(init, fieldDefT(_id, _, _, _, _eid), _eid).
ast_edge(init, localDefT(_id, _, _, _, _, _eid), _eid).
ast_edge(selected, selectT(_id, _, _, _, _eid, _), _eid).
ast_edge(exec, execT(_id, _, _, _eid), _eid).
ast_edge(target, breakT(_id, _, _, _, _eid), _eid).
ast_edge(label, continueT(_id, _, _, _eid, _), _eid).
ast_edge(target, continueT(_id, _, _, _, _eid), _eid).
ast_edge(exec, returnT(_id, _, _, _eid), _eid).
ast_edge(cond, doLoopT(_id, _, _, _eid, _), _eid).
ast_edge(body, doLoopT(_id, _, _, _, _eid), _eid).
ast_edge(cond, whileLoopT(_id, _, _, _eid, _), _eid).
ast_edge(body, whileLoopT(_id, _, _, _, _eid), _eid).
ast_edge(cond, forLoopT(_id, _, _, _, _eid, _, _), _eid).
ast_edge(body, forLoopT(_id, _, _, _, _, _, _eid), _eid).
ast_edge(body, labelT(_id, _, _, _eid, _), _eid).
ast_edge(selector, switchT(_id, _, _, _eid, _), _eid).
ast_edge(label, caseT(_id, _, _, _eid), _eid).
ast_edge(body, synchronizedT(_id, _, _, _, _eid), _eid).
ast_edge(lock, synchronizedT(_id, _, _, _eid, _), _eid).
ast_edge(body, tryT(_id, _, _, _eid, _, _), _eid).
ast_edge(finalize, tryT(_id, _, _, _, _, _eid), _eid).
ast_edge(param, catchT(_id, _, _, _eid, _), _eid).
ast_edge(body, catchT(_id, _, _, _, _eid), _eid).
ast_edge(test, assertT(_id, _, _, _eid, _), _eid).
ast_edge(msg, assertT(_id, _, _, _, _eid), _eid).
ast_edge(cond, ifT(_id, _, _, _eid, _, _), _eid).
ast_edge(if, ifT(_id, _, _, _, _eid, _), _eid).
ast_edge(then, ifT(_id, _, _, _, _, _eid), _eid).
ast_edge(cond, conditionalT(_id, _, _, _eid, _, _), _eid).
ast_edge(if, conditionalT(_id, _, _, _, _eid, _), _eid).
ast_edge(then, conditionalT(_id, _, _, _, _, _eid), _eid).
ast_edge(exec, throwT(_id, _, _, _eid), _eid).
ast_edge(recv, applyT(_id, _, _, _eid, _,_,_), _eid).
ast_edge(def, newClassT(_id, _, _, _, _, _, _eid, _), _eid).
ast_edge(enclosing, newClassT(_id, _, _, _, _, _, _, _eid), _eid).
ast_edge(rhs, assignT(_id, _, _, _, _eid), _eid).
ast_edge(lhs, assignT(_id, _, _, _eid, _), _eid).
ast_edge(expr, typeCastT(_id, _, _, _, _eid), _eid).
ast_edge(expr, typeTestT(_id, _, _, _, _eid), _eid).
ast_edge(index, indexedT(_id, _, _, _eid, _), _eid).
ast_edge(expr, precedenceT(_id,_,_,_eid), _eid).


% Falls es eine Liste von Kanten ist  (1:N)
list_member(_m, _l) :- is_list(_l), member(_m, _l).


/************************* attributes ***************************
*  ast_attr(?type,+term,?attr)
*****************************************************************/
ast_attr(_type, not(_tree), _attr) :- !, ast_attr(_type, _tree, _attr).

ast_attr(name, packageT(_id, _attr), _attr).
ast_attr(name, classDefT(_id, _, _attr, _), _attr).
ast_attr(name, methodDefT(_id, _, _attr, _, _, _, _), _attr).
ast_attr(name, localDefT(_id, _, _, _, _attr, _), _attr).
ast_attr(name, fieldDefT(_id, _, _, _attr, _), _attr).
ast_attr(name, paramDefT(_id, _, _, _attr), _attr).
ast_attr(name, selectT(_id, _, _, _attr, _, _), _attr).
ast_attr(name, identT(_id, _, _, _attr, _), _attr).
ast_attr(name, applyT(_id, _, _, _, _attr, _, _), _attr).

ast_attr(type, methodDefT(_id, _, _, _, _attr, _, _), _attr).
ast_attr(type, fieldDefT(_id, _, _attr, _, _), _attr).
ast_attr(type, localDefT(_id, _, _, _attr, _, _), _attr).
ast_attr(type, paramDefT(_id, _, _attr, _), _attr).
ast_attr(type, literalT(_id, _, _, _attr, _), _attr).
ast_attr(type, newArrayT(_id, _, _, _, _, _attr), _attr).
ast_attr(type, typeCastT(_id, _, _, _attr, _), _attr).
ast_attr(type, typeTestT(_id, _, _, _attr, _), _attr).


ast_attr(modifier, modifierT(_id, _attr), _attr).
ast_attr(modifier, externT(_id), _attr).
ast_attr(modifier, interfaceT(_id), _attr).

ast_attr(label, labelT(_id, _, _, _, _attr), _attr).
ast_attr(label, breakT(_id, _, _, _attr, _), _attr).
ast_attr(filename, toplevelT(_id, _, _attr, _), _attr).
ast_attr(import, importT(_id, _, _attr), _attr).
ast_attr(value, literalT(_id, _, _, _, _attr), _attr).
ast_attr(indexed, indexedT(_id, _, _, _, _attr), _attr).
ast_attr(operator, operationT(_id, _, _, _, _attr, _), _attr).
ast_attr(pos, operationT(_id, _, _, _, _, _attr), _attr).


