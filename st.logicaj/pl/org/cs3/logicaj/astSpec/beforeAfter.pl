

/*
 * Dep-Graph for setter and counter
 * ?- gen_dep_graph([addSetterInterf, addSetterCode, addCounterInterf, addCounterCode]).
 *
 * Dep-Graph for storage, anotherFlag and association
 * ?- gen_dep_graph([addPersistenceInterf, addPersistenceCode, addAssociationInterface, addAssociationCode, addAnotherFlag]).
 *
 * Dep-Graph for counter and storage
 * ?- gen_dep_graph([addCounterInterf, addCounterCode, addPersistenceInterf, addPersistenceCode]).
 *
 * Dep-Graph for all CT's
 * ?- gen_dep_graph([addSetterInterf, addSetterCode, addCounterInterf, addCounterCode, addPersistenceInterf, addPersistenceCode, addAnotherFlag, addAssociationInterface, addAssociationCode]).
 */

/* Assumptions:
 * - Readfields are represented as identT/5
 * - Writefields are represented as assignT/5
 */
 
 
% Treat before- and after-avice-like AST-elements in dependency analysis

%ast_node(before,before(A,_),A).
%ast_node(after,after(A,_),A).
:- multifile ast_node/3.
:- multifile ast_edge/3.
:- multifile ast_attr/3.

/*
ast_node(before,before(A,B),before(A,B)).
ast_node(after,after(A,B),after(A,B)).
*/

ast_node(before(A,B),before(A,B)).
ast_node(after(A,B),after(A,B)).

ast_edge(after,after(A,_),A).
ast_edge(after,after(_,A),A).
ast_edge(before,before(A,_),A).
ast_edge(before,before(_,A),A).