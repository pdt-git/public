% Author:  Günter Kniesel
% Date: 25.08.2005


/* ******************************************************************
   Abstract Syntax Tree (AST) definition for Tom Mens' PhD thesis language.
   *************************************************************** */
/*
 * This is the "old" representation of Tom Mens' PhD thesis language.
 */
:- multifile ast_node/3.
:- multifile ast_edge/3.
:- multifile ast_attr/3.

ast_node(abstract_node, abstract_node(_id,                _type), _id ).  % for Tom Mens
ast_node(abstract_edge, abstract_edge(_id,_node1, _node2, _type), _id ).  % for Tom Mens

ast_edge(sourceNode, abstract_edge(_id,_node1, _node2, _type), _node1 ).  % for Tom Mens
ast_edge(targetNode, abstract_edge(_id,_node1, _node2, _type), _node2 ).  % for Tom Mens

ast_attr(nodeType, abstract_node(_id,                _type), _type ).  % for Tom Mens
ast_attr(edgeType, abstract_edge(_id,_node1, _node2, _type), _type ).  % for Tom Mens


