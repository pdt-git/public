% Author:  Günter Kniesel
% Date: 25.08.2005


/* ******************************************************************
   Abstract Syntax Tree (AST) definition for Tom Mens' PhD thesis language.
   *************************************************************** */
/*
 * This is the "new" representation of Tom Mens' PhD thesis language.
 */
 
 /**
  * ast_reference_type(?Language, ?Type)
  *
  * arg2 is a type whose elements are identities of AST nodes
  * in the syntax of language arg1.
  *
  * This is a multifile predicate. In case of differences, its
  * autoritative documentation is the one in the file
  * languageIndependentSyntax.pl.
  */
:- multifile ast_reference_type/2.

ast_reference_type('Tom', graph_node).

 /**
  * ast_node_def(?Language, ?AstNodeLabel, ?AstNodeArguments)
  *
  * The Abstract Syntax Tree (AST) for the language arg1 contains
  * nodes with label (functor) arg2 and arguments arg3.
  *
  * Language independent bottom up traversals of an AST are supported
  * by the convention that the second argument of every AST
  * node has the name 'parent' and refers to the parent node.
  *
  * This predicate must be defined for every language to be processed
  * by JTransformer. It is a multifile predicate. In case of
  * differences, its autoritative documentation is the one in the file
  * languageIndependent.pl.
  */
:- multifile ast_node_def/3.

ast_node_def('Graphs',graph_node,[
     ast_arg(id,       mult(1,1,no ),  id,    [id]), % <-- convention!!!
     ast_arg(nodeType, mult(1,1,no ),  attr,  [atom])
]).

ast_node_def('Graphs',graph_edge,[
     ast_arg(id,         mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(sourceNode, mult(1,1,no ), id,  [graph_node]),
     ast_arg(targetNode, mult(1,1,no ), id,  [graph_node]),
     ast_arg(edgeType,   mult(1,1,no ), attr,[atom])
]).



