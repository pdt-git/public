% Autor: Tobias Rho & Guenter Kniesel
% Date: 7.9.2004
% ToDo: Siehe Komentare nach Prologdoc zu ast_node_def

/* ******************************************************************
   Abstract Syntax Tree (AST) definition for the Java language.
   *************************************************************** */
 
 /**
  * ast_sub_tree(?Language, ?ArgumentLabel)
  *
  * In all nodes of the AST of language arg1, the argument with 
  * name arg2 refers to a subtree of the respective AST node 
  * (provided that the node has an argument with that name).
  *
  * This is the abstraction allowing language independent 
  * top down traversal of an AST. 
  * 
  * Language independent bottom up traversals are supported 
  * by the convention that the second argument of every AST
  * node has the name 'parent' and refers to the parent node.
  * See documentation of ast_node_def/3. 
  *
  * This is a multifile predicate. In case of differences, 
  * its autoritative documentation is the one in the file 
  * languageIndependent.pl.
  */
:- multifile ast_sub_tree/2.
  
ast_sub_tree('Java',expr).
ast_sub_tree('Java',msg).
ast_sub_tree('Java',args).
ast_sub_tree('Java',param).
ast_sub_tree('Java',lhs).
ast_sub_tree('Java',stmts).
ast_sub_tree('Java',target).
ast_sub_tree('Java',body).
ast_sub_tree('Java',annotation).
ast_sub_tree('Java',values).

 /** 
  * ast_reference_type(?Language, ?Type)   
  * 
  * arg2 is a type whose elements are identities of AST nodes
  * in the syntax of language arg1.  
  * 
  * Legal reference types are:
  *   - 'id' = Values of type 'id' are identities of AST nodes as used 
  *            internally by JTransformer. Identities used to refer to 
  *            other nodes (foreign keys in a database lingo) are 
  *            called references and the types denoting them are
  *            the real reference types. They are defined as:
  *   - If 'label' is the label of an AST node of language L
  *     then it is also a legal AST reference type of L. It denotes
  *     all ids of nodes with label 'label' in L. 
  *
  * A language definition may define additional reference types
  * as abstractions (supertypes) of a group of AST nodes. 
  * See also ast_node_subtype/3.
  *
  * This is a multifile predicate. In case of differences, its 
  * autoritative documentation is the one in the file 
  * languageIndependent.pl.
  */ 
:- multifile ast_reference_type/2.

ast_reference_type('Java', Label) :-
    expression_type('Java',Label).
ast_reference_type('Java',Label) :- 
    statement_type('Java',Label).
ast_reference_type('Java',Label) :- 
    annotation_expression_type('Java',Label).


% expression_type needs the first argument since it is an
% abstraction (from file languageAbstractions.pl).

:- multifile expression_type/2.

expression_type('Java',applyT).
expression_type('Java',assignT).
expression_type('Java',assignopT).
expression_type('Java',conditionalT).
expression_type('Java',getFieldT).
expression_type('Java',identT).
expression_type('Java',indexedT).
expression_type('Java',literalT).
expression_type('Java',newArrayT).
expression_type('Java',newClassT).
expression_type('Java',operationT).
expression_type('Java',precedenceT).
expression_type('Java',selectT).
expression_type('Java',typeCastT).
expression_type('Java',typeTestT).


% annotation_expression_type needs the first argument since it is an
% abstraction (from file languageAbstractions.pl).

:- multifile annotation_expression_type/2.

% From JSR 175:
% Member types
% ============
% The Type is restricted to 
% primitive types, String, Class, enum types,
% annotation types, and arrays of the preceding types. 
% It is permissible to use bounded wildcards to parameterize the Class return type, 
% and the compiler must enforce such bounds on annotations. 
%
% Annotations
% ===========
% If the member type is not an annotation type or an array type, 
% MemberValue must be a ConditionalExpression (JLS 15.25) whose 
% type is assignment compatible (JLS 5.2) with the member type. 
% (A ConditionalExprression is simply an expression without assignments, 
% and not necessarily an expression involving the conditional operator (? :).) 
% If member type is a primitive type or String, the ConditionalExpression 
% must be a constant expression (JLS 15.28). If the member type is Class, 
% the value must be a class literal (JLS 15.8.2). 
% If the member type is an enum type, the value must be the simple 
% (unqualified) name of an enum constant. Loosely speaking, 
% no matter what the member type, the value must be a compile-time constant. 
% Note that null is not a legal member value for any member type.

 % same comments as for expression_type_ above

annotation_expression_type('Java',literalT).
annotation_expression_type('Java',newArrayT).
annotation_expression_type('Java',identT). %TODO only allow typeExpr typeExprT
annotation_expression_type('Java',conditionalT). 
 

% statment_expression_type needs the first argument since it is an
% abstraction (from file languageAbstractions.pl).
:- multifile statement_type/2.
statement_type('Java',assertT).
statement_type('Java',assignopT).
statement_type('Java',assignT).
statement_type('Java',blockT).
statement_type('Java',breakT).
statement_type('Java',caseT).
statement_type('Java',catchT).
statement_type('Java',continueT).
statement_type('Java',doLoopT).
statement_type('Java',execT).
statement_type('Java',forLoopT).
statement_type('Java',ifT).
statement_type('Java',labelT).
statement_type('Java',localDefT).
statement_type('Java',nopT).
statement_type('Java',returnT).
statement_type('Java',switchT).
statement_type('Java',synchronizedT).
statement_type('Java',throwT).
statement_type('Java',tryT).
statement_type('Java',whileLoopT).

statementType(Kind):-
    statement_type('Java',Kind).

expressionType(Kind):-
    expression_type('Java',Kind).

annotationExpressionType(Kind):-
     annotation_expression_type('Java',Kind).
/** 
  * ast_node_subtype(?Language, ?SubType, ?SuperType)   
  * 
  * arg3 is a subtype of the AST type arg3 in language arg1.  
  * 
  * In every language syntax 
  * - the type 'id' is the common supertype of reference types
  * - the type 'nonRef' is the common supertype of non reference types
  *   ('atom', 'number' and 'compound')
  *    
  * A language definition can provide further clauses that
  * define supertypes for a group of AST nodes (e.g. expressions).
  * See the definition of the Java AST for examples.
  *
  * This is a multifile predicate. In case of differences, its 
  * autoritative documentation is the one in the file 
  * languageIndependent.pl.
  */ 

:- multifile ast_node_subtype/3.
 
ast_node_subtype('Java',Label,expressionType) :- 
    expression_type('Java',Label).

ast_node_subtype('Java',Label,annotationExpressionType) :- 
    annotation_expression_type('Java',Label).
    
ast_node_subtype('Java',Label,statementType) :- 
    statement_type('Java',Label).


 /**
  * ast_node_def(?Language, ?AstNodeLabel, ?AstNodeArguments)
  *
  * The Abstract Syntax Tree (AST) for the language arg1 contains 
  * nodes with label (functor) arg2 and arguments arg3. 
  *
  * Node arguments are described by a list of terms of the form
  *    ast_arg(ArgName, Cardinality, IdOrAttribute, Types)
  * where 
  *    ArgName is the name of the argument (usually an atom)
  *    Cardinality is either 
  *        SCHEMA   | EXAMPLE      | EXPLANATION
  *        ---------+--------------+------------------------------------
  *        *        | mult(0,*,no) | Any cardinality, including 0, no order.
  *        list     | mult(0,*,ord)| Any cardinality, including 0, the arguments are ordered.
  *                 |              | In the JT2 representation this argument is a list.
  *        From-To  | mult(1,2,no) | A cardinality range with the lower bound
  *        optional | mult(0,1,no) | An optional argument - may be the atom 'null'.
  *   IsSubtree is either
  *       true  Indicates that the value is a reference to a subtree
  *       false The value is no subtree.
  *   IdOrAttribute is either
  *       id   Indicates that the value is the identity of an 
  *            AST node. By convention the first argument of any
  *            AST node is the id of that node and has ArgName 'id'.
  *            All other id arguments refer to other AST nodes. 
  *            We call them 'references'.
  *       attr Indicates that the value is not to be interpreted 
  *            as an id. It can be any legal Prolog term including
  *            compound terms.
  *   Types is a list of types defined by arg3 of ast_node_type/3. 
  *         Every value of the attribute must be from one of these
  *         types. The value 'null', legal if the cardinality includes
  *         0, is considered as an element of every types.
  *         Two contracts must be fullfilled: 
  *         The types 'typeTermType' and 'atom' must not be used in 
  *         combination with another Type for on attribute.
  *
  * Language independent bottom up traversals of an AST are supported 
  * by the convention that the second argument of every AST
  * node has the name 'parent' and refers to the parent node.
  *
  * Example (excerpt from Java AST definition):
  *
  * ast_node_def('Java', applyT,[
  *     ast_arg(id,     mult(1,1,no ), id,  [id]),              <-- convention!!!
  *     ast_arg(parent, mult(1,1,no ), id,  [id]),              <-- convention!!!
  *     ast_arg(encl,   mult(1,1,no ), id,  [methodDefT,fieldDefT]),
  *     ast_arg(expr,  mult(0,1,no), id,  [expressionType]),
  *     ast_arg(name,   mult(1,1,no ), attr,[atom]),
  *     ast_arg(args,   mult(1,1,no ), id,  [expressionType]),
  *     ast_arg(ref,    mult(1,1,no ), id,  [methodDefT])
  * ]).
  *
  * This predicate must be defined for every language to be processed
  * by JTransformer. It is a multifile predicate. In case of 
  * differences, its autoritative documentation is the one in the file 
  * languageIndependent.pl.
  */
  
 /* ********************************************************************
  * TODO:
  *
  * Ich denke, wir sollten 'atom' durch etwas programmiersprachen-
  * spezifischeres ersetzen (oder erg�nzen), wie z.B. 'identifier'. -- GK
  */

/**
 * ast_arg_id_kind(Name)
 * 
 * The name of all ast arguments
 * of type id.
 * E.g. parent, encl, body, ...
 */
:- dynamic ast_arg_id_kind/1.


% ****************** Interface PEF ******************************

:- multifile ast_node_def/3.

% tree_constraints(packageT ,[[atom]]).
ast_node_def('Java',packageT,[
     ast_arg(id,      mult(1,1,no ),  id,  [id]), % <-- convention!!!
     ast_arg(name,    mult(1,1,no ),  attr,  [atom])
]).

%tree_constraints(projectLocationT ,[[projectT],[atom],[atom],[atom]]).
ast_node_def('Java',projectT,[
     ast_arg(id,   mult(1,1,no ), id,   [projectT]),
     ast_arg(file, mult(1,1,no ), attr, [atom]),
     ast_arg(name, mult(1,1,no ), attr, [atom]),
     ast_arg(file, mult(1,1,no ), attr, [atom])
]).


% tree_constraints(classDefT ,[[execT,packageT,classDefT,newClassT,blockT,nullType],[atom],[methodDefT,fieldDefT,classDefT]]).
ast_node_def('Java',classDefT,[
     ast_arg(id,      mult(1,1,no ), id,  [classDefT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [execT,packageT, classDefT, newClassT, blockT]), 
     ast_arg(name,    mult(1,1,no ), attr,[atom]),
     ast_arg(defs,    mult(0,*,ord), id,  [methodDefT,fieldDefT,classDefT])
%     ast_arg(expr,   mult(1,1,no ), id,  [expressionType])
%     ast_arg(extends, mult(1,1,no ), id,  [classDefT]),
%     ast_arg(implems, mult(0,*,no ), id,  [classDefT]),
%     ast_arg(hasModif,mult(0,*,no ), attr,[atom]),
%     ast_arg(isInterf,mult(0,1,no ),  flag,[]),
%     ast_arg(isExtern,mult(0,1,no ),  flag,[])
]).

% tree_constraints(methodDefT ,[[classDefT],[atom],[paramDefT],[typeTermType,nullType],[classDefT],[blockT,nullType]]).
ast_node_def('Java',methodDefT,[
     ast_arg(id,      mult(1,1,no ), id,  [methodDefT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [classDefT]), 
     ast_arg(name,    mult(1,1,no ), attr,[atom]),
     ast_arg(params,  mult(0,*,ord), id,  [paramDefT]),
     ast_arg(type,    mult(0,1,no),  attr,[typeTermType,nullType]),  % 0, if constructor definition
     ast_arg(excepts, mult(0,*,ord), id,  [classDefT]), %ord, only for compatibility with current PEF represenation: detection of lists
     ast_arg(body,    mult(0,1,no),  id,  [blockT])
%     ast_arg(hasModif,mult(0,*,no),  attr,[atom])     
]).

% tree_constraints(fieldDefT ,[[classDefT],[typeTermType],[atom],[expressionType,nullType]]).
ast_node_def('Java',fieldDefT,[
     ast_arg(id,      mult(1,1,no ), id,  [fieldDefT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [classDefT]),
     ast_arg(type,    mult(1,1,no ), attr,[typeTermType]), % <-- dieser typ ist noch undefined
     ast_arg(name,    mult(1,1,no ), attr,[atom]),
     ast_arg(expr,    mult(0,1,no ), id,  [expressionType])
%     ast_arg(hasModif,mult(0,*,no ), attr,[atom])
]).

% tree_constraints(paramDefT ,[[methodDefT,catchT],[typeTermType],[atom]]).
ast_node_def('Java',paramDefT,[
     ast_arg(id,      mult(1,1,no ),  id,  [paramDefT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ),  id,  [methodDefT,catchT]), 
     ast_arg(type,    mult(1,1,no ),  attr,[typeTermType]), 
     ast_arg(name,    mult(1,1,no ),  attr,[atom])
%     ast_arg(hasModif,mult(0,*,no ),  attr,[atom])     
]).

% ****************** Body PEF ******************************

% tree_constraints(applyT ,[[allType],[methodDefT,fieldDefT],[expressionType,nullType],[atom],[expressionType],[methodDefT]]).
ast_node_def('Java', applyT,[
     ast_arg(id,      mult(1,1,no ), id,  [applyT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT]),
     ast_arg(expr,    mult(0,1,no), id,  [expressionType]),
     ast_arg(name,    mult(1,1,no ), attr,[atom]),
     ast_arg(args,    mult(0,*,ord ), id,  [expressionType]),
     ast_arg(ref,     mult(1,1,no ), id,  [methodDefT])
 ]). 
% tree_constraints(assertT ,[[allType],[methodDefT],[expressionType]]).
ast_node_def('Java',assertT,[
     ast_arg(id,      mult(1,1,no ), id,  [assertT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(expr,    mult(0,1,no), id,  [expressionType]),
     ast_arg(msg,     mult(0,1,no), id,  [expressionType])
]).
% tree_constraints(assignopT,[[allType],[methodDefT,fieldDefT],[getFieldT,identT,indexedT],[atom],[expressionType]]).
ast_node_def('Java',assignT,[
     ast_arg(id,      mult(1,1,no ), id,  [assignT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT]),
     ast_arg(lhs,     mult(1,1,no ), id,  [getFieldT,identT,indexedT]),
     ast_arg(expr,    mult(1,1,no ), id,  [expressionType])
]).
% tree_constraints(assignT,[[allType],[methodDefT,fieldDefT],[getFieldT,identT,indexedT],[expressionType]]).
ast_node_def('Java',assignopT,[
     ast_arg(id,      mult(1,1,no ), id,  [assignopT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT]),
     ast_arg(lhs,     mult(1,1,no ), id,  [getFieldT,identT,indexedT]),
     ast_arg(operator,mult(1,1,no ), attr,[atom]),
     ast_arg(expr,    mult(1,1,no ), id,  [expressionType])
]).
% tree_constraints(blockT, [[allType],[methodDefT],[statementType]]).
ast_node_def('Java',blockT,[
     ast_arg(id,      mult(1,1,no ), id,  [blockT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(stmts,   mult(0,*,ord), id,[statementType]) %  **** xxx
]).               % ^^^ <-- Blocks duerfen doch auch leer sein, oder?
                % die Semantik von * habe ich zuerst anders verstanden
                % Zusammenfassend:
                % 0-*: ist gleichbedeutend mit *
                
% tree_constraints(breakT,[[allType],[methodDefT],[atom],[statementType]]).
ast_node_def('Java',breakT,[
     ast_arg(id,      mult(1,1,no ), id,  [breakT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(label,   mult(1,1,no ), attr,[atom]),
     ast_arg(target,  mult(0,1,no), id,  [statementType])
]).
% tree_constraints(caseT,[[allType],[methodDefT],[expressionType,nullType]]).
ast_node_def('Java',caseT,[
     ast_arg(id,      mult(1,1,no ), id,  [caseT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(expr,    mult(0,1,no), id,  [expressionType])
]).

% tree_constraints(conditionalT,[[allType],[methodDefT,fieldDefT],[expressionType],[expressionType],[expressionType]]).
ast_node_def('Java',conditionalT,[
     ast_arg(id,      mult(1,1,no ), id,  [conditionalT]),
     ast_arg(parent,  mult(1,1,no ), id,  [id]),
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT,classDefT]),
     ast_arg(cond,    mult(1,1,no ), id,  [expressionType]),
     ast_arg(thenexpr,mult(1,1,no ), id,  [expressionType]),
     ast_arg(elseexpr,mult(0,1,no), id,  [expressionType])
]).
% tree_constraints(continueT,[[allType],[methodDefT],[atom],[nullType,statementType]]).
ast_node_def('Java',continueT,[
     ast_arg(id,      mult(1,1,no ), id,  [continueT]),
     ast_arg(parent,  mult(1,1,no ), id,  [id]),
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(label,   mult(1,1,no ), attr,  [atom]),
     ast_arg(target,  mult(0,1,no), id,  [statementType])
]).
% tree_constraints(doLoopT,[[allType],[methodDefT],[expressionType],[statementType]]).
ast_node_def('Java',doLoopT,[
     ast_arg(id,      mult(1,1,no ), id,  [doLoopT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(cond,    mult(1,1,no ), id,  [expressionType]),
     ast_arg(body,    mult(1,1,no ), id,  [statementType])     
]).
% tree_constraints(execT,[[allType],[methodDefT],[expressionType,classDefT]]).
ast_node_def('Java',execT,[
     ast_arg(id,      mult(1,1,no ), id,  [execT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(expr,    mult(1,1,no ), id,  [expressionType])
]).

% tree_constraints(catchT,[[allType],[methodDefT],[paramDefT],[blockT]]).
ast_node_def('Java',catchT,[
     ast_arg(id,      mult(1,1,no ), id,  [catchT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(param,   mult(1,1,no ), id,  [paramDefT]),
     ast_arg(body,    mult(1,1,no ), id,  [blockT])
]).

% tree_constraints(forLoopT,[[allType],[methodDefT],[expressionType,nullType,localDefT],[expressionType,nullType],[expressionType,nullType],[statementType]]).
ast_node_def('Java',forLoopT,[
     ast_arg(id,      mult(1,1,no ), id,  [forLoopT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(inits,   mult(0,*,ord), id,  [expressionType,localDefT]), % <-- TODO: statementexpr: INC,DEC,ASSIGN, CALL, NEWCLASS
     ast_arg(cond,    mult(0,1,no), id,   [expressionType]),
     ast_arg(updaters,mult(0,*,ord), id,  [expressionType]),
     ast_arg(body,    mult(1,1,no ), id,  [blockT])
]).

% tree_constraints(getFieldT, [[allType],[methodDefT,fieldDefT],[expressionType,nullType],[atom], [fieldDefT,nullType]]). % if it is the length field of an array
ast_node_def('Java',getFieldT,[
     ast_arg(id,      mult(1,1,no ), id,  [getFieldT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT]),
     ast_arg(expr,    mult(0,1,no), id,  [expressionType]),
     ast_arg(name,    mult(0,1,no), attr,  [atom]),
     ast_arg(field,   mult(1,1,no ), id,  [fieldDefT])
]).

% tree_constraints(ifT,[[allType],[methodDefT],[expressionType],[blockT,statementType],[blockT,statementType,nullType]]).
ast_node_def('Java',ifT,[
     ast_arg(id,      mult(1,1,no ), id,  [ifT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(cond,    mult(1,1,no ), id,  [expressionType]),
     ast_arg(then,    mult(1,1,no ), id,  [blockT,statementType]),
     ast_arg(else,    mult(0,1,no), id,  [blockT,statementType])
]).

% tree_constraints(importT ,[[toplevelT],[atom,classDefT]]).
ast_node_def('Java',importT,[
     ast_arg(id,      mult(1,1,no ), id,  [importT]), 
     ast_arg(toplevel,mult(1,1,no ), id,  [toplevelT]),
     ast_arg(import,  mult(1,1,no ), id,  [packageT,classDefT])
]).
% tree_constraints(indexedT,[[allType],[methodDefT,fieldDefT],[expressionType],[expressionType]]).
ast_node_def('Java',indexedT,[
     ast_arg(id,      mult(1,1,no ), id,  [indexedT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT]),
     ast_arg(index,   mult(1,1,no ), id,  [expressionType]),
     ast_arg(indexed, mult(1,1,no ), id,  [expressionType])
]).
% tree_constraints(labelT,[[allType],[methodDefT,fieldDefT],[statementType],[atom]]).
ast_node_def('Java',labelT,[
     ast_arg(id,      mult(1,1,no ), id,  [labelT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT]),
     ast_arg(body,    mult(1,1,no ), id,  [statementType]),
     ast_arg(label,   mult(1,1,no ), attr,  [atom])
]).
% tree_constraints(literalT ,[[allType],[methodDefT,fieldDefT],[typeTermType],[atom]]).
ast_node_def('Java',literalT,[
     ast_arg(id,      mult(1,1,no ), id,  [literalT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT,classDefT]),
     ast_arg(type,    mult(1,1,no ), attr,  [typeTermType]),
     ast_arg(value,   mult(1,1,no ), attr,  [atom]) % TODO: hier stand vorher: atom,typeTermType, type literals are now represented as selectT / identTs selectT(....,class,ident(...,Classname,type(class,java.lang.Class,.))
]).

% tree_constraints(localDefT,[[allType],[methodDefT],[typeTermType],[atom],[expressionType,nullType]]).
ast_node_def('Java',localDefT,[
     ast_arg(id,      mult(1,1,no ), id,  [localDefT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- methodDefT, blockT !?!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT]),
     ast_arg(type,    mult(1,1,no ), attr,[typeTermType]), % <-- dieser typ ist noch undefined
     ast_arg(name,    mult(1,1,no ), attr,[atom]),
     ast_arg(expr,    mult(0,1,no), id,  [expressionType])
]).


% tree_constraints(newArrayT, [[allType],[methodDefT,fieldDefT],[expressionType],[expressionType],[typeTermType]]).
ast_node_def('Java',newArrayT,[
     ast_arg(id,      mult(1,1,no ), id,  [newArrayT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT,classDefT]),
     ast_arg(dims,    mult(1,*,ord), id,  [expressionType]),
     ast_arg(elems,   mult(0,*,ord), id,  [expressionType]), % <-- elems wird momentan im FactGenerator immer auf [] gesetzt, warum???
     ast_arg(type,    mult(1,1,no ), attr,  [typeTermType])
]).

% tree_constraints(newClassT, [[allType],[methodDefT,fieldDefT],[methodDefT,nullType],[expressionType],[identT,selectT],[classDefT,nullType],[classDefT,nullType]]).
ast_node_def('Java',newClassT,[
     ast_arg(id,      mult(1,1,no ), id,  [newClassT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT]),
     ast_arg(constr,  mult(0,1,no), id,  [methodDefT]),
     ast_arg(args,    mult(0,*,ord), id,  [expressionType]),
     ast_arg(clident, mult(0,1,no), id,  [identT,selectT]),
     ast_arg(def,     mult(0,1,no), id,  [classDefT]),
     ast_arg(encltype,mult(0,1,no), id, [classDefT])
]).

% tree_constraints(nopT,[[allType],[methodDefT]]).
ast_node_def('Java',nopT,[
     ast_arg(id,     mult(1,1,no ), id,  [nopT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id,  [methodDefT])
]).

% tree_constraints(operationT,[[allType],[methodDefT,fieldDefT],[expressionType],[atom],[atom]]).
ast_node_def('Java',operationT,[
     ast_arg(id,     mult(1,1,no ), id,  [operationT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id,  [methodDefT,fieldDefT,classDefT]), % classDefT if used in an memberValueT
     ast_arg(args,   mult(1,1,ord), id,  [expressionType]),
     ast_arg(op,     mult(1,1,no ), attr,  [atom]),
     ast_arg(pos,    mult(1,1,no ), id,  [number])
]).

% tree_constraints(precedenceT,[[allType],[methodDefT,fieldDefT],[expressionType]]).
ast_node_def('Java',precedenceT,[
     ast_arg(id,     mult(1,1,no ), id,  [precedenceT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id,  [methodDefT,fieldDefT]),
     ast_arg(expr,   mult(1,1,no ), id,  [expressionType])
]).

% tree_constraints(returnT,[[allType],[methodDefT],[expressionType,nullType]]).
ast_node_def('Java',returnT,[
     ast_arg(id,     mult(1,1,no ), id,  [execT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id,  [methodDefT]),
     ast_arg(expr,   mult(1,1,no ), id,  [expressionType])
]).

% tree_constraints(selectT, [[allType], [methodDefT,fieldDefT], [atom],[selectT,identT],[classDefT,packageT,typeTermType]]).
ast_node_def('Java',selectT,[
     ast_arg(id,      mult(1,1,no ), id,  [selectT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT]),
     ast_arg(name,    mult(1,1,no ), attr,  [atom]),
     ast_arg(selected,mult(1,1,no ), id,  [selectT,identT]),
     ast_arg(ref,     mult(1,1,no ), id,  [classDefT,packageT]) %%FIXME: currently also typeTermType, this breaks java_fq!
]).

% tree_constraints(identT, [[allType], [methodDefT,fieldDefT], [atom],[classDefT,packageT]]).
ast_node_def('Java',identT,[
     ast_arg(id,      mult(1,1,no ), id,  [identT]), % <-- convention!!!
     ast_arg(parent,  mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,    mult(1,1,no ), id,  [methodDefT,fieldDefT,classDefT]), % classDefT if used in an memberValueT (this will be moved to typeExprT
     ast_arg(name,    mult(1,1,no ), attr,  [atom]),
     ast_arg(ref,     mult(1,1,no ), id,  [classDefT,packageT,localDefT,paramDefT]) 
      %FIXME: currently also typeTermType, this breaks java_fq!
      %solution: introduce new PEF typeExprT(ID,PARENT,ENCL,NAME, TYPETERM).
]).

% tree_constraints(switchT,[[allType],[methodDefT],[expressionType],[statementType]]).
ast_node_def('Java',switchT,[
     ast_arg(id,     mult(1,1,no ), id,  [switchT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id,  [methodDefT]),
     ast_arg(cond,   mult(1,1,no ), id,  [expressionType]),
     ast_arg(stmts,  mult(1,1,no ), id,  [statementType]) % caseT,...,breakT,...,defaultT
]).

% tree_constraints(synchronizedT,[[allType],[methodDefT],[expressionType],[blockT]]).
ast_node_def('Java',synchronizedT,[
     ast_arg(id,     mult(1,1,no ), id,   [synchronizedT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id,   [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id,   [methodDefT]),
     ast_arg(lock,   mult(1,1,no ), id,   [expressionType]), % <-- gk, 19.09.05
     ast_arg(block,  mult(1,1,no ), id,   [blockT])
]).

% tree_constraints(throwT,[[allType],[methodDefT],[expressionType]]).
ast_node_def('Java',throwT,[
     ast_arg(id,     mult(1,1,no ), id,   [throwT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id,   [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id,   [methodDefT]),
     ast_arg(expr,   mult(1,1,no ), id,   [expressionType])
]).

% tree_constraints(toplevelT, [[packageT,nullType],[atom],[importT,classDefT]]).
ast_node_def('Java',toplevelT,[
     ast_arg(id,     mult(1,1,no ), id,   [toplevelT]), 
     ast_arg(parent, mult(0,1,no), id,   [packageT]),
     ast_arg(file,   mult(1,1,no ), attr,   [atom]),
     ast_arg(defs,   mult(0,*,ord), id,   [importT,classDefT])
]).

% tree_constraints(tryT,[[allType],[methodDefT],[blockT],[catchT],[blockT,nullType]]).
ast_node_def('Java',tryT,[
     ast_arg(id,     mult(1,1,no ), id, [tryT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id, [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id, [methodDefT]),
     ast_arg(block,  mult(1,1,no ), id, [blockT]),
     ast_arg(catchers,mult(1,*,ord), id, [catchT]),
     ast_arg(finalize,mult(0,1,no), id, [blockT])
]).

% tree_constraints(typeCastT,[[allType],[methodDefT,fieldDefT],[typeTermType],[expressionType]]).
ast_node_def('Java',typeCastT,[
     ast_arg(id,     mult(1,1,no ), id,   [typeCastT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id,   [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id,   [methodDefT,fieldDefT]),
     ast_arg(type,   mult(1,1,no ), attr, [typeTermType]),
     ast_arg(expr,   mult(1,1,no ), id,   [expressionType])
]).

% tree_constraints(typeTestT,[[allType],[methodDefT,fieldDefT],[typeTermType],[expressionType]]).
ast_node_def('Java',typeTestT,[    
     ast_arg(id,     mult(1,1,no ), id,   [typeTestT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id,   [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id,   [methodDefT]),
     ast_arg(type,   mult(1,1,no ), attr, [typeTermType]),   % expr instanceof type
     ast_arg(expr,   mult(1,1,no ), id,   [expressionType])
]).

% tree_constraints(whileLoopT,[[allType],[methodDefT],[expressionType],[statementType]]).
ast_node_def('Java',whileLoopT,[
     ast_arg(id,     mult(1,1,no ), id,   [whileLoopT]), % <-- convention!!!
     ast_arg(parent, mult(1,1,no ), id,   [id]), % <-- convention!!!
     ast_arg(encl,   mult(1,1,no ), id,   [methodDefT]),
     ast_arg(cond,   mult(1,1,no ), id,   [expressionType]),
     ast_arg(body,   mult(1,1,no ), id,   [statementType])
]).

%tree_constraints(projectLocationT ,[[toplevelT],[projectT],[atom]]).
ast_node_def('Java',projectT,[
     ast_arg(id,    mult(1,1,no ), id,   [toplevelT]),
     ast_arg(path,  mult(1,1,no ), attr, [atom]),
     ast_arg(id,    mult(1,1,no ), id,   [projectT]),
     ast_arg(path,  mult(1,1,no ), attr, [atom])
]).


%tree_constraints(extendsT,[[],[classDefT]]).
ast_node_def('JavaAttributes',extendsT,[
     ast_arg(sub,     mult(1,1,no ), id,   [classDefT]), 
     ast_arg(super, mult(1,1,no ), id,   [classDefT]) 
]).

%tree_constraints(modifierT,[[atomType]]).
ast_node_def('JavaAttributes',modifierT,[
     ast_arg(id,     mult(1,1,no ), id,   [classDefT,methodDefT,fieldDefT]),
     ast_arg(modifier, mult(1,1,no ), attr, [atom]) 
]).
%tree_constraints(implementsT,[[],[classDefT]]).
ast_node_def('JavaAttributes',implementsT,[
     ast_arg(sub,     mult(1,1,no ), id,   [classDefT]),
     ast_arg(super, mult(1,1,no ), id,   [classDefT]) 
]).
%tree_constraints(externT,[[]]).
ast_node_def('JavaAttributes',externT,[
     ast_arg(id,     mult(1,1,no ), id,   [classDefT]) 
]).
%tree_constraints(interfaceT ,[[]]).
ast_node_def('JavaAttributes',interfaceT,[
     ast_arg(id,     mult(1,1,no ), id,   [classDefT]) 
]).

ast_node_def('JavaAttributes',omitArrayDeclarationT,[
     ast_arg(id,     mult(1,1,no ), id,   [newArrayT]) 
]).

%tree_constraints(interfaceT ,[[]]).
ast_node_def('JavaAttributes',slT,[
     ast_arg(id,     mult(1,1,no ), id,   [id]),
     ast_arg(pos, mult(1,1,no ), attr, [atom]),
     ast_arg(length, mult(1,1,no ), attr, [atom])
]).

ast_node_def('JavaAttributes',sl_argT,[
     ast_arg(id,     mult(1,1,no ), id,   [id]),
     ast_arg(kind, mult(1,1,no ), attr, [atom]),
     ast_arg(pos, mult(1,1,no ), attr, [atom]),
     ast_arg(length, mult(1,1,no ), attr, [atom])
]).

ast_node_def('JavaAttributes',projectLocationT,[
     ast_arg(id,   mult(1,1,no ), id,   [projectT]),
     ast_arg(name, mult(1,1,no ), attr, [atom]),
     ast_arg(file, mult(1,1,no ), attr, [atom])
]).

/*********** JAVA 5 *************/

/*********** Foreach ***************/
ast_node_def('Java',foreachT,[
     ast_arg(id,          mult(1,1,no ), id,  [foreachT]), % <-- convention!!!
     ast_arg(parent,      mult(1,1,no ), id,  [id]), % <-- convention!!!
     ast_arg(encl,        mult(1,1,no ), id,  [methodDefT]),
     ast_arg(init,        mult(1,1,no),  id,  [localDefT]), 
     ast_arg(expression,  mult(1,1,no),  id,  [expressionType]),
     ast_arg(body,        mult(1,1,no ), id,  [blockT])
]).

/*********** Enum ***************/

ast_node_def('JavaAttributes',enumT,[
     ast_arg(id,     mult(1,1,no ), id,   [classDefT]) 
]).

ast_node_def('Java',enumConstantT,[
     ast_arg(id,     mult(1,1,no ),  id,   [enumConstantT]),
     ast_arg(parent, mult(1,1,no ),  id,   [enumT]),  
     ast_arg(encl,   mult(1,1,no ),  id,   [id]),   
     ast_arg(name,   mult(1,1,no ),  attr, [atom]),
     ast_arg(args,   mult(0,*,ord ), id,   [expressionType])
]).

/*********** Annotations JSR-175 ***************/

%tree_constraints(annotationTypeT ,[[classDefT]]).
ast_node_def('JavaAttributes',annotationTypeT,[
     ast_arg(id,     mult(1,1,no ), id,   [classDefT])
]).

%tree_constraints(markerAnnotationT ,[[#id]).
ast_node_def('JavaAttributes',markerAnnotationT,[
     ast_arg(id,     mult(1,1,no ), id,   [annotationT])
]).

ast_node_def('JavaAttributes',annotatedT,[
     ast_arg(id,     mult(1,1,no ), id,   [id]),
     ast_arg(annotation,     mult(1,1,no ), id,   [annotationT])
]).

ast_node_def('Java',annotationT,[
     ast_arg(id,     mult(1,1,no ), id,       [id]),
     ast_arg(parent, mult(1,1,no ), id,       [id]),
     ast_arg(encl,   mult(1,1,no ), id,       [id]),
     ast_arg(annotationType, mult(1,1,no ),id,[classDefT]),
     ast_arg(values, mult(0,*,ord ),id,       [memberValueT]) % <- a general redundancy problem of our current AST
]).

ast_node_def('Java',memberValueT,[
     ast_arg(id,     mult(1,1,no ), id,   [memberValueT]),
     ast_arg(parent, mult(1,1,no ), id,   [annotationT]), 
     ast_arg(member, mult(1,1,no ), id,   [annotationMemberT]),
     ast_arg(value,  mult(1,1,no ), id,   [nullType, annotationExpressionType])
]).

ast_node_def('Java',annotationMemberT,[
     ast_arg(id,     mult(1,1,no ), id,   [id]),
     ast_arg(parent, mult(1,1,no ), id,   [id]), 
     ast_arg(type,   mult(1,1,no ), attr, [typeTermType]),
     ast_arg(name,   mult(1,1,no ), attr, [atom]),
     ast_arg(default,mult(1,1,no ), id, [annotationExpressionType,nullType])
]).

/*********** Generics JSR-014 ***************/

% TODO

/**
 * create_ast_arg_id_kind
 * 
 * First retract and then create ast_arg_id_kind
 * facts from the ast_node_def clauses.
 */
create_ast_arg_id_kind :-
    retractall(ast_arg_id_kind(_)),
	setof(Kind, all_ast_arg_id_kind(Kind),Kinds),
	forall(member(Kind,Kinds),assert(ast_arg_id_kind(Kind))).
	
all_ast_arg_id_kind(Kind):-
    ast_node_def('Java',_,Defs),
    member(ast_arg(Kind,  mult(1,1,no ), id,  _),Defs).

:- create_ast_arg_id_kind.

checkSyntax(Name) :- 
	attribSignature(Name,SigLen),
	not(ast_node_def('JavaAttributes',Name,List)).
checkSyntax(Name) :- 
	not(ast_node_def('JavaAttributes',Name,List)),
	attribSignature(Name,SigLen).
checkSyntax(Name) :- 
	treeSignature(Name,SigLen),
	not(ast_node_def('Java',Name,List)).
checkSyntax(Name) :- 
	not(ast_node_def('Java',Name,List)),
	treeSignature(Name,SigLen).
checkSyntax(Name) :- 
	ast_node_def('Java',Name,List),
	treeSignature(Name,SigLen),
	not(length(List,SigLen)).
checkSyntax(Name) :- 
	ast_node_def('JavaAttributes',Name,List),
	attribSignature(Name,SigLen),
	not(length(List,SigLen)).
checkSyntax(Name) :- 
	clause(
	  ast_node_def('JavaAttributes',Name,List),
	  _,
	  Ref),
	clause(
	  ast_node_def('JavaAttributes',Name,List2),
	  _,
	  Ref2),
	not(Ref = Ref2).		
