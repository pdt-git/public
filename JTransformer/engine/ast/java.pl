% Autor: Tobias Windeln & Guenter Kniesel
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
  
ast_sub_tree('Java',expr).
ast_sub_tree('Java',msg).
ast_sub_tree('Java',args).
ast_sub_tree('Java',param).
ast_sub_tree('Java',lhs).
ast_sub_tree('Java',stmts).
ast_sub_tree('Java',target).
ast_sub_tree('Java',body).


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

ast_reference_type('Java', Label) :-
	expression_type('Java',Label).
ast_reference_type('Java',Label) :- 
	statement_type('Java',Label).

 % expression_type needs the first argument since it is an
 % abstraction (from file languageAbstractions.pl).

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

 % same comments as for expression_type_ above
 
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
   
ast_node_subtype('Java',Label,expressionType) :- 
	expression_type('Java',Label).
	
ast_node_subtype('Java',Label,statementType) :- 
	statement_type('Java','Java',Label).


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
  *        SCHEMA  | EXAMPLE | EXPLANATION
  *        --------+---------+------------------------------------
  *        *       | *       | Any cardinality, including 0.
  *        From-To | 0-1     | A cardinality range with the lower bound
  *                |         | From and the uper bound To (inclusive).
  *        Number  | 1       | Any positive integer denoting a fixed 
  *                |         | number of values (most often 1). 
  *       The cardinality 0 indicates that the value may be 'null'.
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
  *
  * Language independent bottom up traversals of an AST are supported 
  * by the convention that the second argument of every AST
  * node has the name 'parent' and refers to the parent node.
  *
  * Example (excerpt from Java AST definition):
  *
  * ast_node_def('Java', applyT,[
  *     ast_arg(id,      1, id,  [id]),              <-- convention!!!
  *     ast_arg(parent,  1, id,  [id]),              <-- convention!!!
  *     ast_arg(encl,    1, id,  [methodDefT,fieldDefT]),
  *     ast_arg(expr,  0-1, id,  [expressionType]),
  *     ast_arg(name,    1, attr,[atom]),
  *     ast_arg(args,    1, id,  [expressionType]),
  *     ast_arg(ref,     1, id,  [methodDefT])
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
  * In ast_node_def/3 vorhandenes noch mal durchdenken, checken hinsichtlich
  * Konsistenz mit bisherigen tree_constraints und tree_atrributes.
  *
  * Den fehlenden Rest ergänzen. Dabei muss überall wo es einen Modifier
  * geben darf (methodDefT, fieldDefT, localDefT, ...?... HINTER den
  * Beschreibungen der bisherigen Argumente noch folgendes hinzu:
  *    ast_arg(modifs, *, attr, [atom])
  * Für classDefT ist das schon geschehen. 
  *
  * Die parent und encl attribute im Folgenden könnten eigentlich oft 
  * genauer getypt sein als nur 'id'.
  *
  * Ich denke, wir sollten 'atom' durch etwas programmiersprachen-
  * spezifischeres ersetzen (oder ergänzen), wie z.B. 'identifier'.
  */


% ****************** Interface PEF ******************************


% tree_constraints(packageT ,[[atom]]).
ast_node_def('Java',packageT,[
     ast_arg(id,      1,  id,  [id]), % <-- convention!!!
     ast_arg(name,    1,  id,  [atom])
]).

% tree_constraints(classDefT ,[[execT,packageT,classDefT,newClassT,blockT,nullType],[atom],[methodDefT,fieldDefT,classDefT]]).
ast_node_def('Java',classDefT,[
     ast_arg(id,      1,  id,  [id]), % <-- convention!!!
     ast_arg(parent,  1,  id,  [execT,packageT, classDefT, newClassT, blockT]), 
     ast_arg(name,    1,  attr,[atom]),
     ast_arg(defs,    *,  id,  [methodDefT,fieldDefT,classDefT]),
     ast_arg(expr,    1,  id,  [expressionType]),
     ast_arg(extends, 1,  id,  [classDefT]),
     ast_arg(implems, *,  id,  [classDefT]),
     ast_arg(hasModif,*,  attr,[atom]),
     ast_arg(isInterf,0-1,flag,[]),
     ast_arg(isExtern,0-1,flag,[])
]).

% tree_constraints(methodDefT ,[[classDefT],[atom],[paramDefT],[typeTermType,nullType],[classDefT],[blockT,nullType]]).
ast_node_def('Java',methodDefT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [classDefT]), 
     ast_arg(name,    1, attr,[atom]),
     ast_arg(params,  *, id,  [paramDefT]),
     ast_arg(type,  0-1, attr,[typeTermType]), 
     ast_arg(excepts, *, id,  [classDefT]),
     ast_arg(body,  0-1, id,  [blockT]),
     ast_arg(hasModif,*, attr,[atom])     
]).

% tree_constraints(fieldDefT ,[[classDefT],[typeTermType],[atom],[expressionType,nullType]]).
ast_node_def('Java',fieldDefT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [classDefT]), % <-- wieso auch 0?
     ast_arg(type,    1, attr,[typeTermType]), % <-- dieser typ ist noch undefined
     ast_arg(name,    1, attr,[atom]),
     ast_arg(expr,  0-1, id,  [expressionType]),
     ast_arg(hasModif,*,  attr,[atom])
]).

% tree_constraints(paramDefT ,[[methodDefT,catchT],[typeTermType],[atom]]).
ast_node_def('Java',paramDefT,[
     ast_arg(id,      1,  id,  [id]), % <-- convention!!!
     ast_arg(parent,  1,  id,  [methodDefT,catchT]), 
     ast_arg(name,    1,  id,  [atom]), 
     ast_arg(type,    1,  id,  [typeTermType])     
]).

% ****************** Body PEF ******************************

% tree_constraints(applyT ,[[allType],[methodDefT,fieldDefT],[expressionType,nullType],[atom],[expressionType],[methodDefT]]).
ast_node_def('Java', applyT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT,fieldDefT]),
     ast_arg(expr,  0-1, id,  [expressionType]),
     ast_arg(name,    1, attr,[atom]),
     ast_arg(args,    1, id,  [expressionType]),
     ast_arg(ref,     1, id,  [methodDefT])
 ]). 
% tree_constraints(assertT ,[[allType],[methodDefT],[expressionType]]).
ast_node_def('Java',assertT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(expr,  0-1, id,  [expressionType]),
     ast_arg(msg,   0-1, id,  [expressionType])
]).
% tree_constraints(assignopT,[[allType],[methodDefT,fieldDefT],[getFieldT,identT,indexedT],[atom],[expressionType]]).
ast_node_def('Java',assignT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT,fieldDefT]),
     ast_arg(lhs,     1, id,  [getFieldT,identT,indexedT]),
     ast_arg(expr,    1, id,  [expressionType])
]).
% tree_constraints(assignT,[[allType],[methodDefT,fieldDefT],[getFieldT,identT,indexedT],[expressionType]]).
ast_node_def('Java',assignopT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT,fieldDefT]),
     ast_arg(lhs,     1, id,  [getFieldT,identT,indexedT]),
     ast_arg(operator,1, attr,[atom]),
     ast_arg(expr,    1, id,  [expressionType])
]).
% tree_constraints(blockT, [[allType],[methodDefT],[statementType]]).
ast_node_def('Java',blockT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(stmts, 0-*, id,  [statementType])
]).                % ^^^ <-- Blocks duerfen doch auch leer sein, oder?
				% die Semantik von * habe ich zuerst anders verstanden
				% Zusammenfassend:
				% 0-*: bedeutet geordnete Liste von Argumenten (defs,stmts)
				% *  : beliebige Anzahl ungeordneter Argumente (hasModif)
				
% tree_constraints(breakT,[[allType],[methodDefT],[atom],[statementType]]).
ast_node_def('Java',breakT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(label,   1, attr,[atom]),
     ast_arg(target,0-1, id,  [statementType])
]).
% tree_constraints(caseT,[[allType],[methodDefT],[expressionType,nullType]]).
ast_node_def('Java',caseT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(expr,  0-1, id,  [expressionType])
]).

% tree_constraints(conditionalT,[[allType],[methodDefT,fieldDefT],[expressionType],[expressionType],[expressionType]]).
ast_node_def('Java',conditionalT,[
     ast_arg(id,      1, id,  [id]),
     ast_arg(parent,  1, id,  [id]),
     ast_arg(encl,    1, id,  [methodDefT,fieldDefT]),
     ast_arg(cond,    1, id,  [expressionType]),
     ast_arg(thenexpr,1, id,  [expressionType]),
     ast_arg(elseexpr,1, id,  [expressionType])
]).
% tree_constraints(continueT,[[allType],[methodDefT],[atom],[nullType,statementType]]).
ast_node_def('Java',continueT,[
     ast_arg(id,      1, id,  [id]),
     ast_arg(parent,  1, id,  [id]),
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(label,   1, id,  [atom]),
     ast_arg(target,0-1, id,  [statementType])
]).
% tree_constraints(doLoopT,[[allType],[methodDefT],[expressionType],[statementType]]).
ast_node_def('Java',doLoopT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(cond,    1, id,  [expressionType]),
     ast_arg(body,    1, id,  [statementType])     
]).
% tree_constraints(execT,[[allType],[methodDefT],[expressionType,classDefT]]).
ast_node_def('Java',execT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(expr,    1, id,  [expressionType])
]).

% tree_constraints(catchT,[[allType],[methodDefT],[paramDefT],[blockT]]).
ast_node_def('Java',catchT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(param,   1, id,  [paramDefT]),
     ast_arg(body,    1, id,  [blockT])
]).

% tree_constraints(forLoopT,[[allType],[methodDefT],[expressionType,nullType,localDefT],[expressionType,nullType],[expressionType,nullType],[statementType]]).
ast_node_def('Java',forLoopT,[
     ast_arg(id,        1, id,  [id]), % <-- convention!!!
     ast_arg(parent,    1, id,  [id]), % <-- convention!!!
     ast_arg(encl,      1, id,  [methodDefT]),
     ast_arg(inits,   0-*, id,  [[expressionType,localDefT]]),
     ast_arg(cond,    0-1, id,  [[expressionType]]),
     ast_arg(updaters,0-*, id,  [[expressionType]])
]).

% tree_constraints(getFieldT, [[allType],[methodDefT,fieldDefT],[expressionType,nullType],[atom], [fieldDefT,nullType]]). % if it is the length field of an array
ast_node_def('Java',getFieldT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT,fieldDefT]),
     ast_arg(expr,  0-1, id,  [expressionType]),
     ast_arg(name,  0-1, id,  [atom]),
     ast_arg(field,   1, id,  [fieldDefT])
]).

% tree_constraints(identT, [[allType], [methodDefT,fieldDefT], [atom], [classDefT,localDefT,paramDefT,nullType,packageT]]).
ast_node_def('Java',execT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(name,    1, id,  [atom]),
     ast_arg(ref,   0-1, id,  [classDefT,localDefT,paramDefT,packageT])
]).

% tree_constraints(ifT,[[allType],[methodDefT],[expressionType],[blockT,statementType],[blockT,statementType,nullType]]).
ast_node_def('Java',ifT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(cond,    1, id,  [expressionType]),
     ast_arg(then,    1, id,  [blockT,statementType]),
     ast_arg(else,  0-1, id,  [blockT,statementType])
]).

% tree_constraints(importT ,[[toplevelT],[atom,classDefT]]).
ast_node_def('Java',importT,[
     ast_arg(id,      1, id,  [id]), 
     ast_arg(toplevel,1, id,  [toplevelT]),
     ast_arg(import,  1, id,  [atom,classDefT])
]).
% tree_constraints(indexedT,[[allType],[methodDefT,fieldDefT],[expressionType],[expressionType]]).
ast_node_def('Java',indexedT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT,fieldDefT]),
     ast_arg(index,   1, id,  [expressionType]),
     ast_arg(indexed, 1, id,  [expressionType])
]).
% tree_constraints(labelT,[[allType],[methodDefT,fieldDefT],[statementType],[atom]]).
ast_node_def('Java',labelT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT,fieldDefT]),
     ast_arg(body,    1, id,  [statementType]),
     ast_arg(label,   1, id,  [atom])
]).
% tree_constraints(literalT ,[[allType],[methodDefT,fieldDefT],[typeTermType],[atom,typeTermType]]).
ast_node_def('Java',literalT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- convention!!!
     ast_arg(encl,    1, id,  [methodDefT,fieldDefT]),
     ast_arg(type,    1, id,  [typeTermType]),
     ast_arg(value,   1, id,  [atom,typeTermType])
]).

% tree_constraints(localDefT,[[allType],[methodDefT],[typeTermType],[atom],[expressionType,nullType]]).
ast_node_def('Java',localDefT,[
     ast_arg(id,      1, id,  [id]), % <-- convention!!!
     ast_arg(parent,  1, id,  [id]), % <-- methodDefT, blockT !?!
     ast_arg(encl,    1, id,  [methodDefT]),
     ast_arg(type,    1, attr,[typeTermType]), % <-- dieser typ ist noch undefined
     ast_arg(name,    1, attr,[atom]),
     ast_arg(expr,  0-1, id,  [expressionType])
]).


% tree_constraints(newArrayT, [[allType],[methodDefT,fieldDefT],[expressionType],[expressionType],[typeTermType]]).
% tree_constraints(newClassT, [[allType],[methodDefT,fieldDefT],[methodDefT,nullType],[expressionType],[identT,selectT],[classDefT,nullType],[classDefT,nullType]]).
% tree_constraints(nopT,[[allType],[methodDefT]]).
% tree_constraints(operationT,[[allType],[methodDefT,fieldDefT],[expressionType],[atom],[atom]]).
% tree_constraints(precedenceT,[[allType],[methodDefT,fieldDefT],[expressionType]]).
% tree_constraints(returnT,[[allType],[methodDefT],[expressionType,nullType]]).
% tree_constraints(selectT, [[allType], [methodDefT,fieldDefT], [atom],[selectT,identT],[classDefT,packageT]]).
% tree_constraints(switchT,[[allType],[methodDefT],[expressionType],[statementType]]).
% tree_constraints(synchronizedT,[[allType],[methodDefT],[expressionType],[blockT]]).
% tree_constraints(throwT,[[allType],[methodDefT],[expressionType]]).
% tree_constraints(toplevelT, [[packageT,nullType],[atom],[importT,classDefT]]).
% tree_constraints(tryT,[[allType],[methodDefT],[blockT],[catchT],[blockT,nullType]]).
% tree_constraints(typeCastT,[[allType],[methodDefT,fieldDefT],[typeTermType],[expressionType]]).
% tree_constraints(typeTestT,[[allType],[methodDefT,fieldDefT],[typeTermType],[expressionType]]).
% tree_constraints(whileLoopT,[[allType],[methodDefT],[expressionType],[statementType]]).

% tree_constraints(implementsT,[[],[classDefT]]).
% tree_constraints(interfaceT ,[[]]).
% tree_constraints(modifierT,[[atom]]).
% tree_constraints(extendsT,[[],[classDefT]]).
% tree_constraints(externT,[[]]).

/*
ast_node_def('Java',localDefT, [parent]).
treeSignature(paramDefT, 4).
treeSignature(fieldDefT, 5).
treeSignature(methodDefT, 7).
treeSignature(classDefT, 4).
treeSignature(packageT, 2).
treeSignature(identT,5).
treeSignature(literalT,5).
treeSignature(execT,4).
treeSignature(operationT,6).
treeSignature(applyT,7).
treeSignature(blockT,4).
treeSignature(selectT,6).
treeSignature(conditionalT,6).
treeSignature(ifT,6).
treeSignature(assignT,5).
treeSignature(importT,3).
treeSignature(newArrayT,6).
treeSignature(toplevelT,4).
treeSignature(newClassT,8).
treeSignature(returnT,4).
treeSignature(switchT,5).
treeSignature(typeCastT,5).
treeSignature(tryT,6).
treeSignature(whileLoopT,5).
treeSignature(continueT,5).
treeSignature(doLoopT,5).
treeSignature(indexedT,5).
treeSignature(throwT,4).
treeSignature(forLoopT,7).
treeSignature(synchronizedT,5).
treeSignature(labelT,5).
treeSignature(breakT,5).
treeSignature(typeTestT,5).
treeSignature(assignopT,6).
treeSignature(caseT,4).
treeSignature(catchT,5).
treeSignature(assertT,5).
treeSignature(getFieldT,6).
treeSignature(precedenceT,4).

attribSignature(extendsT,2).
attribSignature(implementsT,2).
attribSignature(modifierT,2).
attribSignature(externT,1).
attribSignature(interfaceT,1).
*/