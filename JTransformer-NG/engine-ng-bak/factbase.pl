/*
projectLocationT(#toplevel,Project,SourceFolder)
*/
:-dynamic projectLocationT/3.
:-multifile projectLocationT/3.

/*
fieldDefT(#id, #class, TYPE, 'name', #init)

represents the field declaration.

#id:
the unique ID assigned to this fact.

#class:
classDefT
ID of the enclosing class.

TYPE:
is a valid TypeTerm.

'name':
the name of the variable.

#init:
expression, null
ID of the initializer of this variable declaration.
*/
:- dynamic fieldDefT/5. 
/*
paramDefT(#id, #parent, TYPE, 'name')

represents the method or catch clause parameter declaration.

#id:
the unique ID assigned to this fact.

#parent:
methodDefT- if the parameter is part of a method signature,
catchT- if it is part of a catch clause.

TYPE:
is a valid TypeTerm.

'name':
the name of the parameter.
*/
:- dynamic paramDefT/4.
/*
localDefT(#id, #parent, #enclMethod, TYPE, 'name', #init)

represents local variable declaration.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

TYPE:
is a valid TypeTerm.

'name':
variable name.

#init:
expression, null
ID of the initializer of this variable declaration.
*/
:- dynamic localDefT/6.
/*
methodDefT(#id, #classDef, 'name', [param_1,...], TYPE, [#exception_1,...], #body)

represents the method, constructor or initializer declaration.

#id:
the unique ID assigned to this fact.

#classDef:
classDefT
ID of the parent class.

'name':
1) name of the method declared in this method declaration or
2) '<init>' (constractor declaration) or
3) '<clinit>' (initializer declaration).

[param_1,...]:
paramDefT
list of the method parameter.

TYPE:
is a valid TypeTerm.

[#exception_1,...]:
classDefT
list of IDs of the thrown exseptions.

#body:
blockT, null
ID of the block.
*/
:- dynamic methodDefT/7. 
/*
classDefT(#id, #owner, 'name', [#def_1,...])

represents the class or interface declaration.

#id:
the unique ID assigned to this fact.

#owner:
packageT, classDefT, newClassT, blockT, null
if this is a toplevel class (a normal class, which is member of a package (even the default package!)), the ID of the package this class belongs to. Otherwise, it is an inner class (either named or anonymous), and the owner is its declaring class.

'name':
the class name, without a package. (This is about the only time we use unqualified class names!). In the case of anonymous classes, a globally unique name: ANONYMOUS$<UN>, where <UN> is a unique number.

[#def_1,...]:
methodDefT, fieldDefT, classDefT
list of IDs for other facts representing the methods or fields, and inner classes. These fields and methods are the members (not necessarily public!) of the class.
*/
:- dynamic classDefT/4. 
/*
packageT(#id, fullname)

represents the package declaration.

#id:
the unique ID assigned to this fact.

fullname:
package name of this package declaration, as a String.


E. g.:
packageT(fqn('org.cs3.pl.astvisitor'), 'org.cs3.pl.astvisitor'). packageT(42, 'org.cs3.pl.astvisitor'). 
*/
:- dynamic packageT/2. 
:- dynamic packageT/3. 
:- multifile packageT/3. 
/*
getFieldT(#id, #parent, #encl, #expr, 'name', #field)

represents the field access expression.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

#expr:
expression, 'null'
ID of the expression designing an object on which the field is accessed or 'null' for the implicit field access.

'name':
name of the field accessed in this field access expression.

#field:
fieldDefT
ID of the field accessed by this expression.
*/
:- dynamic getFieldT/6. 
/*
identT(#id, #parent, #encl, 'name', #symbol)
represents

1) simple or qualified "this" AST node type (this expression) or
2) AST node for a simple name or
3) null literal.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

'name':
'this', 'super' or an identifier
(null literal: 'name' = 'null')

#symbol:
classDefT, localDefT, paramDefT, packageT, 'null'
ID of the referenced class or variable
(null literal: #symbol = 'null')
*/
:- dynamic identT/5. 
/*
literalT(#id, #parent, #encl, TYPE, value)

represents the literal node (boolean literal, character literal, number literal, string literal, type literal)

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

TYPE:
is a valid TypeTerm.

value
the value of this literal.
*/
:- dynamic literalT/5. 
/*
execT(#id, #parent, #enclMethod, #expr)

represents the execution of an expression - especially a method call - as a part of a method body.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#expr:
expression
ID of the expression to be executed. */
:- dynamic execT/4. 
/*
operationT (#id, #parent, #encl, [#arg1,...], operatorName, pos)

represents the infix expression, postfix expression and prefix expression.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

[#arg1,...]:
expression
list of the operands of this expression.

operatorName:
the operator of this expression (!,+,-,/,*,<<,...).

pos:
postfix expression: 1
infix expression: 0
prefix expression: -1*/
:- dynamic operationT/6. 
/*
applyT (#id, #parent, #encl, #expr, 'name', [#arg_1,...], #method)

represents the method invocation.

#id:
the unique ID assigned to this fact.

#parent:
ID of the fact that represents the parent of this fact in the prolog AST.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

#expr:
ID of another fact representing the expression on which the method is invoked.

'name':
the simple (i.e. not fully qualified) name of the called method set into high commas.

[#arg_1,...]:
expression
list of IDs of other facts representing the arguments of this method invocation.

#method:
ID of the methodDefT fact that represents the declaration of the invoked method. 
*/
:- dynamic applyT/7. 
/*
blockT(#id, #parent, #enclMethod, [#statement_1,...])

represents the block statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

[#statement_1,...]:
statement
list of the statements in this block.*/
:- dynamic blockT/4. 
/*
selectT(#id, #parent, #encl, 'name', #selectedfrom, #symbol)

represents the part of a full qualified name except the first part, which is an identT. Was formerly used for field and method accesses, which are now represented by getFieldT and applyT. See classDefT for an example

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

'name':
name of the subpackage or type which we select.

#selectedfrom:
selectT, identT
ID of the selectT or identT from which name is selected.

#symbol:
classDefT, packageT
ID of the type or package declaration. 
*/
:- dynamic selectT/6. 
/*
conditionalT (#id, #parent, #encl, #condition, #thenPart, #elsePart)

represents the conditional expression.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

#condition:
expression
ID of the expression in this conditional expression.

#thenPart:
expression
ID of the "then" part of this conditional expression.

#elsePart:
expression
ID of the "else" part of this conditional expression.
*/
:- dynamic conditionalT/6. 
/*
ifT (#id, #parent, #enclMethod, #condition, #thenPart, #elsePart)
represents the if statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#condition:
expression
ID of the expression in this if statement.

#thenPart:
statement
ID of the "then" part of this if statement.

#elsePart:
statement, null
ID of the "else" part of this if statement, or 'null' if this if statement has no "else" part.
*/
:- dynamic ifT/6. 
/*
assignT(#id, #parent, #encl, #lhs, #rhs)

represents the assignment expression.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

#lhs:
getFieldT, identT, indexedT
ID of the left hand side of this assignment expression.

#rhs:
expression
ID of the right hand side of this assignment expression.
*/
:- dynamic assignT/5. 

/*
importT(#id, #toplevel, #classid or packagename)

represents the import declaration.

#id:
the unique ID assigned to this fact.

#toplevel:
toplevelT
ID of the compilation unit.

#classid or package name:
classDefT, 'name'
ID of the class or package name.

E. g.:
importT(0, 1, 3).
importT(0, 1, 'java.util'). 
*/
:- dynamic importT/3. 
/*
newArrayT (#id, #parent, #encl, [#dim_1,...], [#elem_1,...], TYPE)

represents the array creation expression.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

[#dim_1,...]:
expression
list of dimension expressions.

[#elem_1,...]:
expression
list of initial elements of this array.

TYPE:
TypeTerm of this array.
*/
:- dynamic newArrayT/6. 

/*
toplevelT(#id, #package, 'filename', [#def_1,...])

represents the java source file (eAST compilation unit).

#id:
the unique ID assigned to this fact.

#package:
packageT, null
ID of the package declaration of this compilation unit.

'filename':
filename of this compilation unit, absolute path.

[#def_1,...]:
importT, classDefT
list of IDs for other facts representing the top-level type declarations nodes.

E. g.:
toplevelT(0, fqn('org.cs3.pl.astvisitor'), L:\Work\XP-Prak\eclipse\junit-workbench-workspace\prolog_test\src\org\cs3\pl\astvisitor\CodeTest1.java', [fqn('org.cs3.pl.astvisitor.CodeTest1')])). 
*/
:- dynamic toplevelT/4. 
/*
newClassT (#id, #parent, #encl, #constructor, [#arg_1,...], #typeExpr, #def, #enclosing)

represents the class instance creation expression.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

#constructor:
methodDefT, null
ID of the constructor invoked by this expression. If the referenced constructor is a anonymous class constructor #constructor is 'null'.

[#arg_1,...]:
expression
list of argument expressions in this class instance creation expression.

#typeExpr:
ID of an identT or selectT[1] fact, representing a simple type name or a full qualified type name respectively.

#def:
classDefT, null
the anonymous class declaration introduced by this class instance creation expression, if it has one.

#enclosing:
classDefT ID of the inner or member class constructor or ID of the anonymous class.


[#1]
For example the Java fragment*/
:- dynamic newClassT/8. 
/*
returnT(#id, #parent, #enclMethod, #expr)

represents the return statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#expr:
expression, null
ID of the expression of this return statement, or null if there is none.
*/
:- dynamic returnT/4. 
/*
switchT(#id, #parent, #enclMethod, #expr, [#statement_1, ...])
represents the switch statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#expr:
expression
ID of the selection expression (the variable used to switch).

[#statement_1, ...]:
statement
list of the statements in the switch. Cases (caseT) are a special kind of statement here, much like labels. The default statement is a case
*/
:- dynamic switchT/5. 
/*
typeCastT (#id, #parent, #encl, TYPE, #expr)

represents the cast expression.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

TYPE:
is a valid TypeTerm.

#expr:
expression
ID of the expression of this cast expression.
*/
:- dynamic typeCastT/5. 
/*
tryT (#id, #parent, #enclMethod, #body, [#catcher_1,...], #finalizer)

represents the try statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#body:
blockT
ID of the body, guarded by the try-catch statement.

[#catcher_1,...]:
catchT
list of the exception catchers.

#finalizer:
blockT,null
ID of the block containing the statements of the finally part.
*/
:- dynamic tryT/6. 
/*
whileLoopT(#id, #parent, #enclMethod, #condition, #body)

represents the while statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#condition:
expression
ID of the expression of this while statement.

#body
statement
ID of the body of this while statement.
*/
:- dynamic whileLoopT/5. 
/*
continueT(#id, #parent, #enclMethod, label, #target)

represents the continue statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

label:
label of this continue statement, or 'null' if there is none.
If label is 'null' continue leaves the enclosing loop. Otherwise the loop marked with this label.

#target
statement
ID of the statement where to jump.
*/
:- dynamic continueT/5. 
/*
doLoopT(#id, #parent, #enclMethod, #condition, #body)

represents the do statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#condition:
expression
ID of the expression of the loop condition of this do statement.

#body
statement
ID of the body of this do statement.
*/
:- dynamic doLoopT/5. 
/*
indexedT (#id, #parent, #encl, #index, #indexed)

represents the array access expression.

#id:
the unique id assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

#index:
expression
ID of the index expression of this array access expression.

#indexed:
expression
ID of the array expression of this array access expression.
*/
:- dynamic indexedT/5. 
/*
throwT (#id, #parent, #enclMethod, #expr)

represents the throw statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#expr:
expression
ID of the exception.
*/
:- dynamic throwT/4. 
/*
forLoopT (#id, #parent, #enclMethod, [#init_1,...], #condition, [#step_1,...], #body)

represents the for statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

[#init_1,...]:
expression, localDefT, null
list of IDs of the initializer expressions in this for statement.

#condition:
expression, null
ID of the expression in this for statement.

[#step_1,...]:
expression, null
list of IDs of the update expressions in this for statement.

#body:
statement
ID of the body of this for statement.
*/
:- dynamic forLoopT/7. 
/*
synchronizedT (#id, #parent, #enclMethod, #lock, #body)

represents the synchronized statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#lock:
*/
:- dynamic synchronizedT/5. 
/*
labelT (#id, #parent, #enclMethod, #body, 'name')

represents the labeled statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#body:
statement
ID of the body of this labeled statement.

'name':
the name of this label.*/
:- dynamic labelT/5. 
/*
breakT(#id, #parent, #enclMethod, label, #target)

represents the break statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

label:
if label is 'null' break leaves the enclosing block. Otherwise the block marked with this label. In case of an switch block the target is the switch statement.

#target
statement
ID of the statement where to jump. */
:- dynamic breakT/5. 
/*
typeTestT (#id, #parent, #encl, TYPE, #expr)

represents the instanceof expression.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

TYPE:
is a valid TypeTerm (right operand of this instanceof expression).

#expr:
expression
ID of the left operand of this instanceof expression.
*/
:- dynamic typeTestT/5. 
/*
assignopT(#id, #parent, #encl, #lhs, 'operator', #rhs)

represents the assignment expression.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

#lhs:
getFieldT, identT, indexedT
ID of the left hand side of this assignment expression.

'operator':
assignment operator.

#rhs:
expression
ID of the right hand side of this assignment expression.
*/
:- dynamic assignopT/6. 
/*
caseT(#id, #parent, #enclMethod, #label)
represents the case statement within the switch statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the switch statement being used for the node. (which incidentally is the parent within the AST).

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#label:
expression, null
ID of the reference to a label. For the default case (default:) this is 'null'.
*/
:- dynamic caseT/4. 
/*
catchT (#id, #parent, #enclMethod, #param, #body)

represents the catcher in the try-catch statement.

#id:
the unique ID assigned to this fact.

#parent:
ID of the containing try statement.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#param:
paramDefT
ID of the declaration of an exception object.

#body:
blockT
ID of the block containing the statements of this catch block part.*/
:- dynamic catchT/5. 
/*
assertT(#id, #parent, #enclMethod, #condition)

represents the assert statement (Java 1.4).

#id:
the unique ID assigned to this fact.

#parent:
ID of the father node in the AST, typically a block.

#enclMethod:
methodDefT
ID of the enclosing method declaration.

#condition:
expression
boolean expression's ID.
*/
:- dynamic assertT/5. 
/*
modifierT(#id, modifier)
represents the modifier flags.

#id:
ID of the methodDefT, fieldDefT or classDefT to be modified (this is NOT a singular, unique ID, but a reference to another ID!).

modifier:
one of serveral atoms:

    * 'public'
    * 'private'
    * 'protected'
    * 'static'
    * 'strictfp'
    * 'synchronized'
    * 'transient'
    * 'native'
    * 'volatile'
    * 'abstact'
    * 'final'
*/
:- dynamic modifierT/2. 
/*
externT(#id)

represents the Tagging-Fact to mark bytecode-only classes. These class-facts only contain information up to the interface level.

#id:
classDefT
ID of the class tagged. 
*/
:- dynamic externT/1. 
:- dynamic interfaceT/1. 
:- dynamic lastID/1. 
/*
implementsT(#class, #interface)

#class:
classDefT
ID of the class.

#interface:
classDefT
ID of an interface implemented by the class. 
*/
:- dynamic implementsT/2. 
/*
extendsT(#class, #extendedClass)

#class:
classDefT
ID of the class.

#extendedClass:
classDefT
ID of the superclass of the class. */
:- dynamic extendsT/2.
/*
precedenceT(#id, #parent, #encl, #expr)
represents an expression in parenthesises () in the source code.

#id:
the unique ID given to this node.

#parent:
ID if the parent node in the AST.

#encl:
methodDefT, fieldDefT
ID of the fact that represents the enclosing element.

#expr:
expression
ID of the expression within the parenthesis. */
:- dynamic precedenceT/4.
/*
nopT(#id, #parent, #enclMethod).
represents the no operation node, signifying a lone semicolon.

#id:
the unique ID assigned to this fact.

#parent:
ID of the parent node.

#enclMethod:
methodDefT
ID of the enclosing method declaration.
*/
:- dynamic nopT/3.


/**
 * sourceLocation(?ID, ?File, ?Begin, ?Length)
 *
 * 
 */
:- dynamic sourceLocation/4.

:- multifile fieldDefT/5. 

:- multifile paramDefT/4.
:- multifile localDefT/6.
:- multifile methodDefT/7. 
:- multifile classDefT/4. 
:- multifile getFieldT/6. 
:- multifile identT/5. 
:- multifile literalT/5. 
:- multifile execT/4. 
:- multifile operationT/6. 
:- multifile applyT/7. 
:- multifile blockT/4. 
:- multifile selectT/6. 
:- multifile conditionalT/6. 
:- multifile ifT/6. 
:- multifile assignT/5. 
:- multifile importT/3. 
:- multifile newArrayT/6. 
:- multifile toplevelT/4. 
:- multifile newClassT/8. 
:- multifile returnT/4. 
:- multifile switchT/5. 
:- multifile typeCastT/5. 
:- multifile tryT/6. 
:- multifile whileLoopT/5. 
:- multifile continueT/5. 
:- multifile doLoopT/5. 
:- multifile indexedT/5. 
:- multifile throwT/4. 
:- multifile forLoopT/7. 
:- multifile synchronizedT/5. 
:- multifile labelT/5. 
:- multifile breakT/5. 
:- multifile typeTestT/5. 
:- multifile assignopT/6. 
:- multifile caseT/4. 
:- multifile catchT/5. 
:- multifile assertT/5. 
:- multifile modifierT/2. 
:- multifile externT/1. 
:- multifile interfaceT/1. 
:- multifile lastID/1. 
:- multifile packageT/2. 
:- multifile implementsT/2. 
:- multifile extendsT/2. 
:- multifile precedenceT/4.
:- multifile nopT/3.
:- multifile sourceLocation/4.

tree(_id, null, packageT):-packageT(_id,_).
tree(_id, _pid, localDefT):-localDefT(_id, _pid,_,_,_,_).
tree(_id, _pid, paramDefT):-paramDefT(_id, _pid,_,_).
tree(_id, _pid, fieldDefT):-fieldDefT(_id, _pid,_,_,_).
tree(_id, _pid, methodDefT):-methodDefT(_id, _pid,_,_,_,_,_).
tree(_id, _pid, classDefT):-classDefT(_id, _pid,_,_).
tree(_id, _pid, getFieldT):-getFieldT(_id, _pid,_,_,_,_).
tree(_id, _pid, identT):-identT(_id, _pid,_,_,_).
tree(_id, _pid, literalT):-literalT(_id, _pid,_,_,_).
tree(_id, _pid, execT):-execT(_id, _pid,_,_).
tree(_id, _pid, operationT):- operationT(_id, _pid,_,_,_,_).
tree(_id, _pid, applyT):-applyT(_id, _pid,_,_,_,_,_).
tree(_id, _pid, blockT):-blockT(_id, _pid,_,_).
tree(_id, _pid, selectT):-selectT(_id, _pid,_,_,_,_).
tree(_id, _pid, conditionalT):-conditionalT(_id, _pid,_,_,_,_).
tree(_id, _pid, ifT):-ifT(_id, _pid,_,_,_,_).
tree(_id, _pid, assignT):-assignT(_id, _pid,_,_,_).
tree(_id, _pid, importT):-importT(_id, _pid,_).
tree(_id, _pid, newArrayT):-newArrayT(_id, _pid,_,_,_,_).
tree(_id, _pid, toplevelT):-toplevelT(_id, _pid,_,_).
tree(_id, _pid, newClassT):-newClassT(_id, _pid,_,_,_,_,_,_).
tree(_id, _pid, returnT):-returnT(_id, _pid,_,_).
tree(_id, _pid, switchT):-switchT(_id, _pid,_,_,_).
tree(_id, _pid, typeCastT):-typeCastT(_id, _pid,_,_,_).
tree(_id, _pid, tryT):-tryT(_id, _pid,_,_,_,_).
tree(_id, _pid, whileLoopT):-whileLoopT(_id, _pid,_,_,_).
tree(_id, _pid, continueT):-continueT(_id, _pid,_,_,_).
tree(_id, _pid, doLoopT):-doLoopT(_id, _pid,_,_,_).
tree(_id, _pid, indexedT):-indexedT(_id, _pid,_,_,_).
tree(_id, _pid, throwT):-throwT(_id, _pid,_,_).
tree(_id, _pid, forLoopT):-forLoopT(_id, _pid,_,_,_,_,_).
tree(_id, _pid, synchronizedT):-synchronizedT(_id, _pid,_,_,_).
tree(_id, _pid, labelT):-labelT(_id, _pid,_,_,_).
tree(_id, _pid, breakT):-breakT(_id, _pid,_,_,_).
tree(_id, _pid, typeTestT):-typeTestT(_id, _pid,_,_,_).
tree(_id, _pid, assignopT):-assignopT(_id, _pid,_,_,_,_).
tree(_id, _pid, caseT):-caseT(_id, _pid,_,_).
tree(_id, _pid, catchT):-catchT(_id, _pid,_,_,_).
tree(_id, _pid, assertT):-assertT(_id, _pid,_,_,_).
tree(_id, _pid, precedenceT):-precedenceT(_id, _pid,_,_).
tree(_id, _pid, nopT):-nopT(_id, _pid,_).

treeSignature(localDefT, 6).
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
treeSignature(nopT,3).         % TODO: fehlte / inkonsistenz zu tree/3 -- GK 3.9.2004

attribSignature(extendsT,2).
attribSignature(implementsT,2).
attribSignature(modifierT,2).
attribSignature(externT,1).
attribSignature(interfaceT,1).
attribSignature(sourceLocation,4).
attribSignature(projectLocationT,3).

basicType(char).
basicType(int).
basicType(float).
basicType(double).
basicType(void).
basicType(long).
basicType(short).
basicType(byte).
basicType(bolean).

lastID(100000).

%%% source location %%%
:- dynamic slT/3. 
:- multifile slT/3. 

