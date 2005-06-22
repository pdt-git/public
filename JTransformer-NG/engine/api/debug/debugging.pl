
%
% @author Bruno Harbulot <bruno@cs.man.ac.uk>
%
% trho: Probably obsolete

%:- ['visitor'].
%:- ['lists'].


gen_tree_list([]).
gen_tree_list([X | T]) :-
    gen_tree(X), write('\n'), gen_tree_list(T).



writeSpaces(Number) :- Number =< 0.
writeSpaces(Number) :- Number >=0, write('  '), NewNumber is Number-1, writeSpaces(NewNumber).

printTree(Root) :-
    retractall(printTreeIndent(_)),
    assert(printTreeIndent(0)),
    walkTree(printNode, decPrintTreeIndent, printNodeBeforeList, printNodeAfterList, Root), !.
    

decPrintTreeIndent(_) :- decPrintTreeIndent.
decPrintTreeIndent :-
    retract(printTreeIndent(Indent)), NewIndent is Indent-1, assert(printTreeIndent(NewIndent)).
incPrintTreeIndent :-
    retract(printTreeIndent(Indent)), NewIndent is Indent+1, assert(printTreeIndent(NewIndent)).




printNodeBeforeList([]) :-
    incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('[]\n').
printNodeBeforeList([ _x | _t ]) :-
    incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('[\n').
    
printNodeAfterList([]) :- decPrintTreeIndent.
printNodeAfterList([ _x | _t ]) :-
    printTreeIndent(Indent), writeSpaces(Indent), write(']\n'), decPrintTreeIndent.



printNode(null) :- !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('(null)\n').
printNode(type(_, _, _)) :- !, incPrintTreeIndent.

printNode(ID) :-
    applyT(ID, Parent, Encl, Expr, Name, Arguments, Method), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('applyT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Expr), write(', '), write(Name), write(', '), write(Arguments), write(', '), write(Method), write(')\n').
printNode(ID) :-
    assertT(ID, Parent, Encl, Condition, Msg), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('assertT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Condition), write(', '), write(Msg), write(')\n').
printNode(ID) :-
    assignopT(ID, Parent, Encl, Lhs, Op, Rhs), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('assignopT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Lhs), write(', '), write(Op), write(', '), write(Rhs), write(')\n').
printNode(ID) :-
    assignT(ID, Parent, Encl, Lhs, Rhs), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('assignT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Lhs), write(', '), write(Rhs), write(')\n').
printNode(ID) :-
    blockT(ID, Parent, Encl, Statements), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('blockT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Statements), write(')\n').
printNode(ID) :-
    breakT(ID, Parent, Encl, Label, Target), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('breakT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Label), write(', '), write(Target), write(')\n').
printNode(ID) :-
    caseT(ID, Parent, Encl, Label), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('caseT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Label), write(')\n').
printNode(ID) :-
    catchT(ID, Parent, Encl, Param, Body), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('catchT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Param), write(', '), write(Body), write(')\n').
printNode(ID) :-
    conditionalT(ID, Parent, Encl, Condition, ThenPart, ElsePart), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('conditionalT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Condition), write(', '), write(ThenPart), write(', '), write(ElsePart), write(')\n').
printNode(ID) :-
    continueT(ID, Parent, Encl, Label, Target), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('continueT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Label), write(', '), write(Target), write(')\n').
printNode(ID) :-
    doLoopT(ID, Parent, Encl, Condition, Body), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('doLoopT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Condition), write(', '), write(Body), write(')\n').
printNode(ID) :-
    execT(ID, Parent, Encl, Expr), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('execT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Expr), write(')\n').
printNode(ID) :-
    forLoopT(ID, Parent, Encl, Init, Condition, Update, Body), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('forLoopT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Init), write(', '), write(Condition), write(', '), write(Update), write(', '), write(Body), write(')\n').
printNode(ID) :-
    getFieldT(ID, Parent, Encl, Expr, Name, Field), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('getFieldT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Expr), write(', '), write(Name), write(', '), write(Field), write(')\n').
printNode(ID) :-
    identT(ID, Parent, Encl, Name, Symbol), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('identT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Name), write(', '), write(Symbol), write(')\n').
printNode(ID) :-
    ifT(ID, Parent, Encl, Condition, ThenPart, ElsePart), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('ifT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Condition), write(', '), write(ThenPart), write(', '), write(ElsePart), write(')\n').
printNode(ID) :-
    indexedT(ID, Parent, Encl, Index, Indexed), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('indexedT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Index), write(', '), write(Indexed), write(')\n').
printNode(ID) :-
    labelT(ID, Parent, Encl, Body, Name), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('labelT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Body), write(', '), write(Name), write(')\n').
printNode(ID) :-
    literalT(ID, Parent, Encl, Type, Value), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('literalT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Type), write(', '), write(Value), write(')\n').
printNode(ID) :-
    localDefT(ID, Parent, Encl, Type, Name, Init), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('localDefT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Type), write(', '), write(Name), write(', '), write(Init), write(')\n'),
    printNodeAllModifiers(ID).
printNode(ID) :-
    newArrayT(ID, Parent, Encl, Dim, Elem, Type), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('newArrayT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Dim), write(', '), write(Elem), write(', '), write(Type), write(')\n').
printNode(ID) :-
    newClassT(ID, Parent, Encl, Constructor, Args, TypeExpr, Def, Enclosing), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('newClassT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Constructor), write(', '), write(Args), write(', '), write(TypeExpr), write(', '), write(Def), write(', '), write(Enclosing), write(')\n').
printNode(ID) :-
    nopT(ID, Parent, Encl), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('nopT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(')\n').
printNode(ID) :-
    operationT(ID, Parent, Encl, Args, OperatorName, Pos), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('operationT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Args), write(', '), write(OperatorName), write(', '), write(Pos), write(')\n').
printNode(ID) :-
    precedenceT(ID, Parent, Encl, Expr), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('precedenceT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Expr), write(')\n').
printNode(ID) :-
    returnT(ID, Parent, Encl, Expr), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('returnT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Expr), write(')\n').
printNode(ID) :-
    selectT(ID, Parent, Encl, Name, Selected, Symbol), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('selectT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Name), write(', '), write(Selected), write(', '), write(Symbol), write(')\n').
printNode(ID) :-
    switchT(ID, Parent, Encl, Condition, Statements), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('switchT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Condition), write(', '), write(Statements), write(')\n').
printNode(ID) :-
    synchronizedT(ID, Parent, Encl, Lock, Body), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('synchronizedT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Lock), write(', '), write(Body), write(')\n').
printNode(ID) :-
    throwT(ID, Parent, Encl, Expr), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('throwT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Expr), write(')\n').
printNode(ID) :-
    tryT(ID, Parent, Encl, Body, Catchers, Finalizer), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('tryT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Body), write(', '), write(Catchers), write(', '), write(Finalizer), write(')\n').
printNode(ID) :-
    typeCastT(ID, Parent, Encl, Type, Expr), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('typeCastT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Type), write(', '), write(Expr), write(')\n').
printNode(ID) :-
    typeTestT(ID, Parent, Encl, Type, Expr), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('typeTestT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Type), write(', '), write(Expr), write(')\n').
printNode(ID) :-
    whileLoopT(ID, Parent, Encl, Condition, Body), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('whileLoopT('),
    write(ID), write(', '), write(Parent), write(', '), write(Encl), write(', '),
    write(Condition), write(', '), write(Body), write(')\n').



printNode(ID) :-
    toplevelT(ID, Package, Filename, Defs), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('toplevelT('),
    write(ID), write(', '), write(Package), write(', '), write(Filename), write(', '), write(Defs), write(')\n').
printNode(ID) :- 
    packageT(ID, Fullname), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('packageT('),
    write(ID), write(', '), write(Fullname), write(')\n').
printNode(ID) :-
	importT(ID, TopLevel, ClassOrPackage), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('importT('),
    write(ID), write(', '), write(TopLevel), write(', '), write(ClassOrPackage), write(')\n').
printNode(ID) :- 
	classDefT(ID, Owner, Name, Defs), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('classDefT('),
    write(ID), write(', '), write(Owner), write(', '), write(Name), write(', '), write(Defs), write(')\n'),
    printNodeInheritance(ID), printNodeAllModifiers(ID), printNodeExtern(ID).
printNode(ID) :-
	methodDefT(ID, ClassDef, Name, Params, Type, Exceptions, Body), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('methodDefT('),
	write(ID), write(', '), write(ClassDef), write(', '), write(Name), write(', '), write(Params), write(', '), 
	write(Type), write(', '), write(Type), write(', '), write(Exceptions), write(', '), write(Body), write(')\n'),
	printNodeAllModifiers(ID).
printNode(ID) :- 
	fieldDefT(ID, Class, Type, Name, Init), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('fieldDefT('),
    write(ID), write(', '), write(Class), write(', '), write(Type), write(', '),
    write(Name), write(', '), write(Init), write(')\n'),
    printNodeAllModifiers(ID).
printNode(ID) :- 
	paramDefT(ID, Method, Type, Name), !, incPrintTreeIndent, printTreeIndent(Indent), writeSpaces(Indent), write('paramDefT('),
    write(ID), write(', '), write(Method), write(', '), write(Type), write(', '), write(Name), write(')\n'),
    printNodeAllModifiers(ID).

printNode(_) :- !.





    
printNodeExtern(ID) :- 
	externT(ID), printTreeIndent(Indent), writeSpaces(Indent), write('/externT('), write(ID), write(')/\n'), !.
printNodeExtern(_).

printNodeInterface(ID) :- 
	interfaceT(ID), printTreeIndent(Indent), writeSpaces(Indent), write('/interfaceT('), write(ID), write(')/\n').

printNodeImplements(ID, Interface) :-
    printTreeIndent(Indent), writeSpaces(Indent), write('/implementsT('),
    write(ID), write(', '), write(Interface), write(')/\n').
printNodeExtends(ID, SuperClass) :-
    printTreeIndent(Indent), writeSpaces(Indent), write('/extendsT('),
    write(ID), write(', '), write(SuperClass), write(')/\n').

printNodeModifier(ID, Modifier) :- 
	printTreeIndent(Indent), writeSpaces(Indent), write('/modifierT('),
    write(ID), write(', '), write(Modifier), write(')/\n').


printNodeInheritance(ID) :-
    (printNodeInterface(ID) ; true), !,
    findall(SuperClass, extendsT(ID, SuperClass), Extends), !, mapGoalAlwaysSucceed(printNodeExtends(ID), Extends), !,
    findall(Interface, implementsT(ID, Interface), Implements), !, mapGoalAlwaysSucceed(printNodeImplements(ID), Implements), !.
    
printNodeAllModifiers(ID) :-
    findall(Modif, modifierT(ID, Modif), Modifiers), !, mapGoalAlwaysSucceed(printNodeModifier(ID), Modifiers), !.
    
  
/**
  printListLocalVariablesOrParams(List).
  
  Displays the list of localDefTs or paramDefTs. Mainly for debugging purposes.
 */
printListLocalVariablesOrParams([]).
printListLocalVariablesOrParams([X|Tail]) :- 
    paramDefT(X, _, _, Name),
    write(Name), write('\n'),
    printListLocalVariablesOrParams(Tail).
printListLocalVariablesOrParams([X|Tail]) :- 
    localDefT(X, _, _, _, Name, _),
    write(Name), write('\n'),
    printListLocalVariablesOrParams(Tail).








