%
% @author Bruno Harbulot <bruno@cs.man.ac.uk>
%

% Unlike cloneTree/2, duplicateTree/2 does not add a cloned class to
% the top level tree.

%:- ['visitor'].


duplicate(X, Y) :-
    cloned(X, Y), !.
duplicate('null', 'null') :- !.
duplicate([], []) :- !.
duplicate([OldRef | OldList], [NewRef | NewList]) :-
    duplicate(OldRef, NewRef),
    duplicate(OldList, NewList), !.
duplicate([X | OldList], [X | NewList]) :-
    duplicate(OldList, NewList), !.
duplicate(type(basic, TypeName, Num), type(basic, TypeName, Num)).
duplicate(type(class, OldRef, Num), type(class, NewRef, Num)) :-
	duplicate(OldRef, NewRef) ; NewRef=OldRef.




replaceAllRefsInID(ID) :-
    (replaceRefInID(ID), fail ; true).
replaceRefsInClonedEntries :-
    ((cloned(_, X), replaceAllRefsInID(X)), fail ; true).

duplicateTree(Tree, NewTree) :-
    retractall(cloned(_,_)), !,
	walkTree(duplicateNode, nothing, nothing, nothing, Tree), !,
	duplicate(Tree, NewTree), !,
	replaceRefsInClonedEntries, !.




% % % interfaceT(Class)
% % % externT(Class)
% % % implementsT(Class,Interface)
% % % extendsT(Class,ExtendedClass)

duplicateInterface(ID, NewID) :-
    interfaceT(ID), !, assert(interfaceT(NewID)), !.
duplicateInterface(_, _) :- !.

duplicateExtern(ID, NewID) :-
    externT(ID), !, assert(externT(NewID)), !.
duplicateExtern(_, _) :- !.

duplicateImplements(_, _, []).
duplicateImplements(ID, NewID, [ Interface | T ]) :-
    assert(implementsT(NewID, Interface)), duplicateImplements(ID, NewID, T).
duplicateImplements(ID, NewID) :-
    findall(Interface, implementsT(ID, Interface), List), !,
    duplicateImplements(ID, NewID, List).
    
duplicateExtends(_, _, []).
duplicateExtends(ID, NewID, [ SuperClass | T ]) :-
    assert(extendsT(NewID, SuperClass)), duplicateExtends(ID, NewID, T).
duplicateExtends(ID, NewID) :-
    findall(SuperClass, extendsT(ID, SuperClass), List), !,
    duplicateExtends(ID, NewID, List).
    
duplicateModifiers(_, _, []).
duplicateModifiers(ID, NewID, [ Modifier | T ]) :-
    assert(modifierT(NewID, Modifier)), duplicateModifiers(ID, NewID, T).
duplicateModifiers(ID, NewID) :-
    findall(Modifier, modifierT(ID, Modifier), List), !,
    duplicateModifiers(ID, NewID, List).

duplicateNonUniques(ID, NewID) :-
    !,
    duplicateInterface(ID, NewID), !,
    duplicateExtern(ID, NewID), !,
    duplicateImplements(ID, NewID), !,
    duplicateExtends(ID, NewID), !,
    duplicateModifiers(ID, NewID), !,
    true.





duplicateNode(ID) :-
    duplicateNode(ID, NewID), !,
    duplicateNonUniques(ID, NewID), !,
    assert(cloned(ID, NewID)).
duplicateNode(_) :- !.




duplicateNode(ID, NewID) :-
    toplevelT(ID, Package, Filename, DefList), 
    new_id(NewID), 
    assert(toplevelT(NewID, Package, Filename, DefList)).

duplicateNode(ID, NewID) :-
    packageT(ID, Fullname), 
    new_id(NewID), 
    assert(packageT(NewID, Fullname)).

duplicateNode(ID, NewID) :-
    importT(ID, TopLevel, ClassidOrPackagename), 
    new_id(NewID), 
    assert(importT(NewID, TopLevel, ClassidOrPackagename)).

duplicateNode(ID, NewID) :-
    classDefT(ID, Owner, Name, DefList), 
    new_id(NewID), 
    assert(classDefT(NewID, Owner, Name, DefList)).

duplicateNode(ID, NewID) :-
    methodDefT(ID, ClassDef, Name, ParamList, Type, ExceptionList, Body), 
    new_id(NewID), 
    assert(methodDefT(NewID, ClassDef, Name, ParamList, Type, ExceptionList, Body)).

duplicateNode(ID, NewID) :-
    fieldDefT(ID, Class, Type, Name, Init), 
    new_id(NewID), 
    assert(fieldDefT(NewID, Class, Type, Name, Init)).

duplicateNode(ID, NewID) :-
    paramDefT(ID, Method, Type, Name), 
    new_id(NewID), 
    assert(paramDefT(NewID, Method, Type, Name)).

duplicateNode(ID, NewID) :-
    applyT(ID, Parent, Encl, Expr, Name, ArgList, Method), 
    new_id(NewID), 
    assert(applyT(NewID, Parent, Encl, Expr, Name, ArgList, Method)).

duplicateNode(ID, NewID) :-
    assertT(ID, Parent, Encl, Condition, Msg), 
    new_id(NewID), 
    assert(assertT(NewID, Parent, Encl, Condition, Msg)).

duplicateNode(ID, NewID) :-
    assignopT(ID, Parent, Encl, Lhs, Operator, Rhs), 
    new_id(NewID), 
    assert(assignopT(NewID, Parent, Encl, Lhs, Operator, Rhs)).

duplicateNode(ID, NewID) :-
    assignT(ID, Parent, Encl, Lhs, Rhs), 
    new_id(NewID), 
    assert(assignT(NewID, Parent, Encl, Lhs, Rhs)).

duplicateNode(ID, NewID) :-
    blockT(ID, Parent, Encl, StatementsList), 
    new_id(NewID), 
    assert(blockT(NewID, Parent, Encl, StatementsList)).

duplicateNode(ID, NewID) :-
    breakT(ID, Parent, Encl, Label, Target), 
    new_id(NewID), 
    assert(breakT(NewID, Parent, Encl, Label, Target)).

duplicateNode(ID, NewID) :-
    caseT(ID, Parent, Encl, Label), 
    new_id(NewID), 
    assert(caseT(NewID, Parent, Encl, Label)).

duplicateNode(ID, NewID) :-
    catchT(ID, Parent, Encl, Param, Body), 
    new_id(NewID), 
    assert(catchT(NewID, Parent, Encl, Param, Body)).

duplicateNode(ID, NewID) :-
    conditionalT(ID, Parent, Encl, Condition, ThenPart, ElsePart), 
    new_id(NewID), 
    assert(conditionalT(NewID, Parent, Encl, Condition, ThenPart, ElsePart)).

duplicateNode(ID, NewID) :-
    continueT(ID, Parent, Encl, Label, Target), 
    new_id(NewID), 
    assert(continueT(NewID, Parent, Encl, Label, Target)).

duplicateNode(ID, NewID) :-
    doLoopT(ID, Parent, Encl, Condition, Body), 
    new_id(NewID), 
    assert(doLoopT(NewID, Parent, Encl, Condition, Body)).

duplicateNode(ID, NewID) :-
    execT(ID, Parent, Encl, Expr), 
    new_id(NewID), 
    assert(execT(NewID, Parent, Encl, Expr)).

duplicateNode(ID, NewID) :-
    forLoopT(ID, Parent, Encl, InitList, Condition, UpdateList, Body), 
    new_id(NewID), 
    assert(forLoopT(NewID, Parent, Encl, InitList, Condition, UpdateList, Body)).

duplicateNode(ID, NewID) :-
    getFieldT(ID, Parent, Encl, Expr, Name, Field), 
    new_id(NewID), 
    assert(getFieldT(NewID, Parent, Encl, Expr, Name, Field)).

duplicateNode(ID, NewID) :-
    identT(ID, Parent, Encl, Name, Symbol), 
    new_id(NewID), 
    assert(identT(NewID, Parent, Encl, Name, Symbol)).

duplicateNode(ID, NewID) :-
    ifT(ID, Parent, Encl, Condition, ThenPart, ElsePart), 
    new_id(NewID), 
    assert(ifT(NewID, Parent, Encl, Condition, ThenPart, ElsePart)).

duplicateNode(ID, NewID) :-
    indexedT(ID, Parent, Encl, Index, Indexed), 
    new_id(NewID), 
    assert(indexedT(NewID, Parent, Encl, Index, Indexed)).

duplicateNode(ID, NewID) :-
    labelT(ID, Parent, Encl, Body, Name), 
    new_id(NewID), 
    assert(labelT(NewID, Parent, Encl, Body, Name)).

duplicateNode(ID, NewID) :-
    literalT(ID, Parent, Encl, Type, Value), 
    new_id(NewID), 
    assert(literalT(NewID, Parent, Encl, Type, Value)).

duplicateNode(ID, NewID) :-
    localDefT(ID, Parent, EnclClass, Type, Name, Init), 
    new_id(NewID), 
    assert(localDefT(NewID, Parent, EnclClass, Type, Name, Init)).

duplicateNode(ID, NewID) :-
    newArrayT(ID, Parent, Encl, DimList, ElemList, Type), 
    new_id(NewID), 
    assert(newArrayT(NewID, Parent, Encl, DimList, ElemList, Type)).

duplicateNode(ID, NewID) :-
    newClassT(ID, Parent, Encl, Constructor, ArgList, TypeExpr, Def, Enclosing), 
    new_id(NewID), 
    assert(newClassT(NewID, Parent, Encl, Constructor, ArgList, TypeExpr, Def, Enclosing)).

duplicateNode(ID, NewID) :-
    nopT(ID, Parent, Encl), 
    new_id(NewID), 
    assert(nopT(NewID, Parent, Encl)).

duplicateNode(ID, NewID) :-
    operationT(ID, Parent, Encl, ArgList, OperatorName, Pos), 
    new_id(NewID), 
    assert(operationT(NewID, Parent, Encl, ArgList, OperatorName, Pos)).

duplicateNode(ID, NewID) :-
    precedenceT(ID, Parent, Encl, Expr), 
    new_id(NewID), 
    assert(precedenceT(NewID, Parent, Encl, Expr)).

duplicateNode(ID, NewID) :-
    returnT(ID, Parent, Encl, Exec), 
    new_id(NewID), 
    assert(returnT(NewID, Parent, Encl, Exec)).

duplicateNode(ID, NewID) :-
    selectT(ID, Parent, Encl, Name, Selected, Symbol), 
    new_id(NewID), 
    assert(selectT(NewID, Parent, Encl, Name, Selected, Symbol)).

duplicateNode(ID, NewID) :-
    switchT(ID, Parent, Encl, Condition, StatementsList), 
    new_id(NewID), 
    assert(switchT(NewID, Parent, Encl, Condition, StatementsList)).

duplicateNode(ID, NewID) :-
    synchronizedT(ID, Parent, Encl, Lock, Body), 
    new_id(NewID), 
    assert(synchronizedT(NewID, Parent, Encl, Lock, Body)).

duplicateNode(ID, NewID) :-
    throwT(ID, Parent, Encl, Exec), 
    new_id(NewID), 
    assert(throwT(NewID, Parent, Encl, Exec)).

duplicateNode(ID, NewID) :-
    tryT(ID, Parent, Encl, Body, CatchersList, Finalizer), 
    new_id(NewID), 
    assert(tryT(NewID, Parent, Encl, Body, CatchersList, Finalizer)).

duplicateNode(ID, NewID) :-
    typeCastT(ID, Parent, Encl, Type, Expr), 
    new_id(NewID), 
    assert(typeCastT(NewID, Parent, Encl, Type, Expr)).

duplicateNode(ID, NewID) :-
    typeTestT(ID, Parent, Encl, Type, Expr), 
    new_id(NewID), 
    assert(typeTestT(NewID, Parent, Encl, Type, Expr)).

duplicateNode(ID, NewID) :-
    whileLoopT(ID, Parent, Encl, Condition, Body), 
    new_id(NewID), 
    assert(whileLoopT(NewID, Parent, Encl, Condition, Body)).















% toplevelT(ID,Package,Filename,DefList)
% Package
replaceRefInID(ID) :-
    toplevelT(ID, OldRef, Filename, DefList),
    (duplicate(OldRef, NewRef), 
     retract(toplevelT(ID, OldRef, Filename, DefList)),
     assert(toplevelT(ID, NewRef, Filename, DefList))
     ; true).
% toplevelT(ID,Package,Filename,DefList)
% Filename
% toplevelT(ID,Package,Filename,DefList)
% DefList
replaceRefInID(ID) :-
    toplevelT(ID, Package, Filename, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(toplevelT(ID, Package, Filename, OldRef)),
     assert(toplevelT(ID, Package, Filename, NewRef))
     ; true).

% packageT(ID,Fullname)
% Fullname

% importT(ID,TopLevel,ClassidOrPackagename)
% TopLevel
replaceRefInID(ID) :-
    importT(ID, OldRef, ClassidOrPackagename),
    (duplicate(OldRef, NewRef), 
     retract(importT(ID, OldRef, ClassidOrPackagename)),
     assert(importT(ID, NewRef, ClassidOrPackagename))
     ; true).
% importT(ID,TopLevel,ClassidOrPackagename)
% ClassidOrPackagename
replaceRefInID(ID) :-
    importT(ID, TopLevel, OldRef),
    (classDefT(OldRef, _, _, _),
     duplicate(OldRef, NewRef), 
     retract(importT(ID, TopLevel, OldRef)),
     assert(importT(ID, TopLevel, NewRef))
     ; true).

% classDefT(ID,Owner,Name,DefList)
% Owner
replaceRefInID(ID) :-
    classDefT(ID, OldRef, Name, DefList),
    (duplicate(OldRef, NewRef), 
     retract(classDefT(ID, OldRef, Name, DefList)),
     assert(classDefT(ID, NewRef, Name, DefList))
     ; true).
% classDefT(ID,Owner,Name,DefList)
% Name
% classDefT(ID,Owner,Name,DefList)
% DefList
replaceRefInID(ID) :-
    classDefT(ID, Owner, Name, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(classDefT(ID, Owner, Name, OldRef)),
     assert(classDefT(ID, Owner, Name, NewRef))
     ; true).

% methodDefT(ID,ClassDef,Name,ParamList,Type,ExceptionList,Body)
% ClassDef
replaceRefInID(ID) :-
    methodDefT(ID, OldRef, Name, ParamList, Type, ExceptionList, Body),
    (duplicate(OldRef, NewRef), 
     retract(methodDefT(ID, OldRef, Name, ParamList, Type, ExceptionList, Body)),
     assert(methodDefT(ID, NewRef, Name, ParamList, Type, ExceptionList, Body))
     ; true).
% methodDefT(ID,ClassDef,Name,ParamList,Type,ExceptionList,Body)
% Name
% methodDefT(ID,ClassDef,Name,ParamList,Type,ExceptionList,Body)
% ParamList
replaceRefInID(ID) :-
    methodDefT(ID, ClassDef, Name, OldRef, Type, ExceptionList, Body),
    (duplicate(OldRef, NewRef), 
     retract(methodDefT(ID, ClassDef, Name, OldRef, Type, ExceptionList, Body)),
     assert(methodDefT(ID, ClassDef, Name, NewRef, Type, ExceptionList, Body))
     ; true).
% methodDefT(ID,ClassDef,Name,ParamList,Type,ExceptionList,Body)
% Type
replaceRefInID(ID) :-
    methodDefT(ID, ClassDef, Name, ParamList, OldRef, ExceptionList, Body),
    (duplicate(OldRef, NewRef), 
     retract(methodDefT(ID, ClassDef, Name, ParamList, OldRef, ExceptionList, Body)),
     assert(methodDefT(ID, ClassDef, Name, ParamList, NewRef, ExceptionList, Body))
     ; true).
% methodDefT(ID,ClassDef,Name,ParamList,Type,ExceptionList,Body)
% ExceptionList
replaceRefInID(ID) :-
    methodDefT(ID, ClassDef, Name, ParamList, Type, OldRef, Body),
    (duplicate(OldRef, NewRef), 
     retract(methodDefT(ID, ClassDef, Name, ParamList, Type, OldRef, Body)),
     assert(methodDefT(ID, ClassDef, Name, ParamList, Type, NewRef, Body))
     ; true).
% methodDefT(ID,ClassDef,Name,ParamList,Type,ExceptionList,Body)
% Body
replaceRefInID(ID) :-
    methodDefT(ID, ClassDef, Name, ParamList, Type, ExceptionList, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(methodDefT(ID, ClassDef, Name, ParamList, Type, ExceptionList, OldRef)),
     assert(methodDefT(ID, ClassDef, Name, ParamList, Type, ExceptionList, NewRef))
     ; true).

% fieldDefT(ID,Class,Type,Name,Init)
% Class
replaceRefInID(ID) :-
    fieldDefT(ID, OldRef, Type, Name, Init),
    (duplicate(OldRef, NewRef), 
     retract(fieldDefT(ID, OldRef, Type, Name, Init)),
     assert(fieldDefT(ID, NewRef, Type, Name, Init))
     ; true).
% fieldDefT(ID,Class,Type,Name,Init)
% Type
replaceRefInID(ID) :-
    fieldDefT(ID, Class, OldRef, Name, Init),
    (duplicate(OldRef, NewRef), 
     retract(fieldDefT(ID, Class, OldRef, Name, Init)),
     assert(fieldDefT(ID, Class, NewRef, Name, Init))
     ; true).
% fieldDefT(ID,Class,Type,Name,Init)
% Name
% fieldDefT(ID,Class,Type,Name,Init)
% Init
replaceRefInID(ID) :-
    fieldDefT(ID, Class, Type, Name, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(fieldDefT(ID, Class, Type, Name, OldRef)),
     assert(fieldDefT(ID, Class, Type, Name, NewRef))
     ; true).

% paramDefT(ID,Method,Type,Name)
% Method
replaceRefInID(ID) :-
    paramDefT(ID, OldRef, Type, Name),
    (duplicate(OldRef, NewRef), 
     retract(paramDefT(ID, OldRef, Type, Name)),
     assert(paramDefT(ID, NewRef, Type, Name))
     ; true).
% paramDefT(ID,Method,Type,Name)
% Type
replaceRefInID(ID) :-
    paramDefT(ID, Method, OldRef, Name),
    (duplicate(OldRef, NewRef), 
     retract(paramDefT(ID, Method, OldRef, Name)),
     assert(paramDefT(ID, Method, NewRef, Name))
     ; true).
% paramDefT(ID,Method,Type,Name)
% Name

% % % interfaceT(Class)

% % % modifierT(ID,Modifier)

% % % externT(Class)

% % % implementsT(Class,Interface)

% % % extendsT(Class,ExtendedClass)

% applyT(ID,Parent,Encl,Expr,Name,ArgList,Method)
% Parent
replaceRefInID(ID) :-
    applyT(ID, OldRef, Encl, Expr, Name, ArgList, Method),
    (duplicate(OldRef, NewRef), 
     retract(applyT(ID, OldRef, Encl, Expr, Name, ArgList, Method)),
     assert(applyT(ID, NewRef, Encl, Expr, Name, ArgList, Method))
     ; true).
% applyT(ID,Parent,Encl,Expr,Name,ArgList,Method)
% Encl
replaceRefInID(ID) :-
    applyT(ID, Parent, OldRef, Expr, Name, ArgList, Method),
    (duplicate(OldRef, NewRef), 
     retract(applyT(ID, Parent, OldRef, Expr, Name, ArgList, Method)),
     assert(applyT(ID, Parent, NewRef, Expr, Name, ArgList, Method))
     ; true).
% applyT(ID,Parent,Encl,Expr,Name,ArgList,Method)
% Expr
replaceRefInID(ID) :-
    applyT(ID, Parent, Encl, OldRef, Name, ArgList, Method),
    (duplicate(OldRef, NewRef), 
     retract(applyT(ID, Parent, Encl, OldRef, Name, ArgList, Method)),
     assert(applyT(ID, Parent, Encl, NewRef, Name, ArgList, Method))
     ; true).
% applyT(ID,Parent,Encl,Expr,Name,ArgList,Method)
% Name
% applyT(ID,Parent,Encl,Expr,Name,ArgList,Method)
% ArgList
replaceRefInID(ID) :-
    applyT(ID, Parent, Encl, Expr, Name, OldRef, Method),
    (duplicate(OldRef, NewRef), 
     retract(applyT(ID, Parent, Encl, Expr, Name, OldRef, Method)),
     assert(applyT(ID, Parent, Encl, Expr, Name, NewRef, Method))
     ; true).
% applyT(ID,Parent,Encl,Expr,Name,ArgList,Method)
% Method
replaceRefInID(ID) :-
    applyT(ID, Parent, Encl, Expr, Name, ArgList, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(applyT(ID, Parent, Encl, Expr, Name, ArgList, OldRef)),
     assert(applyT(ID, Parent, Encl, Expr, Name, ArgList, NewRef))
     ; true).

% assertT(ID,Parent,Encl,Condition,Msg)
% Parent
replaceRefInID(ID) :-
    assertT(ID, OldRef, Encl, Condition, Msg),
    (duplicate(OldRef, NewRef), 
     retract(assertT(ID, OldRef, Encl, Condition, Msg)),
     assert(assertT(ID, NewRef, Encl, Condition, Msg))
     ; true).
% assertT(ID,Parent,Encl,Condition,Msg)
% Encl
replaceRefInID(ID) :-
    assertT(ID, Parent, OldRef, Condition, Msg),
    (duplicate(OldRef, NewRef), 
     retract(assertT(ID, Parent, OldRef, Condition, Msg)),
     assert(assertT(ID, Parent, NewRef, Condition, Msg))
     ; true).
% assertT(ID,Parent,Encl,Condition,Msg)
% Condition
replaceRefInID(ID) :-
    assertT(ID, Parent, Encl, OldRef, Msg),
    (duplicate(OldRef, NewRef), 
     retract(assertT(ID, Parent, Encl, OldRef, Msg)),
     assert(assertT(ID, Parent, Encl, NewRef, Msg))
     ; true).
% assertT(ID,Parent,Encl,Condition,Msg)
% Msg

% assignopT(ID,Parent,Encl,Lhs,Operator,Rhs)
% Parent
replaceRefInID(ID) :-
    assignopT(ID, OldRef, Encl, Lhs, Operator, Rhs),
    (duplicate(OldRef, NewRef), 
     retract(assignopT(ID, OldRef, Encl, Lhs, Operator, Rhs)),
     assert(assignopT(ID, NewRef, Encl, Lhs, Operator, Rhs))
     ; true).
% assignopT(ID,Parent,Encl,Lhs,Operator,Rhs)
% Encl
replaceRefInID(ID) :-
    assignopT(ID, Parent, OldRef, Lhs, Operator, Rhs),
    (duplicate(OldRef, NewRef), 
     retract(assignopT(ID, Parent, OldRef, Lhs, Operator, Rhs)),
     assert(assignopT(ID, Parent, NewRef, Lhs, Operator, Rhs))
     ; true).
% assignopT(ID,Parent,Encl,Lhs,Operator,Rhs)
% Lhs
replaceRefInID(ID) :-
    assignopT(ID, Parent, Encl, OldRef, Operator, Rhs),
    (duplicate(OldRef, NewRef), 
     retract(assignopT(ID, Parent, Encl, OldRef, Operator, Rhs)),
     assert(assignopT(ID, Parent, Encl, NewRef, Operator, Rhs))
     ; true).
% assignopT(ID,Parent,Encl,Lhs,Operator,Rhs)
% Operator
% assignopT(ID,Parent,Encl,Lhs,Operator,Rhs)
% Rhs
replaceRefInID(ID) :-
    assignopT(ID, Parent, Encl, Lhs, Operator, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(assignopT(ID, Parent, Encl, Lhs, Operator, OldRef)),
     assert(assignopT(ID, Parent, Encl, Lhs, Operator, NewRef))
     ; true).

% assignT(ID,Parent,Encl,Lhs,Rhs)
% Parent
replaceRefInID(ID) :-
    assignT(ID, OldRef, Encl, Lhs, Rhs),
    (duplicate(OldRef, NewRef), 
     retract(assignT(ID, OldRef, Encl, Lhs, Rhs)),
     assert(assignT(ID, NewRef, Encl, Lhs, Rhs))
     ; true).
% assignT(ID,Parent,Encl,Lhs,Rhs)
% Encl
replaceRefInID(ID) :-
    assignT(ID, Parent, OldRef, Lhs, Rhs),
    (duplicate(OldRef, NewRef), 
     retract(assignT(ID, Parent, OldRef, Lhs, Rhs)),
     assert(assignT(ID, Parent, NewRef, Lhs, Rhs))
     ; true).
% assignT(ID,Parent,Encl,Lhs,Rhs)
% Lhs
replaceRefInID(ID) :-
    assignT(ID, Parent, Encl, OldRef, Rhs),
    (duplicate(OldRef, NewRef), 
     retract(assignT(ID, Parent, Encl, OldRef, Rhs)),
     assert(assignT(ID, Parent, Encl, NewRef, Rhs))
     ; true).
% assignT(ID,Parent,Encl,Lhs,Rhs)
% Rhs
replaceRefInID(ID) :-
    assignT(ID, Parent, Encl, Lhs, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(assignT(ID, Parent, Encl, Lhs, OldRef)),
     assert(assignT(ID, Parent, Encl, Lhs, NewRef))
     ; true).

% blockT(ID,Parent,Encl,StatementsList)
% Parent
replaceRefInID(ID) :-
    blockT(ID, OldRef, Encl, StatementsList),
    (duplicate(OldRef, NewRef), 
     retract(blockT(ID, OldRef, Encl, StatementsList)),
     assert(blockT(ID, NewRef, Encl, StatementsList))
     ; true).
% blockT(ID,Parent,Encl,StatementsList)
% Encl
replaceRefInID(ID) :-
    blockT(ID, Parent, OldRef, StatementsList),
    (duplicate(OldRef, NewRef), 
     retract(blockT(ID, Parent, OldRef, StatementsList)),
     assert(blockT(ID, Parent, NewRef, StatementsList))
     ; true).
% blockT(ID,Parent,Encl,StatementsList)
% StatementsList
replaceRefInID(ID) :-
    blockT(ID, Parent, Encl, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(blockT(ID, Parent, Encl, OldRef)),
     assert(blockT(ID, Parent, Encl, NewRef))
     ; true).

% breakT(ID,Parent,Encl,Label,Target)
% Parent
replaceRefInID(ID) :-
    breakT(ID, OldRef, Encl, Label, Target),
    (duplicate(OldRef, NewRef), 
     retract(breakT(ID, OldRef, Encl, Label, Target)),
     assert(breakT(ID, NewRef, Encl, Label, Target))
     ; true).
% breakT(ID,Parent,Encl,Label,Target)
% Encl
replaceRefInID(ID) :-
    breakT(ID, Parent, OldRef, Label, Target),
    (duplicate(OldRef, NewRef), 
     retract(breakT(ID, Parent, OldRef, Label, Target)),
     assert(breakT(ID, Parent, NewRef, Label, Target))
     ; true).
% breakT(ID,Parent,Encl,Label,Target)
% Label
% breakT(ID,Parent,Encl,Label,Target)
% Target
replaceRefInID(ID) :-
    breakT(ID, Parent, Encl, Label, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(breakT(ID, Parent, Encl, Label, OldRef)),
     assert(breakT(ID, Parent, Encl, Label, NewRef))
     ; true).

% caseT(ID,Parent,Encl,Label)
% Parent
replaceRefInID(ID) :-
    caseT(ID, OldRef, Encl, Label),
    (duplicate(OldRef, NewRef), 
     retract(caseT(ID, OldRef, Encl, Label)),
     assert(caseT(ID, NewRef, Encl, Label))
     ; true).
% caseT(ID,Parent,Encl,Label)
% Encl
replaceRefInID(ID) :-
    caseT(ID, Parent, OldRef, Label),
    (duplicate(OldRef, NewRef), 
     retract(caseT(ID, Parent, OldRef, Label)),
     assert(caseT(ID, Parent, NewRef, Label))
     ; true).
% caseT(ID,Parent,Encl,Label)
% Label
replaceRefInID(ID) :-
    caseT(ID, Parent, Encl, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(caseT(ID, Parent, Encl, OldRef)),
     assert(caseT(ID, Parent, Encl, NewRef))
     ; true).

% catchT(ID,Parent,Encl,Param,Body)
% Parent
replaceRefInID(ID) :-
    catchT(ID, OldRef, Encl, Param, Body),
    (duplicate(OldRef, NewRef), 
     retract(catchT(ID, OldRef, Encl, Param, Body)),
     assert(catchT(ID, NewRef, Encl, Param, Body))
     ; true).
% catchT(ID,Parent,Encl,Param,Body)
% Encl
replaceRefInID(ID) :-
    catchT(ID, Parent, OldRef, Param, Body),
    (duplicate(OldRef, NewRef), 
     retract(catchT(ID, Parent, OldRef, Param, Body)),
     assert(catchT(ID, Parent, NewRef, Param, Body))
     ; true).
% catchT(ID,Parent,Encl,Param,Body)
% Param
replaceRefInID(ID) :-
    catchT(ID, Parent, Encl, OldRef, Body),
    (duplicate(OldRef, NewRef), 
     retract(catchT(ID, Parent, Encl, OldRef, Body)),
     assert(catchT(ID, Parent, Encl, NewRef, Body))
     ; true).
% catchT(ID,Parent,Encl,Param,Body)
% Body
replaceRefInID(ID) :-
    catchT(ID, Parent, Encl, Param, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(catchT(ID, Parent, Encl, Param, OldRef)),
     assert(catchT(ID, Parent, Encl, Param, NewRef))
     ; true).

% conditionalT(ID,Parent,Encl,Condition,ThenPart,ElsePart)
% Parent
replaceRefInID(ID) :-
    conditionalT(ID, OldRef, Encl, Condition, ThenPart, ElsePart),
    (duplicate(OldRef, NewRef), 
     retract(conditionalT(ID, OldRef, Encl, Condition, ThenPart, ElsePart)),
     assert(conditionalT(ID, NewRef, Encl, Condition, ThenPart, ElsePart))
     ; true).
% conditionalT(ID,Parent,Encl,Condition,ThenPart,ElsePart)
% Encl
replaceRefInID(ID) :-
    conditionalT(ID, Parent, OldRef, Condition, ThenPart, ElsePart),
    (duplicate(OldRef, NewRef), 
     retract(conditionalT(ID, Parent, OldRef, Condition, ThenPart, ElsePart)),
     assert(conditionalT(ID, Parent, NewRef, Condition, ThenPart, ElsePart))
     ; true).
% conditionalT(ID,Parent,Encl,Condition,ThenPart,ElsePart)
% Condition
replaceRefInID(ID) :-
    conditionalT(ID, Parent, Encl, OldRef, ThenPart, ElsePart),
    (duplicate(OldRef, NewRef), 
     retract(conditionalT(ID, Parent, Encl, OldRef, ThenPart, ElsePart)),
     assert(conditionalT(ID, Parent, Encl, NewRef, ThenPart, ElsePart))
     ; true).
% conditionalT(ID,Parent,Encl,Condition,ThenPart,ElsePart)
% ThenPart
replaceRefInID(ID) :-
    conditionalT(ID, Parent, Encl, Condition, OldRef, ElsePart),
    (duplicate(OldRef, NewRef), 
     retract(conditionalT(ID, Parent, Encl, Condition, OldRef, ElsePart)),
     assert(conditionalT(ID, Parent, Encl, Condition, NewRef, ElsePart))
     ; true).
% conditionalT(ID,Parent,Encl,Condition,ThenPart,ElsePart)
% ElsePart
replaceRefInID(ID) :-
    conditionalT(ID, Parent, Encl, Condition, ThenPart, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(conditionalT(ID, Parent, Encl, Condition, ThenPart, OldRef)),
     assert(conditionalT(ID, Parent, Encl, Condition, ThenPart, NewRef))
     ; true).

% continueT(ID,Parent,Encl,Label,Target)
% Parent
replaceRefInID(ID) :-
    continueT(ID, OldRef, Encl, Label, Target),
    (duplicate(OldRef, NewRef), 
     retract(continueT(ID, OldRef, Encl, Label, Target)),
     assert(continueT(ID, NewRef, Encl, Label, Target))
     ; true).
% continueT(ID,Parent,Encl,Label,Target)
% Encl
replaceRefInID(ID) :-
    continueT(ID, Parent, OldRef, Label, Target),
    (duplicate(OldRef, NewRef), 
     retract(continueT(ID, Parent, OldRef, Label, Target)),
     assert(continueT(ID, Parent, NewRef, Label, Target))
     ; true).
% continueT(ID,Parent,Encl,Label,Target)
% Label
% continueT(ID,Parent,Encl,Label,Target)
% Target
replaceRefInID(ID) :-
    continueT(ID, Parent, Encl, Label, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(continueT(ID, Parent, Encl, Label, OldRef)),
     assert(continueT(ID, Parent, Encl, Label, NewRef))
     ; true).

% doLoopT(ID,Parent,Encl,Condition,Body)
% Parent
replaceRefInID(ID) :-
    doLoopT(ID, OldRef, Encl, Condition, Body),
    (duplicate(OldRef, NewRef), 
     retract(doLoopT(ID, OldRef, Encl, Condition, Body)),
     assert(doLoopT(ID, NewRef, Encl, Condition, Body))
     ; true).
% doLoopT(ID,Parent,Encl,Condition,Body)
% Encl
replaceRefInID(ID) :-
    doLoopT(ID, Parent, OldRef, Condition, Body),
    (duplicate(OldRef, NewRef), 
     retract(doLoopT(ID, Parent, OldRef, Condition, Body)),
     assert(doLoopT(ID, Parent, NewRef, Condition, Body))
     ; true).
% doLoopT(ID,Parent,Encl,Condition,Body)
% Condition
replaceRefInID(ID) :-
    doLoopT(ID, Parent, Encl, OldRef, Body),
    (duplicate(OldRef, NewRef), 
     retract(doLoopT(ID, Parent, Encl, OldRef, Body)),
     assert(doLoopT(ID, Parent, Encl, NewRef, Body))
     ; true).
% doLoopT(ID,Parent,Encl,Condition,Body)
% Body
replaceRefInID(ID) :-
    doLoopT(ID, Parent, Encl, Condition, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(doLoopT(ID, Parent, Encl, Condition, OldRef)),
     assert(doLoopT(ID, Parent, Encl, Condition, NewRef))
     ; true).

% execT(ID,Parent,Encl,Expr)
% Parent
replaceRefInID(ID) :-
    execT(ID, OldRef, Encl, Expr),
    (duplicate(OldRef, NewRef), 
     retract(execT(ID, OldRef, Encl, Expr)),
     assert(execT(ID, NewRef, Encl, Expr))
     ; true).
% execT(ID,Parent,Encl,Expr)
% Encl
replaceRefInID(ID) :-
    execT(ID, Parent, OldRef, Expr),
    (duplicate(OldRef, NewRef), 
     retract(execT(ID, Parent, OldRef, Expr)),
     assert(execT(ID, Parent, NewRef, Expr))
     ; true).
% execT(ID,Parent,Encl,Expr)
% Expr
replaceRefInID(ID) :-
    execT(ID, Parent, Encl, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(execT(ID, Parent, Encl, OldRef)),
     assert(execT(ID, Parent, Encl, NewRef))
     ; true).

% forLoopT(ID,Parent,Encl,InitList,Condition,UpdateList,Body)
% Parent
replaceRefInID(ID) :-
    forLoopT(ID, OldRef, Encl, InitList, Condition, UpdateList, Body),
    (duplicate(OldRef, NewRef), 
     retract(forLoopT(ID, OldRef, Encl, InitList, Condition, UpdateList, Body)),
     assert(forLoopT(ID, NewRef, Encl, InitList, Condition, UpdateList, Body))
     ; true).
% forLoopT(ID,Parent,Encl,InitList,Condition,UpdateList,Body)
% Encl
replaceRefInID(ID) :-
    forLoopT(ID, Parent, OldRef, InitList, Condition, UpdateList, Body),
    (duplicate(OldRef, NewRef), 
     retract(forLoopT(ID, Parent, OldRef, InitList, Condition, UpdateList, Body)),
     assert(forLoopT(ID, Parent, NewRef, InitList, Condition, UpdateList, Body))
     ; true).
% forLoopT(ID,Parent,Encl,InitList,Condition,UpdateList,Body)
% InitList
replaceRefInID(ID) :-
    forLoopT(ID, Parent, Encl, OldRef, Condition, UpdateList, Body),
    (duplicate(OldRef, NewRef), 
     retract(forLoopT(ID, Parent, Encl, OldRef, Condition, UpdateList, Body)),
     assert(forLoopT(ID, Parent, Encl, NewRef, Condition, UpdateList, Body))
     ; true).
% forLoopT(ID,Parent,Encl,InitList,Condition,UpdateList,Body)
% Condition
replaceRefInID(ID) :-
    forLoopT(ID, Parent, Encl, InitList, OldRef, UpdateList, Body),
    (duplicate(OldRef, NewRef), 
     retract(forLoopT(ID, Parent, Encl, InitList, OldRef, UpdateList, Body)),
     assert(forLoopT(ID, Parent, Encl, InitList, NewRef, UpdateList, Body))
     ; true).
% forLoopT(ID,Parent,Encl,InitList,Condition,UpdateList,Body)
% UpdateList
replaceRefInID(ID) :-
    forLoopT(ID, Parent, Encl, InitList, Condition, OldRef, Body),
    (duplicate(OldRef, NewRef), 
     retract(forLoopT(ID, Parent, Encl, InitList, Condition, OldRef, Body)),
     assert(forLoopT(ID, Parent, Encl, InitList, Condition, NewRef, Body))
     ; true).
% forLoopT(ID,Parent,Encl,InitList,Condition,UpdateList,Body)
% Body
replaceRefInID(ID) :-
    forLoopT(ID, Parent, Encl, InitList, Condition, UpdateList, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(forLoopT(ID, Parent, Encl, InitList, Condition, UpdateList, OldRef)),
     assert(forLoopT(ID, Parent, Encl, InitList, Condition, UpdateList, NewRef))
     ; true).

% getFieldT(ID,Parent,Encl,Expr,Name,Field)
% Parent
replaceRefInID(ID) :-
    getFieldT(ID, OldRef, Encl, Expr, Name, Field),
    (duplicate(OldRef, NewRef), 
     retract(getFieldT(ID, OldRef, Encl, Expr, Name, Field)),
     assert(getFieldT(ID, NewRef, Encl, Expr, Name, Field))
     ; true).
% getFieldT(ID,Parent,Encl,Expr,Name,Field)
% Encl
replaceRefInID(ID) :-
    getFieldT(ID, Parent, OldRef, Expr, Name, Field),
    (duplicate(OldRef, NewRef), 
     retract(getFieldT(ID, Parent, OldRef, Expr, Name, Field)),
     assert(getFieldT(ID, Parent, NewRef, Expr, Name, Field))
     ; true).
% getFieldT(ID,Parent,Encl,Expr,Name,Field)
% Expr
replaceRefInID(ID) :-
    getFieldT(ID, Parent, Encl, OldRef, Name, Field),
    (duplicate(OldRef, NewRef), 
     retract(getFieldT(ID, Parent, Encl, OldRef, Name, Field)),
     assert(getFieldT(ID, Parent, Encl, NewRef, Name, Field))
     ; true).
% getFieldT(ID,Parent,Encl,Expr,Name,Field)
% Name
% getFieldT(ID,Parent,Encl,Expr,Name,Field)
% Field
replaceRefInID(ID) :-
    getFieldT(ID, Parent, Encl, Expr, Name, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(getFieldT(ID, Parent, Encl, Expr, Name, OldRef)),
     assert(getFieldT(ID, Parent, Encl, Expr, Name, NewRef))
     ; true).

% identT(ID,Parent,Encl,Name,Symbol)
% Parent
replaceRefInID(ID) :-
    identT(ID, OldRef, Encl, Name, Symbol),
    (duplicate(OldRef, NewRef), 
     retract(identT(ID, OldRef, Encl, Name, Symbol)),
     assert(identT(ID, NewRef, Encl, Name, Symbol))
     ; true).
% identT(ID,Parent,Encl,Name,Symbol)
% Encl
replaceRefInID(ID) :-
    identT(ID, Parent, OldRef, Name, Symbol),
    (duplicate(OldRef, NewRef), 
     retract(identT(ID, Parent, OldRef, Name, Symbol)),
     assert(identT(ID, Parent, NewRef, Name, Symbol))
     ; true).
% identT(ID,Parent,Encl,Name,Symbol)
% Name
% identT(ID,Parent,Encl,Name,Symbol)
% Symbol
replaceRefInID(ID) :-
    identT(ID, Parent, Encl, Name, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(identT(ID, Parent, Encl, Name, OldRef)),
     assert(identT(ID, Parent, Encl, Name, NewRef))
     ; true).

% ifT(ID,Parent,Encl,Condition,ThenPart,ElsePart)
% Parent
replaceRefInID(ID) :-
    ifT(ID, OldRef, Encl, Condition, ThenPart, ElsePart),
    (duplicate(OldRef, NewRef), 
     retract(ifT(ID, OldRef, Encl, Condition, ThenPart, ElsePart)),
     assert(ifT(ID, NewRef, Encl, Condition, ThenPart, ElsePart))
     ; true).
% ifT(ID,Parent,Encl,Condition,ThenPart,ElsePart)
% Encl
replaceRefInID(ID) :-
    ifT(ID, Parent, OldRef, Condition, ThenPart, ElsePart),
    (duplicate(OldRef, NewRef), 
     retract(ifT(ID, Parent, OldRef, Condition, ThenPart, ElsePart)),
     assert(ifT(ID, Parent, NewRef, Condition, ThenPart, ElsePart))
     ; true).
% ifT(ID,Parent,Encl,Condition,ThenPart,ElsePart)
% Condition
replaceRefInID(ID) :-
    ifT(ID, Parent, Encl, OldRef, ThenPart, ElsePart),
    (duplicate(OldRef, NewRef), 
     retract(ifT(ID, Parent, Encl, OldRef, ThenPart, ElsePart)),
     assert(ifT(ID, Parent, Encl, NewRef, ThenPart, ElsePart))
     ; true).
% ifT(ID,Parent,Encl,Condition,ThenPart,ElsePart)
% ThenPart
replaceRefInID(ID) :-
    ifT(ID, Parent, Encl, Condition, OldRef, ElsePart),
    (duplicate(OldRef, NewRef), 
     retract(ifT(ID, Parent, Encl, Condition, OldRef, ElsePart)),
     assert(ifT(ID, Parent, Encl, Condition, NewRef, ElsePart))
     ; true).
% ifT(ID,Parent,Encl,Condition,ThenPart,ElsePart)
% ElsePart
replaceRefInID(ID) :-
    ifT(ID, Parent, Encl, Condition, ThenPart, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(ifT(ID, Parent, Encl, Condition, ThenPart, OldRef)),
     assert(ifT(ID, Parent, Encl, Condition, ThenPart, NewRef))
     ; true).

% indexedT(ID,Parent,Encl,Index,Indexed)
% Parent
replaceRefInID(ID) :-
    indexedT(ID, OldRef, Encl, Index, Indexed),
    (duplicate(OldRef, NewRef), 
     retract(indexedT(ID, OldRef, Encl, Index, Indexed)),
     assert(indexedT(ID, NewRef, Encl, Index, Indexed))
     ; true).
% indexedT(ID,Parent,Encl,Index,Indexed)
% Encl
replaceRefInID(ID) :-
    indexedT(ID, Parent, OldRef, Index, Indexed),
    (duplicate(OldRef, NewRef), 
     retract(indexedT(ID, Parent, OldRef, Index, Indexed)),
     assert(indexedT(ID, Parent, NewRef, Index, Indexed))
     ; true).
% indexedT(ID,Parent,Encl,Index,Indexed)
% Index
replaceRefInID(ID) :-
    indexedT(ID, Parent, Encl, OldRef, Indexed),
    (duplicate(OldRef, NewRef), 
     retract(indexedT(ID, Parent, Encl, OldRef, Indexed)),
     assert(indexedT(ID, Parent, Encl, NewRef, Indexed))
     ; true).
% indexedT(ID,Parent,Encl,Index,Indexed)
% Indexed
replaceRefInID(ID) :-
    indexedT(ID, Parent, Encl, Index, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(indexedT(ID, Parent, Encl, Index, OldRef)),
     assert(indexedT(ID, Parent, Encl, Index, NewRef))
     ; true).

% labelT(ID,Parent,Encl,Body,Name)
% Parent
replaceRefInID(ID) :-
    labelT(ID, OldRef, Encl, Body, Name),
    (duplicate(OldRef, NewRef), 
     retract(labelT(ID, OldRef, Encl, Body, Name)),
     assert(labelT(ID, NewRef, Encl, Body, Name))
     ; true).
% labelT(ID,Parent,Encl,Body,Name)
% Encl
replaceRefInID(ID) :-
    labelT(ID, Parent, OldRef, Body, Name),
    (duplicate(OldRef, NewRef), 
     retract(labelT(ID, Parent, OldRef, Body, Name)),
     assert(labelT(ID, Parent, NewRef, Body, Name))
     ; true).
% labelT(ID,Parent,Encl,Body,Name)
% Body
replaceRefInID(ID) :-
    labelT(ID, Parent, Encl, OldRef, Name),
    (duplicate(OldRef, NewRef), 
     retract(labelT(ID, Parent, Encl, OldRef, Name)),
     assert(labelT(ID, Parent, Encl, NewRef, Name))
     ; true).
% labelT(ID,Parent,Encl,Body,Name)
% Name

% literalT(ID,Parent,Encl,Type,Value)
% Parent
replaceRefInID(ID) :-
    literalT(ID, OldRef, Encl, Type, Value),
    (duplicate(OldRef, NewRef), 
     retract(literalT(ID, OldRef, Encl, Type, Value)),
     assert(literalT(ID, NewRef, Encl, Type, Value))
     ; true).
% literalT(ID,Parent,Encl,Type,Value)
% Encl
replaceRefInID(ID) :-
    literalT(ID, Parent, OldRef, Type, Value),
    (duplicate(OldRef, NewRef), 
     retract(literalT(ID, Parent, OldRef, Type, Value)),
     assert(literalT(ID, Parent, NewRef, Type, Value))
     ; true).
% literalT(ID,Parent,Encl,Type,Value)
% Type
replaceRefInID(ID) :-
    literalT(ID, Parent, Encl, OldRef, Value),
    (duplicate(OldRef, NewRef), 
     retract(literalT(ID, Parent, Encl, OldRef, Value)),
     assert(literalT(ID, Parent, Encl, NewRef, Value))
     ; true).
% literalT(ID,Parent,Encl,Type,Value)
% Value

% localDefT(ID,Parent,EnclClass,Type,Name,Init)
% Parent
replaceRefInID(ID) :-
    localDefT(ID, OldRef, EnclClass, Type, Name, Init),
    (duplicate(OldRef, NewRef), 
     retract(localDefT(ID, OldRef, EnclClass, Type, Name, Init)),
     assert(localDefT(ID, NewRef, EnclClass, Type, Name, Init))
     ; true).
% localDefT(ID,Parent,EnclClass,Type,Name,Init)
% EnclClass
replaceRefInID(ID) :-
    localDefT(ID, Parent, OldRef, Type, Name, Init),
    (duplicate(OldRef, NewRef), 
     retract(localDefT(ID, Parent, OldRef, Type, Name, Init)),
     assert(localDefT(ID, Parent, NewRef, Type, Name, Init))
     ; true).
% localDefT(ID,Parent,EnclClass,Type,Name,Init)
% Type
replaceRefInID(ID) :-
    localDefT(ID, Parent, EnclClass, OldRef, Name, Init),
    (duplicate(OldRef, NewRef), 
     retract(localDefT(ID, Parent, EnclClass, OldRef, Name, Init)),
     assert(localDefT(ID, Parent, EnclClass, NewRef, Name, Init))
     ; true).
% localDefT(ID,Parent,EnclClass,Type,Name,Init)
% Name
% localDefT(ID,Parent,EnclClass,Type,Name,Init)
% Init
replaceRefInID(ID) :-
    localDefT(ID, Parent, EnclClass, Type, Name, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(localDefT(ID, Parent, EnclClass, Type, Name, OldRef)),
     assert(localDefT(ID, Parent, EnclClass, Type, Name, NewRef))
     ; true).

% newArrayT(ID,Parent,Encl,DimList,ElemList,Type)
% Parent
replaceRefInID(ID) :-
    newArrayT(ID, OldRef, Encl, DimList, ElemList, Type),
    (duplicate(OldRef, NewRef), 
     retract(newArrayT(ID, OldRef, Encl, DimList, ElemList, Type)),
     assert(newArrayT(ID, NewRef, Encl, DimList, ElemList, Type))
     ; true).
% newArrayT(ID,Parent,Encl,DimList,ElemList,Type)
% Encl
replaceRefInID(ID) :-
    newArrayT(ID, Parent, OldRef, DimList, ElemList, Type),
    (duplicate(OldRef, NewRef), 
     retract(newArrayT(ID, Parent, OldRef, DimList, ElemList, Type)),
     assert(newArrayT(ID, Parent, NewRef, DimList, ElemList, Type))
     ; true).
% newArrayT(ID,Parent,Encl,DimList,ElemList,Type)
% DimList
replaceRefInID(ID) :-
    newArrayT(ID, Parent, Encl, OldRef, ElemList, Type),
    (duplicate(OldRef, NewRef), 
     retract(newArrayT(ID, Parent, Encl, OldRef, ElemList, Type)),
     assert(newArrayT(ID, Parent, Encl, NewRef, ElemList, Type))
     ; true).
% newArrayT(ID,Parent,Encl,DimList,ElemList,Type)
% ElemList
replaceRefInID(ID) :-
    newArrayT(ID, Parent, Encl, DimList, OldRef, Type),
    (duplicate(OldRef, NewRef), 
     retract(newArrayT(ID, Parent, Encl, DimList, OldRef, Type)),
     assert(newArrayT(ID, Parent, Encl, DimList, NewRef, Type))
     ; true).
% newArrayT(ID,Parent,Encl,DimList,ElemList,Type)
% Type
replaceRefInID(ID) :-
    newArrayT(ID, Parent, Encl, DimList, ElemList, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(newArrayT(ID, Parent, Encl, DimList, ElemList, OldRef)),
     assert(newArrayT(ID, Parent, Encl, DimList, ElemList, NewRef))
     ; true).

% newClassT(ID,Parent,Encl,Constructor,ArgList,TypeExpr,Def,Enclosing)
% Parent
replaceRefInID(ID) :-
    newClassT(ID, OldRef, Encl, Constructor, ArgList, TypeExpr, Def, Enclosing),
    (duplicate(OldRef, NewRef), 
     retract(newClassT(ID, OldRef, Encl, Constructor, ArgList, TypeExpr, Def, Enclosing)),
     assert(newClassT(ID, NewRef, Encl, Constructor, ArgList, TypeExpr, Def, Enclosing))
     ; true).
% newClassT(ID,Parent,Encl,Constructor,ArgList,TypeExpr,Def,Enclosing)
% Encl
replaceRefInID(ID) :-
    newClassT(ID, Parent, OldRef, Constructor, ArgList, TypeExpr, Def, Enclosing),
    (duplicate(OldRef, NewRef), 
     retract(newClassT(ID, Parent, OldRef, Constructor, ArgList, TypeExpr, Def, Enclosing)),
     assert(newClassT(ID, Parent, NewRef, Constructor, ArgList, TypeExpr, Def, Enclosing))
     ; true).
% newClassT(ID,Parent,Encl,Constructor,ArgList,TypeExpr,Def,Enclosing)
% Constructor
replaceRefInID(ID) :-
    newClassT(ID, Parent, Encl, OldRef, ArgList, TypeExpr, Def, Enclosing),
    (duplicate(OldRef, NewRef), 
     retract(newClassT(ID, Parent, Encl, OldRef, ArgList, TypeExpr, Def, Enclosing)),
     assert(newClassT(ID, Parent, Encl, NewRef, ArgList, TypeExpr, Def, Enclosing))
     ; true).
% newClassT(ID,Parent,Encl,Constructor,ArgList,TypeExpr,Def,Enclosing)
% ArgList
replaceRefInID(ID) :-
    newClassT(ID, Parent, Encl, Constructor, OldRef, TypeExpr, Def, Enclosing),
    (duplicate(OldRef, NewRef), 
     retract(newClassT(ID, Parent, Encl, Constructor, OldRef, TypeExpr, Def, Enclosing)),
     assert(newClassT(ID, Parent, Encl, Constructor, NewRef, TypeExpr, Def, Enclosing))
     ; true).
% newClassT(ID,Parent,Encl,Constructor,ArgList,TypeExpr,Def,Enclosing)
% TypeExpr
replaceRefInID(ID) :-
    newClassT(ID, Parent, Encl, Constructor, ArgList, OldRef, Def, Enclosing),
    (duplicate(OldRef, NewRef), 
     retract(newClassT(ID, Parent, Encl, Constructor, ArgList, OldRef, Def, Enclosing)),
     assert(newClassT(ID, Parent, Encl, Constructor, ArgList, NewRef, Def, Enclosing))
     ; true).
% newClassT(ID,Parent,Encl,Constructor,ArgList,TypeExpr,Def,Enclosing)
% Def
replaceRefInID(ID) :-
    newClassT(ID, Parent, Encl, Constructor, ArgList, TypeExpr, OldRef, Enclosing),
    (duplicate(OldRef, NewRef), 
     retract(newClassT(ID, Parent, Encl, Constructor, ArgList, TypeExpr, OldRef, Enclosing)),
     assert(newClassT(ID, Parent, Encl, Constructor, ArgList, TypeExpr, NewRef, Enclosing))
     ; true).
% newClassT(ID,Parent,Encl,Constructor,ArgList,TypeExpr,Def,Enclosing)
% Enclosing
replaceRefInID(ID) :-
    newClassT(ID, Parent, Encl, Constructor, ArgList, TypeExpr, Def, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(newClassT(ID, Parent, Encl, Constructor, ArgList, TypeExpr, Def, OldRef)),
     assert(newClassT(ID, Parent, Encl, Constructor, ArgList, TypeExpr, Def, NewRef))
     ; true).

% nopT(ID,Parent,Encl)
% Parent
replaceRefInID(ID) :-
    nopT(ID, OldRef, Encl),
    (duplicate(OldRef, NewRef), 
     retract(nopT(ID, OldRef, Encl)),
     assert(nopT(ID, NewRef, Encl))
     ; true).
% nopT(ID,Parent,Encl)
% Encl
replaceRefInID(ID) :-
    nopT(ID, Parent, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(nopT(ID, Parent, OldRef)),
     assert(nopT(ID, Parent, NewRef))
     ; true).

% operationT(ID,Parent,Encl,ArgList,OperatorName,Pos)
% Parent
replaceRefInID(ID) :-
    operationT(ID, OldRef, Encl, ArgList, OperatorName, Pos),
    (duplicate(OldRef, NewRef), 
     retract(operationT(ID, OldRef, Encl, ArgList, OperatorName, Pos)),
     assert(operationT(ID, NewRef, Encl, ArgList, OperatorName, Pos))
     ; true).
% operationT(ID,Parent,Encl,ArgList,OperatorName,Pos)
% Encl
replaceRefInID(ID) :-
    operationT(ID, Parent, OldRef, ArgList, OperatorName, Pos),
    (duplicate(OldRef, NewRef), 
     retract(operationT(ID, Parent, OldRef, ArgList, OperatorName, Pos)),
     assert(operationT(ID, Parent, NewRef, ArgList, OperatorName, Pos))
     ; true).
% operationT(ID,Parent,Encl,ArgList,OperatorName,Pos)
% ArgList
replaceRefInID(ID) :-
    operationT(ID, Parent, Encl, OldRef, OperatorName, Pos),
    (duplicate(OldRef, NewRef), 
     retract(operationT(ID, Parent, Encl, OldRef, OperatorName, Pos)),
     assert(operationT(ID, Parent, Encl, NewRef, OperatorName, Pos))
     ; true).
% operationT(ID,Parent,Encl,ArgList,OperatorName,Pos)
% OperatorName
% operationT(ID,Parent,Encl,ArgList,OperatorName,Pos)
% Pos

% precedenceT(ID,Parent,Encl,Expr)
% Parent
replaceRefInID(ID) :-
    precedenceT(ID, OldRef, Encl, Expr),
    (duplicate(OldRef, NewRef), 
     retract(precedenceT(ID, OldRef, Encl, Expr)),
     assert(precedenceT(ID, NewRef, Encl, Expr))
     ; true).
% precedenceT(ID,Parent,Encl,Expr)
% Encl
replaceRefInID(ID) :-
    precedenceT(ID, Parent, OldRef, Expr),
    (duplicate(OldRef, NewRef), 
     retract(precedenceT(ID, Parent, OldRef, Expr)),
     assert(precedenceT(ID, Parent, NewRef, Expr))
     ; true).
% precedenceT(ID,Parent,Encl,Expr)
% Expr
replaceRefInID(ID) :-
    precedenceT(ID, Parent, Encl, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(precedenceT(ID, Parent, Encl, OldRef)),
     assert(precedenceT(ID, Parent, Encl, NewRef))
     ; true).

% returnT(ID,Parent,Encl,Exec)
% Parent
replaceRefInID(ID) :-
    returnT(ID, OldRef, Encl, Exec),
    (duplicate(OldRef, NewRef), 
     retract(returnT(ID, OldRef, Encl, Exec)),
     assert(returnT(ID, NewRef, Encl, Exec))
     ; true).
% returnT(ID,Parent,Encl,Exec)
% Encl
replaceRefInID(ID) :-
    returnT(ID, Parent, OldRef, Exec),
    (duplicate(OldRef, NewRef), 
     retract(returnT(ID, Parent, OldRef, Exec)),
     assert(returnT(ID, Parent, NewRef, Exec))
     ; true).
% returnT(ID,Parent,Encl,Exec)
% Exec
replaceRefInID(ID) :-
    returnT(ID, Parent, Encl, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(returnT(ID, Parent, Encl, OldRef)),
     assert(returnT(ID, Parent, Encl, NewRef))
     ; true).

% selectT(ID,Parent,Encl,Name,Selected,Symbol)
% Parent
replaceRefInID(ID) :-
    selectT(ID, OldRef, Encl, Name, Selected, Symbol),
    (duplicate(OldRef, NewRef), 
     retract(selectT(ID, OldRef, Encl, Name, Selected, Symbol)),
     assert(selectT(ID, NewRef, Encl, Name, Selected, Symbol))
     ; true).
% selectT(ID,Parent,Encl,Name,Selected,Symbol)
% Encl
replaceRefInID(ID) :-
    selectT(ID, Parent, OldRef, Name, Selected, Symbol),
    (duplicate(OldRef, NewRef), 
     retract(selectT(ID, Parent, OldRef, Name, Selected, Symbol)),
     assert(selectT(ID, Parent, NewRef, Name, Selected, Symbol))
     ; true).
% selectT(ID,Parent,Encl,Name,Selected,Symbol)
% Name
replaceRefInID(ID) :-
    selectT(ID, Parent, Encl, OldRef, Selected, Symbol),
    (duplicate(OldRef, NewRef), 
     retract(selectT(ID, Parent, Encl, OldRef, Selected, Symbol)),
     assert(selectT(ID, Parent, Encl, NewRef, Selected, Symbol))
     ; true).
% selectT(ID,Parent,Encl,Name,Selected,Symbol)
% Selected
replaceRefInID(ID) :-
    selectT(ID, Parent, Encl, Name, OldRef, Symbol),
    (duplicate(OldRef, NewRef), 
     retract(selectT(ID, Parent, Encl, Name, OldRef, Symbol)),
     assert(selectT(ID, Parent, Encl, Name, NewRef, Symbol))
     ; true).
% selectT(ID,Parent,Encl,Name,Selected,Symbol)
% Symbol
replaceRefInID(ID) :-
    selectT(ID, Parent, Encl, Name, Selected, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(selectT(ID, Parent, Encl, Name, Selected, OldRef)),
     assert(selectT(ID, Parent, Encl, Name, Selected, NewRef))
     ; true).

% switchT(ID,Parent,Encl,Condition,StatementsList)
% Parent
replaceRefInID(ID) :-
    switchT(ID, OldRef, Encl, Condition, StatementsList),
    (duplicate(OldRef, NewRef), 
     retract(switchT(ID, OldRef, Encl, Condition, StatementsList)),
     assert(switchT(ID, NewRef, Encl, Condition, StatementsList))
     ; true).
% switchT(ID,Parent,Encl,Condition,StatementsList)
% Encl
replaceRefInID(ID) :-
    switchT(ID, Parent, OldRef, Condition, StatementsList),
    (duplicate(OldRef, NewRef), 
     retract(switchT(ID, Parent, OldRef, Condition, StatementsList)),
     assert(switchT(ID, Parent, NewRef, Condition, StatementsList))
     ; true).
% switchT(ID,Parent,Encl,Condition,StatementsList)
% Condition
replaceRefInID(ID) :-
    switchT(ID, Parent, Encl, OldRef, StatementsList),
    (duplicate(OldRef, NewRef), 
     retract(switchT(ID, Parent, Encl, OldRef, StatementsList)),
     assert(switchT(ID, Parent, Encl, NewRef, StatementsList))
     ; true).
% switchT(ID,Parent,Encl,Condition,StatementsList)
% StatementsList
replaceRefInID(ID) :-
    switchT(ID, Parent, Encl, Condition, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(switchT(ID, Parent, Encl, Condition, OldRef)),
     assert(switchT(ID, Parent, Encl, Condition, NewRef))
     ; true).

% synchronizedT(ID,Parent,Encl,Lock,Body)
% Parent
replaceRefInID(ID) :-
    synchronizedT(ID, OldRef, Encl, Lock, Body),
    (duplicate(OldRef, NewRef), 
     retract(synchronizedT(ID, OldRef, Encl, Lock, Body)),
     assert(synchronizedT(ID, NewRef, Encl, Lock, Body))
     ; true).
% synchronizedT(ID,Parent,Encl,Lock,Body)
% Encl
replaceRefInID(ID) :-
    synchronizedT(ID, Parent, OldRef, Lock, Body),
    (duplicate(OldRef, NewRef), 
     retract(synchronizedT(ID, Parent, OldRef, Lock, Body)),
     assert(synchronizedT(ID, Parent, NewRef, Lock, Body))
     ; true).
% synchronizedT(ID,Parent,Encl,Lock,Body)
% Lock
replaceRefInID(ID) :-
    synchronizedT(ID, Parent, Encl, OldRef, Body),
    (duplicate(OldRef, NewRef), 
     retract(synchronizedT(ID, Parent, Encl, OldRef, Body)),
     assert(synchronizedT(ID, Parent, Encl, NewRef, Body))
     ; true).
% synchronizedT(ID,Parent,Encl,Lock,Body)
% Body
replaceRefInID(ID) :-
    synchronizedT(ID, Parent, Encl, Lock, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(synchronizedT(ID, Parent, Encl, Lock, OldRef)),
     assert(synchronizedT(ID, Parent, Encl, Lock, NewRef))
     ; true).

% throwT(ID,Parent,Encl,Exec)
% Parent
replaceRefInID(ID) :-
    throwT(ID, OldRef, Encl, Exec),
    (duplicate(OldRef, NewRef), 
     retract(throwT(ID, OldRef, Encl, Exec)),
     assert(throwT(ID, NewRef, Encl, Exec))
     ; true).
% throwT(ID,Parent,Encl,Exec)
% Encl
replaceRefInID(ID) :-
    throwT(ID, Parent, OldRef, Exec),
    (duplicate(OldRef, NewRef), 
     retract(throwT(ID, Parent, OldRef, Exec)),
     assert(throwT(ID, Parent, NewRef, Exec))
     ; true).
% throwT(ID,Parent,Encl,Exec)
% Exec
replaceRefInID(ID) :-
    throwT(ID, Parent, Encl, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(throwT(ID, Parent, Encl, OldRef)),
     assert(throwT(ID, Parent, Encl, NewRef))
     ; true).

% tryT(ID,Parent,Encl,Body,CatchersList,Finalizer)
% Parent
replaceRefInID(ID) :-
    tryT(ID, OldRef, Encl, Body, CatchersList, Finalizer),
    (duplicate(OldRef, NewRef), 
     retract(tryT(ID, OldRef, Encl, Body, CatchersList, Finalizer)),
     assert(tryT(ID, NewRef, Encl, Body, CatchersList, Finalizer))
     ; true).
% tryT(ID,Parent,Encl,Body,CatchersList,Finalizer)
% Encl
replaceRefInID(ID) :-
    tryT(ID, Parent, OldRef, Body, CatchersList, Finalizer),
    (duplicate(OldRef, NewRef), 
     retract(tryT(ID, Parent, OldRef, Body, CatchersList, Finalizer)),
     assert(tryT(ID, Parent, NewRef, Body, CatchersList, Finalizer))
     ; true).
% tryT(ID,Parent,Encl,Body,CatchersList,Finalizer)
% Body
replaceRefInID(ID) :-
    tryT(ID, Parent, Encl, OldRef, CatchersList, Finalizer),
    (duplicate(OldRef, NewRef), 
     retract(tryT(ID, Parent, Encl, OldRef, CatchersList, Finalizer)),
     assert(tryT(ID, Parent, Encl, NewRef, CatchersList, Finalizer))
     ; true).
% tryT(ID,Parent,Encl,Body,CatchersList,Finalizer)
% CatchersList
replaceRefInID(ID) :-
    tryT(ID, Parent, Encl, Body, OldRef, Finalizer),
    (duplicate(OldRef, NewRef), 
     retract(tryT(ID, Parent, Encl, Body, OldRef, Finalizer)),
     assert(tryT(ID, Parent, Encl, Body, NewRef, Finalizer))
     ; true).
% tryT(ID,Parent,Encl,Body,CatchersList,Finalizer)
% Finalizer
replaceRefInID(ID) :-
    tryT(ID, Parent, Encl, Body, CatchersList, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(tryT(ID, Parent, Encl, Body, CatchersList, OldRef)),
     assert(tryT(ID, Parent, Encl, Body, CatchersList, NewRef))
     ; true).

% typeCastT(ID,Parent,Encl,Type,Expr)
% Parent
replaceRefInID(ID) :-
    typeCastT(ID, OldRef, Encl, Type, Expr),
    (duplicate(OldRef, NewRef), 
     retract(typeCastT(ID, OldRef, Encl, Type, Expr)),
     assert(typeCastT(ID, NewRef, Encl, Type, Expr))
     ; true).
% typeCastT(ID,Parent,Encl,Type,Expr)
% Encl
replaceRefInID(ID) :-
    typeCastT(ID, Parent, OldRef, Type, Expr),
    (duplicate(OldRef, NewRef), 
     retract(typeCastT(ID, Parent, OldRef, Type, Expr)),
     assert(typeCastT(ID, Parent, NewRef, Type, Expr))
     ; true).
% typeCastT(ID,Parent,Encl,Type,Expr)
% Type
replaceRefInID(ID) :-
    typeCastT(ID, Parent, Encl, OldRef, Expr),
    (duplicate(OldRef, NewRef), 
     retract(typeCastT(ID, Parent, Encl, OldRef, Expr)),
     assert(typeCastT(ID, Parent, Encl, NewRef, Expr))
     ; true).
% typeCastT(ID,Parent,Encl,Type,Expr)
% Expr
replaceRefInID(ID) :-
    typeCastT(ID, Parent, Encl, Type, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(typeCastT(ID, Parent, Encl, Type, OldRef)),
     assert(typeCastT(ID, Parent, Encl, Type, NewRef))
     ; true).

% typeTestT(ID,Parent,Encl,Type,Expr)
% Parent
replaceRefInID(ID) :-
    typeTestT(ID, OldRef, Encl, Type, Expr),
    (duplicate(OldRef, NewRef), 
     retract(typeTestT(ID, OldRef, Encl, Type, Expr)),
     assert(typeTestT(ID, NewRef, Encl, Type, Expr))
     ; true).
% typeTestT(ID,Parent,Encl,Type,Expr)
% Encl
replaceRefInID(ID) :-
    typeTestT(ID, Parent, OldRef, Type, Expr),
    (duplicate(OldRef, NewRef), 
     retract(typeTestT(ID, Parent, OldRef, Type, Expr)),
     assert(typeTestT(ID, Parent, NewRef, Type, Expr))
     ; true).
% typeTestT(ID,Parent,Encl,Type,Expr)
% Type
replaceRefInID(ID) :-
    typeTestT(ID, Parent, Encl, OldRef, Expr),
    (duplicate(OldRef, NewRef), 
     retract(typeTestT(ID, Parent, Encl, OldRef, Expr)),
     assert(typeTestT(ID, Parent, Encl, NewRef, Expr))
     ; true).
% typeTestT(ID,Parent,Encl,Type,Expr)
% Expr
replaceRefInID(ID) :-
    typeTestT(ID, Parent, Encl, Type, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(typeTestT(ID, Parent, Encl, Type, OldRef)),
     assert(typeTestT(ID, Parent, Encl, Type, NewRef))
     ; true).

% whileLoopT(ID,Parent,Encl,Condition,Body)
% Parent
replaceRefInID(ID) :-
    whileLoopT(ID, OldRef, Encl, Condition, Body),
    (duplicate(OldRef, NewRef), 
     retract(whileLoopT(ID, OldRef, Encl, Condition, Body)),
     assert(whileLoopT(ID, NewRef, Encl, Condition, Body))
     ; true).
% whileLoopT(ID,Parent,Encl,Condition,Body)
% Encl
replaceRefInID(ID) :-
    whileLoopT(ID, Parent, OldRef, Condition, Body),
    (duplicate(OldRef, NewRef), 
     retract(whileLoopT(ID, Parent, OldRef, Condition, Body)),
     assert(whileLoopT(ID, Parent, NewRef, Condition, Body))
     ; true).
% whileLoopT(ID,Parent,Encl,Condition,Body)
% Condition
replaceRefInID(ID) :-
    whileLoopT(ID, Parent, Encl, OldRef, Body),
    (duplicate(OldRef, NewRef), 
     retract(whileLoopT(ID, Parent, Encl, OldRef, Body)),
     assert(whileLoopT(ID, Parent, Encl, NewRef, Body))
     ; true).
% whileLoopT(ID,Parent,Encl,Condition,Body)
% Body
replaceRefInID(ID) :-
    whileLoopT(ID, Parent, Encl, Condition, OldRef),
    (duplicate(OldRef, NewRef), 
     retract(whileLoopT(ID, Parent, Encl, Condition, OldRef)),
     assert(whileLoopT(ID, Parent, Encl, Condition, NewRef))
     ; true).

