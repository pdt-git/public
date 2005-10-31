%
% @author Bruno Harbulot <bruno@cs.man.ac.uk>
% @date 01 July 2004
%


nothing(_).


%walkTree(VisitorBefore, VisitorAfter, []) :- call(VisitorBefore,[]), call(VisitorAfter,[]).
%walkTree(VisitorBefore, VisitorAfter, [X | T]) :- call(VisitorBefore, [X | T]), call(VisitorAfter, [X | T]).
%walkTree(VisitorBefore, VisitorAfter, null) :- call(VisitorBefore, null), call(VisitorAfter, null).



walkTreeList(_, _, _, _, []).
walkTreeList(BeforeNode, AfterNode, BeforeList, AfterList, [X | T]) :-
	walkTree(BeforeNode, AfterNode, BeforeList, AfterList, X),
	walkTreeList(BeforeNode, AfterNode, BeforeList, AfterList, T).


walkTree(_, _, BeforeList, AfterList, []) :-
	call(BeforeList, []), call(AfterList, []).
	
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, [X | T]) :-
    call(BeforeList,[X | T]),
	walkTreeList(BeforeNode, AfterNode, BeforeList, AfterList, [X | T]),
	call(AfterList,[X | T]).



walkTree(BeforeNode, AfterNode, _, _, null) :- call(BeforeNode, null), call(AfterNode, null).


walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    applyT(ID, _Parent, _Encl, Expr, _Name, Arguments, _Method),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Expr), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Arguments),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    assertT(ID, _Parent, _Encl, Condition, Msg),
	call(BeforeNode, ID),
	walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Condition), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Msg),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    assignopT(ID, _Parent, _Encl, Lhs, _, Rhs),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Lhs), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Rhs),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    assignT(ID, _Parent, _Encl, Lhs, Rhs),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Lhs), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Rhs),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    blockT(ID, _Parent, _Encl, Statements),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Statements),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, _, _, ID) :-
    breakT(ID, _Parent, _Encl, _Label, _Target),
    call(BeforeNode, ID),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, _, _, ID) :-
    caseT(ID, _Parent, _Encl, _Label), 
    call(BeforeNode, ID),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    catchT(ID, _Parent, _Encl, Param, Body), 
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Param), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    conditionalT(ID, _Parent, _Encl, Condition, ThenPart, ElsePart),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Condition), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ThenPart), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ElsePart),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, _, _, ID) :-
    continueT(ID, _Parent, _Encl, _Label, _Target),
    call(BeforeNode, ID),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    doLoopT(ID, _Parent, _Encl, Condition, Body), 
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Condition), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    execT(ID, _Parent, _Encl, Expr),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    forLoopT(ID, _Parent, _Encl, Init, Condition, Update, Body),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Init), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Condition), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Update), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    getFieldT(ID, _Parent, _Encl, Expr, _Name, _Field),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, _, _, ID) :-
    identT(ID, _Parent, _Encl, _Name, _Symbol),
    call(BeforeNode, ID),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    ifT(ID, _Parent, _Encl, Condition, ThenPart, ElsePart),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Condition), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ThenPart), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ElsePart),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    indexedT(ID, _Parent, _Encl, Index, Indexed),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Index), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Indexed),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, _, _, ID) :-
    labelT(ID, _Parent, _Encl, _Body, _Name),
    call(BeforeNode, ID),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    literalT(ID, _Parent, _Encl, Type, _Value),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Type),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    localDefT(ID, _Parent, _Encl, Type, _Name, Init),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Type),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Init),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    newArrayT(ID, _Parent, _Encl, Dim, Elem, Type),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Dim),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Elem),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Type),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    newClassT(ID, _Parent, _Encl, _Constructor, Args, TypeExpr, Def, _Enclosing),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Args),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, TypeExpr),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList,Def),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, _, _, ID) :-
    nopT(ID, _Parent, _Encl),
    call(BeforeNode, ID),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    operationT(ID, _Parent, _Encl, Args, _OperatorName, _Pos),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Args),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    precedenceT(ID, _Parent, _Encl, Expr),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    returnT(ID, _Parent, _Encl, Expr),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, _, _, ID) :-
    selectT(ID, _Parent, _Encl, _Name, _Selected, _Symbol),
    call(BeforeNode, ID),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    switchT(ID, _Parent, _Encl, Condition, Statements),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Condition), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Statements),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    synchronizedT(ID, _Parent, _Encl, Lock, Body),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Lock), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    throwT(ID, _Parent, _Encl, Expr),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    tryT(ID, _Parent, _Encl, Body, Catchers, Finalizer),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Body), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Catchers), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Finalizer),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    typeCastT(ID, _Parent, _Encl, Type, Expr),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Type),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    typeTestT(ID, _Parent, _Encl, Type, Expr),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Type),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    whileLoopT(ID, _Parent, _Encl, Condition, Body),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Condition), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, ID).




walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    toplevelT(ID, Package, _Filename, Defs),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Package), walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Defs),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, _, _, ID) :- 
    packageT(ID, _Fullname),
    call(BeforeNode, ID),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, _, _, ID) :-
	importT(ID, _TopLevel, _ClassOrPackage),
    call(BeforeNode, ID),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :- 
	classDefT(ID, _Owner, _Name, Defs),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Defs),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
	methodDefT(ID, _ClassDef, _Name, Params, Type, Exceptions, Body),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Params),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Type),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Exceptions),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :- 
	fieldDefT(ID, _Class, Type, _Name, Init),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Type),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Init),
    call(AfterNode, ID).
walkTree(BeforeNode, AfterNode, BeforeList, AfterList, ID) :- 
	paramDefT(ID, _Method, Type, _Name),
    call(BeforeNode, ID),
    walkTree(BeforeNode, AfterNode, BeforeList, AfterList, Type),
    call(AfterNode, ID).


walkTree(BeforeNode, AfterNode, _, _, type(A, B, C)) :- 
    call(BeforeNode, type(A, B, C)),
    call(AfterNode, type(A, B, C)).
%walkTree(_, _, _, _, type(_, _, _)).

















nothing(Memory, Memory, _).




walkTreeMemoryList(Memory, Memory, _, _, _, _, []).
walkTreeMemoryList(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, [X | T]) :-
	walkTreeMemory(OldMemory, TempMemory, BeforeNode, AfterNode, BeforeList, AfterList, X),
	walkTreeMemoryList(TempMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, T).


walkTreeMemory(OldMemory, NewMemory, _, _, BeforeList, AfterList, []) :-
	call(BeforeList, OldMemory, TempMemory, []),
	call(AfterList, TempMemory, NewMemory, []).
	
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, [X | T]) :-
    call(BeforeList, OldMemory, TempMemory1, [X | T]),
	walkTreeMemoryList(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, [X | T]),
	call(AfterList, TempMemory2, NewMemory, [X | T]).



walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, null) :-
	call(BeforeNode, OldMemory, TempMemory, null),
	call(AfterNode, TempMemory, NewMemory, null).


walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    applyT(ID, _Parent, _Encl, Expr, _Name, Arguments, _Method), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Arguments),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    assertT(ID, _Parent, _Encl, Condition, Msg), !,
	call(BeforeNode, OldMemory, TempMemory1, ID),
	walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Condition),
	walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Msg),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    assignopT(ID, _Parent, _Encl, Lhs, _, Rhs), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Lhs),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Rhs),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    assignT(ID, _Parent, _Encl, Lhs, Rhs), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Lhs),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Rhs),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    blockT(ID, _Parent, _Encl, Statements), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Statements),
    call(AfterNode, TempMemory2, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, ID) :-
    breakT(ID, _Parent, _Encl, _Label, _Target), !,
    call(BeforeNode, OldMemory, TempMemory, ID),
    call(AfterNode, TempMemory, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, ID) :-
    caseT(ID, _Parent, _Encl, _Label), !,
    call(BeforeNode, OldMemory, TempMemory, ID),
    call(AfterNode, TempMemory, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    catchT(ID, _Parent, _Encl, Param, Body), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Param),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    conditionalT(ID, _Parent, _Encl, Condition, ThenPart, ElsePart), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Condition),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, ThenPart),
    walkTreeMemory(TempMemory3, TempMemory4, BeforeNode, AfterNode, BeforeList, AfterList, ElsePart),
    call(AfterNode, TempMemory4, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, ID) :-
    continueT(ID, _Parent, _Encl, _Label, _Target), !,
    call(BeforeNode, OldMemory, TempMemory, ID),
    call(AfterNode, TempMemory, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    doLoopT(ID, _Parent, _Encl, Condition, Body),  !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Condition),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    execT(ID, _Parent, _Encl, Expr), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, TempMemory2, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    forLoopT(ID, _Parent, _Encl, Init, Condition, Update, Body), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Init),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Condition),
    walkTreeMemory(TempMemory3, TempMemory4, BeforeNode, AfterNode, BeforeList, AfterList, Update),
    walkTreeMemory(TempMemory4, TempMemory5, BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, TempMemory5, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    getFieldT(ID, _Parent, _Encl, Expr, _Name, _Field), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, TempMemory2, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, ID) :-
    identT(ID, _Parent, _Encl, _Name, _Symbol), !,
    call(BeforeNode, OldMemory, TempMemory, ID),
    call(AfterNode, TempMemory, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    ifT(ID, _Parent, _Encl, Condition, ThenPart, ElsePart), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Condition),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, ThenPart),
    walkTreeMemory(TempMemory3, TempMemory4, BeforeNode, AfterNode, BeforeList, AfterList, ElsePart),
    call(AfterNode, TempMemory4, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    indexedT(ID, _Parent, _Encl, Index, Indexed), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Index),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Indexed),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, ID) :-
    labelT(ID, _Parent, _Encl, _Body, _Name), !,
    call(BeforeNode, OldMemory, TempMemory, ID),
    call(AfterNode, TempMemory, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    literalT(ID, _Parent, _Encl, Type, _Value), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Type),
    call(AfterNode, TempMemory2, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    localDefT(ID, _Parent, _Encl, Type, _Name, Init), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Type),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Init),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    newArrayT(ID, _Parent, _Encl, Dim, Elem, Type), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Dim),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Elem),
    walkTreeMemory(TempMemory3, TempMemory4, BeforeNode, AfterNode, BeforeList, AfterList, Type),
    call(AfterNode, TempMemory4, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    newClassT(ID, _Parent, _Encl, _Constructor, Args, TypeExpr, Def, _Enclosing), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Args),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, TypeExpr),
    walkTreeMemory(TempMemory3, TempMemory4, BeforeNode, AfterNode, BeforeList, AfterList,Def),
    call(AfterNode, TempMemory4, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, ID) :-
    nopT(ID, _Parent, _Encl), !,
    call(BeforeNode, OldMemory, TempMemory, ID),
    call(AfterNode, TempMemory, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    operationT(ID, _Parent, _Encl, Args, _OperatorName, _Pos), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Args),
    call(AfterNode, TempMemory2, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    precedenceT(ID, _Parent, _Encl, Expr), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, TempMemory2, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    returnT(ID, _Parent, _Encl, Expr), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, TempMemory2, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, ID) :-
    selectT(ID, _Parent, _Encl, _Name, _Selected, _Symbol), !,
    call(BeforeNode, OldMemory, TempMemory, ID),
    call(AfterNode, TempMemory, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    switchT(ID, _Parent, _Encl, Condition, Statements), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Condition),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Statements),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    synchronizedT(ID, _Parent, _Encl, Lock, Body), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Lock),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    throwT(ID, _Parent, _Encl, Expr), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    call(AfterNode, TempMemory2, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    tryT(ID, _Parent, _Encl, Body, Catchers, Finalizer), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Body),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Catchers),
    walkTreeMemory(TempMemory3, TempMemory4, BeforeNode, AfterNode, BeforeList, AfterList, Finalizer),
    call(AfterNode, TempMemory4, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    typeCastT(ID, _Parent, _Encl, Type, Expr), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Type),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    typeTestT(ID, _Parent, _Encl, Type, Expr), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Expr),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Type),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    whileLoopT(ID, _Parent, _Encl, Condition, Body), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Condition),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, TempMemory3, NewMemory, ID).




walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
    toplevelT(ID, Package, _Filename, Defs), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Package),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Defs),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, ID) :- 
    packageT(ID, _Fullname), !,
    call(BeforeNode, OldMemory, TempMemory, ID),
    call(AfterNode, TempMemory, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, ID) :-
	importT(ID, _TopLevel, _ClassOrPackage), !,
    call(BeforeNode, OldMemory, TempMemory, ID),
    call(AfterNode, TempMemory, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :- 
	classDefT(ID, _Owner, _Name, Defs), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Defs),
    call(AfterNode, TempMemory2, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :-
	methodDefT(ID, _ClassDef, _Name, Params, Type, Exceptions, Body),
    call(BeforeNode, OldMemory, TempMemory1, ID), !,
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Params),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Type),
    walkTreeMemory(TempMemory3, TempMemory4, BeforeNode, AfterNode, BeforeList, AfterList, Exceptions),
    walkTreeMemory(TempMemory4, TempMemory5, BeforeNode, AfterNode, BeforeList, AfterList, Body),
    call(AfterNode, TempMemory5, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :- 
	fieldDefT(ID, _Class, Type, _Name, Init), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Type),
    walkTreeMemory(TempMemory2, TempMemory3, BeforeNode, AfterNode, BeforeList, AfterList, Init),
    call(AfterNode, TempMemory3, NewMemory, ID).
walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, BeforeList, AfterList, ID) :- 
	paramDefT(ID, _Method, Type, _Name), !,
    call(BeforeNode, OldMemory, TempMemory1, ID),
    walkTreeMemory(TempMemory1, TempMemory2, BeforeNode, AfterNode, BeforeList, AfterList, Type),
    call(AfterNode, TempMemory2, NewMemory, ID).


walkTreeMemory(OldMemory, NewMemory, BeforeNode, AfterNode, _, _, type(A, B, C)) :- 
    call(BeforeNode, OldMemory, TempMemory, type(A, B, C)),
    call(AfterNode, TempMemory, NewMemory, type(A, B, C)).
%walkTreeMemory(Memory, Memory, _, _, _, _, type(_, _, _)).
















    
    

