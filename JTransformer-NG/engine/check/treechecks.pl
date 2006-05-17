:- dynamic constraintErrorMsg/1.

/**
  * violatedContraints(+Id, -ErrorMsg)
  */
violatedContraints(Id, ErrorMsg):-
    retractall(constraintErrorMsg(_)),
    !,
    checkTreeLinks(Id, assertErrorMsgs),
    constraintErrorMsg(ErrorMsg).

setUp(violatedContraints):-
    assert(classDefT(a,b,c,[d])).
test(violatedContraints):-
    assert_true(findall(M,violatedContraints(a,M),List)),
    assert_true(length(List,4)).     
tearDown(violatedContraints):-
    retractall(classDefT(a,b,c,[d])).

assertErrorMsgs(ErrorMsg):-
    assert(constraintErrorMsg(ErrorMsg)).
/*
    checkTreeLinks

    Testet alle trees auf korrekte Referenzierung durch den parent tree.
*/

checkTreeLinks :- findall(_id, checkTreeLinks(_id,write), _l).

/*
    checkTreeLinks(+Id, +ErrorHandler)

    Tests the constraints of parent, encl and referencing trees of id.
    Provide an ErrorHandler predicate to handle the error message.
    E.g. checkTreeLinks(1001,write) will write out all error messages to the console.
*/


checkTreeLinks(_id,ErrorHandler) :-
    tree(_id, _p, _ptype),
	checkConstraints(_id,ErrorHandler),    
    checkParentLink(_p,_id,ErrorHandler),
    sub_trees(_id, _subs),
    vars_bound(_id,_subs,ErrorHandler),
    checkTreeLinks(_id, _ptype, _subs,ErrorHandler).

checkTreeLinks(_, _, [],_).
checkTreeLinks(_pid, _ptype, [_h|_t],ErrorHandler) :-
    tree(_h, _p, _stype),
%    checkParentLink(_p,_h),
    !,
    subtreeError(_h, _stype, _p, _pid, _ptype,ErrorHandler),
    checkTreeLinks(_pid, _ptype, _t,ErrorHandler).

checkTreeLinks(_pid, _ptype, [_h|_t],ErrorHandler) :-
     sformat(Msg,'referenced subtree ~a from ~a ~a does not exist.~n',[_h, _ptype, _pid]),
	 Term =.. [ErrorHandler,Msg], 
	 call(Term),
     checkTreeLinks(_pid, _ptype, _t,ErrorHandler).


checkParentLink(_,ID,_):-
    packageT(ID,_),
    !.
        
checkParentLink(null,_,_):-
	!.
	
checkParentLink(NODE, CHILD,_) :-
    blockT(CHILD, _,_,_),
    tryT(NODE, _,_,_,_, CHILD),
    !.
	
checkParentLink(Node,Child,_):-
    sub_trees(Node,Subs),
    member(Child,Subs),
    !.

checkParentLink(Node,Child,ErrorHandler):-
    sformat(Msg,'node ~a is not a subtree of its parent ~a.~n',[Child,Node]),
	 Term =.. [ErrorHandler,Msg], 
	 call(Term).

vars_bound(_,[],_).
vars_bound(_id,[_sub|_subs],ErrorHandler):-
    nonvar(_sub),
    !,
    vars_bound(_id,_subs,ErrorHandler).
    
vars_bound(_id,[_sub|_],ErrorHandler):-
    sformat(Msg,'referenced id ~a from ~a is not bound.~n',[_sub, _id]),
	 Term =.. [ErrorHandler,Msg], 
	 call(Term),
    fail.

subtreeError(_id, _,   _pid, _pid, _,_) :- !.
% sonderfall, classen haben als parent packages statt toplevel, die sie eigentlich referenzieren
subtreeError(_id, classDefT,   _, _, toplevelT,_) :- !.
subtreeError(_id, _stype, _p, _pid, _ptype,ErrorHandler) :-
    sformat(Msg,'referenced ~a ~a has ~a as parent instead of ~a ~a.~n',[_stype, _id, _p, _ptype, _pid]),
	 Term =.. [ErrorHandler,Msg], 
	 call(Term).


/*
checkTree(_id):-
   getFieldT(_id,_pid,_encl,_expr,_v2,_v3), 
   printErrorIfNotMethodAndNotField(_encl),
   printErrorIfNotExpressionOrNull(_expr).

   
checkEncl(_id):-selectT(_id,_pid,_encl,_v1,_v2,_v3), printFailure(_id,_encl).
checkEncl(_id):-identT(_id,_pid,_encl,_v1,_v2), notMethodOrField(_encl),printFailure(_id,_encl).

checkEncl(_id):-methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5), not(classDefT(_pid,_,_,_)),printFailure(_id,_pid).
checkEncl(_id):-fieldDefT(_id,_pid,_v1,_v2,_v3), not(classDefT(_pid,_,_,_)),printFailure(_id,_pid).

checkEncl(_id):-localDefT(_id,_pid,_encl,_v1,_v2,_v3), not(methodDefT(_encl,_,_,_,_,_,_)),printFailure(_id,_encl).
checkEncl(_id):-paramDefT(_id,_pid,_v1,_v2), not(methodDefT(_pid,_,_,_,_,_,_)),printFailure(_id,_pid).

checkEncl(_id):-classDefT(_id,_pid,_v1,_v2),not(packageT(_pid,_)),printFailure(_id,_pid).
checkEncl(_id):-packageT(_id,_v1),!.
checkEncl(_id):-toplevelT(_id,_pid,_v1,_v2),not(packageT(_pid,_)),printFailure(_id,_pid).

checkEncl(_id):-blockT(_id,_pid,_encl,_v1), notMethodOrField(_encl),printFailure(_id,_encl).

checkEncl(_id):-doLoopT(_id,_pid,_encl,_v1,_v2), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-whileLoopT(_id,_pid,_encl,_v1,_v2),notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4),notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-labelT(_id,_pid,_encl,_v1,_v2), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-switchT(_id,_pid,_encl,_v1,_v2), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-caseT(_id,_pid,_encl,_v1), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-synchronizedT(_id,_pid,_encl,_v1,_v2), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-tryT(_id,_pid,_encl,_v1,_v2,_v3),notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-catchT(_id,_pid,_encl,_v1,_v2),notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-ifT(_id,_pid,_encl,_v1,_v2,_v3), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-conditionalT(_id,_pid,_encl,_v1,_v2,_v3),notMethodOrField(_encl),printFailure(_id,_encl).
//format('rt:exec: ~a~n', [_id])
checkEncl(_id):-execT(_id,_pid,_encl,_v1), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-returnT(_id,_pid,_encl,_v1), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-breakT(_id,_pid,_encl,_v1,_v2),notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-continueT(_id,_pid,_encl,_v1,_v2), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-throwT(_id,_pid,_encl,_v1),notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4),notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-newArrayT(_id,_pid,_encl,_v1,_v2,_v3), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-assignT(_id,_pid,_encl,_v1,_v2), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-assignopT(_id,_pid,_encl,_v1,_v2,_v3), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-operationT(_id,_pid,_encl,_v1,_v2,_v3),notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-typeCastT(_id,_pid,_encl,_v1,_v2), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-typeTestT(_id,_pid,_encl,_v1,_v2), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-indexedT(_id,_pid,_encl,_v1,_v2),notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-literalT(_id,_pid,_encl,_v1,_v2),notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id):-assertT(_id,_pid,_encl,_v1,_v2),notMethodOrField(_encl),printFailure(_id,_encl).
%checkEncl(_id):-classMembersT(_id,_v1), notMethodOrField(_encl),printFailure(_id,_encl).
checkEncl(_id).

printFailure(_id,_encl):-
        format('encl elem of ~a is ~a but not a method or field (for method: class)~n',[_id,_encl]).
*/

printErrorIfNotMethodAndNotField(_encl):-
	not(methodDefT(_encl,_,_,_,_,_,_)), 
	not(fieldDefT(_encl,_,_,_,_)),
	printFailure(_id,_encl),
	!.
printErrorIfNotMethodAndNotField(_).

printErrorIfNotExpressionOrNull(_id):-
    not(isExpression(_id)),
    not(_id = 'null'),
    tree(_id,_p,_type),
    format('the id ~a should bei an expression or null, but it is an ~a~n',[_id,_type]).
printErrorIfNotExpressionOrNull(_).

/**
 * checkConstraint(ID,ErrorHandler)
 *
 * ErrorHandler is a predicate with arity 1
 * which is evaluated on the error message
 * generated on failure.
 */
 
checkConstraints(ID,ErrorHandler):-
    getTerm(ID,Term),
    Term =.. [Functor|[ID|Args]],
    tree_constraints(Functor,Constraints),
    checkConstraints(Term,Constraints,Args,2,ErrorHandler).
    
checkConstraints(_,[],[],_,_).
checkConstraints(Term,[Constraints | CRest],[Arg|ARest],Pos,ErrorHandler):-
    (
	  (checkConstraintList(Constraints,Arg),!);
	  printCheckError(Term,Constraints,Arg,Pos,ErrorHandler)
	),
	plus(Pos,1,Inc),
    checkConstraints(Term,CRest,ARest,Inc,ErrorHandler).


printCheckError(Term,Constraints,Arg,Pos,ErrorHandler):-
    term_to_atom(Term,TermAtom),
    term_to_atom(Constraints,ConstraintsAtom),
    term_to_atom(Arg,ArgAtom),
    sformat(Msg,'ERROR in term: ~a, argument ~a (\'~a\') does not fullfill constraints:~n  ~a~n',
 	  [TermAtom,Pos,ArgAtom,ConstraintsAtom]),
	 MsgTerm =.. [ErrorHandler,Msg], 
	 call(MsgTerm),
 	  
 	flush_output.


checkConstraintList(_,[]):-!.
checkConstraintList(Constraints,[H|T]):-
	checkConstraintList(Constraints,H),
	checkConstraintList(Constraints,T).

	

/*
checkConstraintList(_,[],_):-!.
checkConstraintList(Constraints,[H|T],Pos):-
    (
	  (
	    checkConstraints(Constraints,H),
	    !
	  );
	  (
	    printCheckError('list',Constraints,H,Pos),
  	    fail
  	  )
	),
	plus(Pos,1,Inc),
    checkConstraintList(Constraints,T,Inc).
*/



/**
 * checks if the type of arg is a member of the Constraints list:
 * e.g.: identT(Arg,...), checkConstraintList([identT],Arg)
 * (checks if functor is a member of the list Constraints )
 */
checkConstraintList(Constraints,Arg):-
    getTerm(Arg, Term),
    functor(Term,PLASTFact, _),
    member(PLASTFact,Constraints),
    tree(_,_,PLASTFact),
    !.

/**
 * checks if the type of arg is a member of the Constraints list:
 * e.g.: identT(Arg,...), checkConstraintList([expressionType],Arg)
 */
checkConstraintList(Constraints,Arg):-
    getTerm(Arg, Term),
    functor(Term,PLASTFact, _),
    member(Constraint,Constraints),
    not(treeSignature(Constraint,_)),
%    term_to_atom(Arg,Atom),
%    format('check2: ~a,~a~n',[Constraint,Atom]),
    Predicate =.. [Constraint, PLASTFact],
    call(Predicate),
    !.

/**
 * checks if the type of arg is a member of the Constraints list:
 * e.g.: identT(Arg,...), checkConstraintList([atomType],'Name')
 */
checkConstraintList(Constraints,Arg):-
    member(Constraint,Constraints),
    not(treeSignature(Constraint,_)),
%    term_to_atom(Arg,Atom),
%    format('check3: ~a,~a~n',[Constraint,Atom]),
    Predicate =.. [Constraint, Arg],
    call(Predicate),
    !.
	

/**
 * tree_constraints(treeType, [[kind_1,...],...])
 *
 * constraints for all tree elements 
 */

test('tree_constraints_ident'):-
    assert(identT(1,parentId,enclId,name,classId)),
    assert(identT(parentId,a,enclId,a,a)),
    assert(fieldDefT(enclId,a,a,a,a)),
    assert(classDefT(classId,a,a,a)),
    checkConstraints(1,write),
    retract(identT(1,parentId,enclId,name,classId)),
    retract(identT(parentId,a,enclId,a,a)),
    retract(fieldDefT(enclId,a,a,a,a)),
    retract(classDefT(classId,a,a,a)).
    




tree_constraints(applyT ,[[allType],[methodDefT,fieldDefT],[expressionType,nullType],[atomType],[expressionType],[methodDefT]]).
tree_constraints(assertT ,[[allType],[methodDefT],[expressionType]]).
tree_constraints(assignopT,[[allType],[methodDefT,fieldDefT],[getFieldT,identT,indexedT],[atomType],[expressionType]]).
tree_constraints(assignT,[[allType],[methodDefT,fieldDefT],[getFieldT,identT,indexedT],[expressionType]]).
tree_constraints(blockT, [[allType],[methodDefT],[statementType]]).
tree_constraints(breakT,[[allType],[methodDefT],[atomType],[statementType]]).
tree_constraints(caseT,[[allType],[methodDefT],[expressionType,nullType]]).
tree_constraints(catchT,[[allType],[methodDefT],[paramDefT],[blockT]]).
tree_constraints(classDefT ,[[execT,packageT,classDefT,newClassT,blockT,nullType],[atomType],[methodDefT,fieldDefT,classDefT]]).
tree_constraints(conditionalT,[[allType],[methodDefT,fieldDefT],[expressionType],[expressionType],[expressionType]]).
tree_constraints(continueT,[[allType],[methodDefT],[atomType],[statementType]]).
tree_constraints(doLoopT,[[allType],[methodDefT,fieldDefT],[expressionType],[statementType]]).
tree_constraints(execT,[[allType],[methodDefT],[expressionType,classDefT]]).
tree_constraints(extendsT,[[],[classDefT]]).
tree_constraints(externT,[[]]).
tree_constraints(fieldDefT ,[[classDefT],[typeTermType],[atomType],[expressionType,nullType]]).
tree_constraints(forLoopT,[[allType],[methodDefT],[expressionType,nullType,localDefT],[expressionType,nullType],[expressionType,nullType],[statementType]]).
tree_constraints(getFieldT, [[allType],[methodDefT,fieldDefT],[expressionType,nullType],[atomType], [fieldDefT,nullType]]). % if it is the length field of an array
tree_constraints(identT, [[allType], [methodDefT,fieldDefT], [atomType], [classDefT,localDefT,paramDefT,nullType,packageT]]).
tree_constraints(ifT,[[allType],[methodDefT],[expressionType],[blockT,statementType],[blockT,statementType,nullType]]).
tree_constraints(implementsT,[[],[classDefT]]).
tree_constraints(importT ,[[toplevelT],[packageT,classDefT]]).
tree_constraints(indexedT,[[allType],[methodDefT,fieldDefT],[expressionType],[expressionType]]).
tree_constraints(interfaceT ,[[]]).
tree_constraints(labelT,[[allType],[methodDefT,fieldDefT],[statementType],[atomType]]).
tree_constraints(literalT ,[[allType],[methodDefT,fieldDefT],[typeTermType],[atomType/* TODO: vorher auch typeTermType*/ ]]). 
tree_constraints(localDefT,[[allType],[methodDefT],[typeTermType],[atomType],[expressionType,nullType]]).
tree_constraints(methodDefT ,[[classDefT],[atomType],[paramDefT],[typeTermType,nullType],[classDefT],[blockT,nullType]]).
tree_constraints(modifierT,[[atomType]]).
tree_constraints(newArrayT, [[allType],[methodDefT,fieldDefT],[expressionType],[expressionType],[typeTermType]]).
tree_constraints(newClassT, [[allType],[methodDefT,fieldDefT],[methodDefT,nullType],[expressionType],[identT,selectT],[classDefT,nullType],[classDefT,nullType]]).
tree_constraints(nopT,[[allType],[methodDefT]]).
tree_constraints(operationT,[[allType],[methodDefT,fieldDefT],[expressionType],[atomType],[atomType]]).
tree_constraints(packageT ,[[atomType]]).
tree_constraints(paramDefT ,[[methodDefT,catchT],[typeTermType],[atomType]]).
tree_constraints(precedenceT,[[allType],[methodDefT,fieldDefT],[expressionType]]).
tree_constraints(returnT,[[allType],[methodDefT],[expressionType,nullType]]).
tree_constraints(selectT, [[allType], [methodDefT,fieldDefT], [atomType],[selectT,identT],[classDefT,packageT]]).
tree_constraints(switchT,[[allType],[methodDefT],[expressionType],[statementType]]).
tree_constraints(synchronizedT,[[allType],[methodDefT],[expressionType],[blockT]]).
tree_constraints(throwT,[[allType],[methodDefT],[expressionType]]).
tree_constraints(toplevelT, [[packageT,nullType],[atomType],[importT,classDefT]]).
tree_constraints(tryT,[[allType],[methodDefT],[blockT],[catchT],[blockT,nullType]]).
tree_constraints(typeCastT,[[allType],[methodDefT,fieldDefT],[typeTermType],[expressionType]]).
tree_constraints(typeTestT,[[allType],[methodDefT,fieldDefT],[typeTermType],[expressionType]]).
tree_constraints(whileLoopT,[[allType],[methodDefT],[expressionType],[statementType]]).

expressionType(applyT).
expressionType(assignT).
expressionType(assignopT).
expressionType(conditionalT).
expressionType(getFieldT).
expressionType(identT).
expressionType(indexedT).
expressionType(literalT).
expressionType(newArrayT).
expressionType(newClassT).
expressionType(operationT).
expressionType(precedenceT).
expressionType(selectT).
expressionType(typeCastT).
expressionType(typeTestT).

expression(Id) :- tree(Id, _, Type), expressionType(Type). 

allType(ID):-
	tree(ID,_,_).  

atomType(Atom):-atom(Atom).
atomType(Atom):-number(Atom).


typeTypes(classDefT).    

refType(classDefT).
refType(localDefT).
refType(paramDefT).
refType(fieldDefT).
refType(methodDefT).

typeTermType(type(_,_,_)).

nullType('null').

statementType(assertT).
statementType(assignopT).
statementType(assignT).
statementType(blockT).
statementType(breakT).
statementType(caseT).
statementType(catchT).
statementType(continueT).
statementType(doLoopT).
statementType(execT).
statementType(forLoopT).
statementType(ifT).
statementType(labelT).
statementType(localDefT).
statementType(nopT).
statementType(returnT).
statementType(switchT).
statementType(synchronizedT).
statementType(throwT).
statementType(tryT).
statementType(whileLoopT).


statement(Id) :- tree(Id, _, Type), statementType(Type). 



% ma
declarationType(classDefT).
declarationType(methodDefT).
declarationType(fieldDefT).

declaration(Id) :- tree(Id, _, Type), declarationType(Type). 



/*validReferenceType(_type, _pos, _validtypes) :-
    (_type == identT, 
    	(_pos == 5, _validtypes == [classDefT, localDefT, paramDefT, fieldDefT]));
   	(_type == getFieldT, 
   	    (_pos == #, _validtypes ==[];
   	     _pos == #, _validtypes ==[]))
    (_type == selectT, 
    	(_pos == 5, _validtypes == [selectT, identT];
    	 _pos == 6, _validtypes == [classDefT, packageT])).
*/


    
/* usw. */
/* Statt mit pos könnte man auch den Parametern Namen verpassen. */
/* den Knoten mittels tree. */
    
invalidSelectReference(_id, _selectedfrom, _pos, _validtypes, _actualtype) :- 
   selectT(_id, _, _, _, _selectedfrom, _), 
   validReferenceType(selectT, 5, _validtypes), 
   tree(_selectedfrom, _, _actualtype), 
   not(member(_actualtype, _validtypes)).
   
   
    