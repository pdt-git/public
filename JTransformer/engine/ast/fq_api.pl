/*
 * To Be Done:
 * Use Partial Evaluation!
 * PEF_FQN
 
  
  

typeTerm:

localDefT
literalT
typeCastT
typeTestT

classDefT:

toplevelT
selectT
identT
newClassT
importT
execT
*/


/**
 * class_fq(?FQN, ?PackageName, ?Name)
 *
 * TODO: To be optimized (second clause)
 */

class_fq(FQN,PackageName,Name):-
    nonvar(FQN),
    fullQualifiedName(CID,FQN),
    class(CID, PID, Name),
    packageT(PID,PackageName).

class_fq(FQN,PackageName,Name):-
    packageT(PID,PackageName),
    class(CID, PID, Name),
    fullQualifiedName(CID,FQN).

/**
 * param_fq(?Id, ?MethodId, ?Type, ?Name)
 *
 */
 
param_fq(Id, MethodId, Type, Name) :-
    var(Id),
    nonvar(Type),
    type_term_to_atom(TypeTerm,Type),
    paramDefT(Id,MethodId,TypeTerm,Name).

param_fq(Id, MethodId, Type, Name) :-
    type_term_to_atom(TypeTerm,Type),
    paramDefT(Id,MethodId,TypeTerm,Name).


/**
 * field_fq(?Id, ?Type, ?DeclType, ?Name, ?Init)
 *
 * TODO: To be optimized (second clause)
 */

field_fq(Id, Type, DeclType, Name, Init) :-
    nonvar(DeclType),
    nonvar(Type),
    var(Id),
    !,
    fullQualifiedName(DeclTypeId,DeclType),
    type_term_to_atom(TypeTerm,Type),
    fieldDefT(Id,DeclTypeId,TypeTerm,Name,Init).

field_fq(Id, Type, DeclType, Name, Init) :-
    fieldDefT(Id,DeclTypeId,TypeTerm,Name,Init),
    fullQualifiedName(DeclTypeId,DeclType),
    type_term_to_atom(TypeTerm,Type).

/**
 * method_fq(Id, RetType, DeclType, Name, Params, ExceptionNames, Body)
 *
 * TODO: to be optimized 
 */
 
method_fq(Id, RetType, DeclType, Name, Params, ExceptionNames, Body) :-
    var(Id),
	nonvar(DeclType),
	nonvar(RetType),
	nonvar(ExceptionNames),
    fullQualifiedName(DeclTypeId,DeclType),
    fullQualifiedNames(Exceptions, ExceptionNames),
    type_term_to_atom(RetTypeTerm,RetType),
    methodDefT(Id,DeclTypeId,Name, Params, RetTypeTerm, Exceptions,Body).


method_fq(Id, RetType, DeclType, Name, Params, ExceptionNames, Body) :-
    methodDefT(Id,DeclTypeId,Name, Params, RetTypeTerm, Exceptions,Body),
    fullQualifiedName(DeclTypeId,DeclType),
    fullQualifiedNames(Exceptions, ExceptionNames),
    type_term_to_atom(RetTypeTerm,RetType).


/**
 * type_term_to_atom(+TypeTerm,?Atom)
 */

type_term_to_atom(TypeTerm,Atom):-
    nonvar(TypeTerm),
    !,
    type_term_to_atom_(TypeTerm,Atom).

type_term_to_atom(TypeTerm,Atom):-
    nonvar(Atom),
    atom_to_type_term_(Atom,TypeTerm).


type_term_to_atom_(type(basic,TypeName,Arity), Atom):-
    arity_to_brackets_(Arity,Brackets),
    atom_concat(TypeName,Brackets,Atom).
    
type_term_to_atom_(type(class,Id,Arity), Atom):-
    arity_to_brackets_(Arity,Brackets),
    fullQualifiedName(Id,FQN),
    atom_concat(FQN,Brackets,Atom).

/**
 * arity_to_brackets(+Arity,?Brackets)
 */    

arity_to_brackets_(0,'').
arity_to_brackets_(Dim,Brackets) :-
    succ(DimDec, Dim),
    arity_to_brackets_(DimDec,SubBrackets),
    atom_concat(SubBrackets, '[]',Brackets).
    
    
/**
 * atom_to_type_term(+TypeTerm,?Atom)
 */
    
atom_to_type_term_(Atom,type(Kind,Type,Arity)):-
    remove_brackets(Atom,Kind,Type,Arity).

remove_brackets(Atom,Kind,TypeName,Arity) :-
    atom_concat(TypeCandidate, '[]',Atom),
    !,
	remove_brackets(TypeCandidate,Kind,TypeName,ArityDec),
    succ(ArityDec,Arity).
    
remove_brackets(Atom,basic, Atom,0):-
	basicType(Atom).    

remove_brackets(Atom,class, Id,0):-
	fullQualifiedName(Id,Atom).


/**
 * java_fq
 */

 java_fq(Term):-
	var(Term),
	throw('java_fq: term not instantiated').

% clause for classDefT

 java_fq(Term):-
     checkArgumentNumber(Term),
     Term =.. [classDefT|[FQN|Args]],
     nonvar(FQN),
     !,
     fullQualifiedName(Id,FQN),
	 getTerm(Id,JavaAST),
     JavaAST =.. [classDefT,Id|JavaASTArgs],
     call(JavaAST),
     mapArgs(JavaASTArgs,Args).

 java_fq(Term):-
     Term =.. [classDefT|[FQN|Args]],
     var(FQN),
     !,
	 ast_node_def('Java',classDefT,[_|ArgDefs]),
	 bindArgs(ArgDefs, Args, JavaASTArgs),
     JavaAST =.. [classDefT,Id|JavaASTArgs],
     call(JavaAST),
     fullQualifiedName(Id,FQN),
     mapArgs(JavaASTArgs,Args).

% attributes (modifierT,interfaceT...)
java_fq(Term):-
     Term =.. [Functor|Args],
     attribSignature(Functor,_),
     !,
	 ast_node_def('JavaAttributes',Functor,ArgDefs),
	 bindArgs(ArgDefs, Args, JavaASTArgs),
     JavaAST =.. [Functor|JavaASTArgs],
     call(JavaAST),
     mapArgs(JavaASTArgs,Args).
     

% clause for instantiated Id
 java_fq(Term):-
     Term =.. [Functor|[Id|Args]],
     nonvar(Id),
     !,
	 getTerm(Id,JavaAST),
     JavaAST =.. [Functor,Id|JavaASTArgs],
     call(JavaAST),
     mapArgs(JavaASTArgs,Args).
    
% clause for uninstantiated Id
 java_fq(Term):-
     Term =.. [Functor|[Id|Args]],
     !,
	 ast_node_def('Java',Functor,[_|ArgDefs]),
	 bindArgs(ArgDefs, Args, JavaASTArgs),
     JavaAST =.. [Functor,Id|JavaASTArgs],
     call(JavaAST),
     mapArgs(JavaASTArgs,Args).
    
% clause for uninstantiated Id
/*
 java_fq(Term):-
     Term =.. [Functor|[Id|Args]],
     !,
	 checkArgumentNumber(Functor,[Id|Args]),
     JavaAST =.. [Functor,Id|JavaASTArgs],
     call(JavaAST),
     mapArgs(JavaASTArgs,Args).
 */  
    
checkArgumentNumber(Term):-
    Term =.. [Functor|Args],    
    length(Args,Len),
    treeSignature(Functor,Len),
    !.
    
checkArgumentNumber(Term):-
    Term =.. [Functor|Args],    
    length(Args,Len),
    attribSignature(Functor,Len),
    !.

checkArgumentNumber(Term):-
    Term =.. [Functor|Args],    
    term_to_atom(Args,Atom),
    sformat(S,'~a(~a) is not a valid java AST.', [Functor,Atom]),
    error_occured(S).
	
	
mapArgs([],[]).
    
mapArgs([JavaASTArg|JavaASTArgs],[FQN|Args]):-
    classDefT(JavaASTArg,_,_,_),
    !,
    fullQualifiedName(JavaASTArg,FQN),
    mapArgs(JavaASTArgs,Args).	
    
mapArgs([type(Kind,Id,Arity)|JavaASTArgs],[FQN|Args]):-
    !,
    type_term_to_atom(type(Kind,Id,Arity),FQN),
    mapArgs(JavaASTArgs,Args).

mapArgs([Arg|JavaASTArgs],[Arg|Args]):-
    mapArgs(JavaASTArgs,Args).
     
test(java_fq,(
   assert_true('full instantiated fieldDefT',
      java_fq(fieldDefT(100174, 'java.lang.ClassLoader', 'java.util.Map', classAssertionStatus, null))),
   assert_true('full instantiated fieldDefT',
      java_fq(fieldDefT(100174, 'java.lang.ClassLoader', 'java.util.Map', classAssertionStatus, null))),
   assert_true('',
	   java_fq(fieldDefT(_,_,'java.lang.Class',_,_)))
      )).


bindArgs(_, [], []).

bindArgs([_|ArgDefs], [Arg|Args], [_|JavaASTArgs]) :-
    var(Arg),
    !,
	bindArgs(ArgDefs, Args, JavaASTArgs).


% in the case the id is null
bindArgs([ast_arg(_,_, id,  _)|ArgDefs], [null|Args], [null|JavaASTArgs]) :-
	bindArgs(ArgDefs, Args, JavaASTArgs).

% attribute is a classDefT
bindArgs([ast_arg(_,_, _,  [classDefT])|ArgDefs], [FQN|Args], [JavaASTArg|JavaASTArgs]) :-
    fullQualifiedName(JavaASTArg, FQN),
	bindArgs(ArgDefs, Args, JavaASTArgs).
    
bindArgs([ast_arg(_,mult(_,_,ord), id,  Kind)|ArgDefs], [FQNAndIdList|Args], [IdList|JavaASTArgs]) :-
	member(classDefT,Kind),
	replaceFQNsWithClassIds(FQNAndIdList,IdList),
	bindArgs(ArgDefs, Args, JavaASTArgs).
    
    
% type term attribute.
% INFO: there MUST not be other attribute kind possible where [typeTermType]
% assigned, also no nullType)
bindArgs([ast_arg(_,_, _,  [typeTermType])|ArgDefs], [FQN|Args], [JavaASTArg|JavaASTArgs]) :-
    !,
    type_term_to_atom(JavaASTArg,FQN),
	bindArgs(ArgDefs, Args, JavaASTArgs).

% attribute that is not a type term
bindArgs([ast_arg(_,_, attr,  _)|ArgDefs], [JavaASTArg|Args], [JavaASTArg|JavaASTArgs]) :-
	bindArgs(ArgDefs, Args, JavaASTArgs).

% id with several Kinds, but no classDefT
bindArgs([ast_arg(_,_, id,  Kind)|ArgDefs], [JavaASTArg|Args], [JavaASTArg|JavaASTArgs]) :-
	not(member(classDefT,Kind)),
	bindArgs(ArgDefs, Args, JavaASTArgs).

% id with several Kinds, and a classDefT - TODO: by now expensive!?
bindArgs([ast_arg(_,_, id, _)|ArgDefs], [FQN|Args], [JavaASTArg|JavaASTArgs]) :-
	fullQualifiedName(JavaASTArg,FQN),
	bindArgs(ArgDefs, Args, JavaASTArgs).

bindArgs([ast_arg(_,_, _,  _)|ArgDefs], [JavaASTArg|Args], [JavaASTArg|JavaASTArgs]) :-
	bindArgs(ArgDefs, Args, JavaASTArgs).
         
replaceFQNsWithClassIds([],[]).
replaceFQNsWithClassIds([FQN|FQNsAndIds],[_|Ids]):-
	var(FQN),
	!,
	replaceFQNsWithClassIds(FQNsAndIds,Ids).
	
	
replaceFQNsWithClassIds([FQN|FQNsAndIds], [Id|Ids]):-
	fullQualifiedName(Id,FQN),
	!,
	replaceFQNsWithClassIds(FQNsAndIds,Ids).

replaceFQNsWithClassIds([Id|FQNsAndIds], [Id|Ids]):-
	replaceFQNsWithClassIds(FQNsAndIds,Ids).

         
error_occured(S):-
    write(S),
    flush_output,
	throw(S).
      
