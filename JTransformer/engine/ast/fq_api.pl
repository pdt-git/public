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
     Term =.. [Functor|[Id|Args]],
     nonvar(Id),
	 ast_node_def('Java',Functor,[_|ArgDefs]),
     JavaAST =.. [Functor|Id|[JavaASTArgs]],
     call(JavaAST),
     mapArgs(ArgDefs,JavaASTArgs,Args).
    
mapArgs([],[]).
     
mapArgs([JavaASTArg|JavaASTArgs],[FQN|Args]):-
    classDefT(JavaASTArg,_,_,_),
    fullQualifiedName(JavaASTArg,FQN),
    mapArgs(JavaASTArgs,Args).	
    
mapArgs([type(Kind,Id,Arity)|JavaASTArgs],[FQN|Args]):-
    type_term_to_atom(type(Kind,Id,Arity),FQN),
    mapArgs(JavaASTArgs,Args).
     
     
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
     
 