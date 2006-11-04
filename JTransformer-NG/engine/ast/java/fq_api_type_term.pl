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
    sformat(S,'~w(~w) is not a valid java AST.', [Functor,Atom]),
    error_occured(S).
	

/**
	mapArgs([ast_arg1,...],[fq_arg1,...])
*/
	
mapArgs([],[]).
    
mapArgs( [JavaASTArg|JavaASTArgs],[FQN|Args]):-
    classDefT(JavaASTArg,_,_,_),
    !,
    fullQualifiedName(JavaASTArg,FQN),
    mapArgs(JavaASTArgs,Args).	
    
mapArgs([type(Kind,Id,Arity)|JavaASTArgs],[FQN|Args]):-
    !,
    map_type_term(type(Kind,Id,Arity),FQN),
    mapArgs(JavaASTArgs,Args).

mapArgs([[JavaSubListArg|JavaSubListArgs]|JavaASTArgs],[[SubListArg|SubListArgs]|Args]):-
    !,
    mapArgs([JavaSubListArg|JavaSubListArgs],[SubListArg|SubListArgs]),
    mapArgs(JavaASTArgs,Args).

mapArgs([Arg|JavaASTArgs],[Arg|Args]):-
    mapArgs(JavaASTArgs,Args).

/**
	map_type_term(?type/3 FQN, ?type/3 Id)).
*/
    
map_type_term(type(basic,TypeName,Arity), type(basic,TypeName,Arity)).
    
map_type_term(type(class,Id,Arity), type(class,FQN,Arity)):-
    fullQualifiedName(Id,FQN).
    
     
/**
	bindArgs([ast_arg_descr/4,...],[java_fq_arg1,...],[java_ast_arg1,...])
*/
bindArgs(_, [], []).

bindArgs([_|ArgDefs], [Arg|Args], [_|JavaASTArgs]) :-
    var(Arg),
    !,
	bindArgs(ArgDefs, Args, JavaASTArgs).


bindArgs([ast_arg(_,mult(_,_,ord), id,  Kind)|ArgDefs], [FQNAndIdList|Args], [IdList|JavaASTArgs]) :-
    !,
	member(classDefT,Kind),
	replaceFQNsWithClassIds(FQNAndIdList,IdList),
	bindArgs(ArgDefs, Args, JavaASTArgs).

% in the case the id is null
bindArgs([ast_arg(_,_, id,  _)|ArgDefs], [null|Args], [null|JavaASTArgs]) :-
	bindArgs(ArgDefs, Args, JavaASTArgs).


% attribute is a classDefT
bindArgs([ast_arg(_,_, _,  [classDefT])|ArgDefs], [FQN|Args], [JavaASTArg|JavaASTArgs]) :-
    fullQualifiedName(JavaASTArg, FQN),
	bindArgs(ArgDefs, Args, JavaASTArgs).
    
    
    
% type term attribute.
% INFO: there MUST not be other attribute kind possible where [typeTermType]
% assigned, also no nullType)
bindArgs([ast_arg(_,_, _,  [typeTermType])|ArgDefs], [FQN|Args], [JavaASTArg|JavaASTArgs]) :-
    !,
    map_type_term(JavaASTArg,FQN),
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
    nonvar(Id),
	!,
	fullQualifiedName(Id,FQN),
	replaceFQNsWithClassIds(FQNsAndIds,Ids).
    
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
      
