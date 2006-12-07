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
 * java_fq(PEF)
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
     fullQualifiedName_ri(Id,FQN),
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
     fullQualifiedName_ri(Id,FQN),
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
     bind_unbound_args(Functor, Id,JavaASTArgs,ArgDefs),
     call(JavaAST),
     mapArgs(JavaASTArgs,Args).

bind_unbound_args(_Functor,_Id,_Args,_ArgDefs) :-
	\+ use_reverse_indexes,
	!.

bind_unbound_args(_Functor,_Id,[],_ArgDefs).

bind_unbound_args(Functor,Id,[Arg|JavaASTArgs],
       [ast_arg(Kind,  mult(1,1,no ), id,  _)| ArgDefs]) :-
     %var(Id),
     nonvar(Arg),
     ri_functor_kind(Functor,Kind,Ri),
     Term =.. [Ri,Arg,Id],
     !,
     call(Term),
     bind_unbound_args(Functor,Id,JavaASTArgs,ArgDefs).
     
     
bind_unbound_args(Functor,Id,[_|JavaASTArgs], [_| ArgDefs]) :-
	 bind_unbound_args(Functor,Id,JavaASTArgs,ArgDefs).	  

/*
bind_unbound_parent(_Id,_JavaArgs,_ArgDefs).
    
bind_unbound_encl(Id,[Encl|_JavaASTArgs],
       [ast_arg(parent,  mult(1,1,no ), id,  _),ast_arg(encl,  mult(1,1,no ), id,  _)| _ArgDefs]) :-
     %var(Id),
     nonvar(Encl),
     ri_encl(Encl,Id).

bind_unbound_encl(_Id,_JavaArgs,_ArgDefs).    
*/    
    
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
        
mapArgs([],[]):- !.
    
mapArgs( [JavaASTArg|JavaASTArgs],[FQN|Args]):-
    classDefT(JavaASTArg,_,_,_),
    !,
    fullQualifiedName_ri(JavaASTArg,FQN),
    mapArgs(JavaASTArgs,Args),
    !.
    
mapArgs([type(Kind,Id,Arity)|JavaASTArgs],[FQN|Args]):-
    !,
    map_type_term(type(Kind,Id,Arity),FQN),
    mapArgs(JavaASTArgs,Args),
    !.

mapArgs([[JavaSubListArg|JavaSubListArgs]|JavaASTArgs],[[SubListArg|SubListArgs]|Args]):-
    !,
    mapArgs([JavaSubListArg|JavaSubListArgs],[SubListArg|SubListArgs]),
    mapArgs(JavaASTArgs,Args),
    !.

mapArgs([Arg|JavaASTArgs],[Arg|Args]):-
    mapArgs(JavaASTArgs,Args),
    !.

/**
 *  Public API
 *      map_type_term(?type/3, ?FQN)).
 * 
 *  Maps between type terms and FQNs,
 *
 *  map_type_term(type(class,10001,2),'java.lang.Object[][]').
 * e.g.: 
*/


map_type_term(TypeTerm, FQNBrackets):-
    nonvar(TypeTerm),
    type(Kind,TypeName,Arity) = TypeTerm,
    nonvar(Kind),
    Kind = basic,
    !,
    type_with_brackets(TypeName,Arity,FQNBrackets),
    !.
        
map_type_term(TypeTerm, null):-
    nonvar(TypeTerm),
    TypeTerm = type(class,null,0).

map_type_term(type(class,null,0), FQNBrackets):-
    nonvar(FQNBrackets),
    FQNBrackets = null.

map_type_term(TypeTerm, FQNBrackets):-
    nonvar(FQNBrackets),
    type_with_brackets(TypeName,Arity,FQNBrackets),
    map_to_basic_or_class(TypeName,Arity,TypeTerm),
    !.    
map_type_term(type(class,Id,0), FQN):-
%    resolve_class(FQN),
    fullQualifiedName_ri(Id,FQN),
    !.
map_type_term(type(class,Id,Arity), FQNBrackets):-
    nonvar(Id),
    fullQualifiedName_ri(Id,FQN),
    type_with_brackets(FQN,Arity,FQNBrackets),
    !.

map_type_term(type(class,Id,Arity), FQNBrackets):-
    nonvar(FQNBrackets),
    type_with_brackets(FQN,Arity,FQNBrackets),
    fullQualifiedName_ri(Id,FQN),
    !.

map_to_basic_or_class(TypeName,Arity,TypeTerm):-
    basicType(TypeName),
    TypeTerm=type(basic,TypeName,Arity),
    !.    
map_to_basic_or_class(TypeName,Arity,TypeTerm):-
    fullQualifiedName_ri(Id,TypeName),
    TypeTerm=type(class,Id,Arity),
    !.    


test(map_type_term) :-
    assert_true('gen null type', map_type_term(type(class,null,0),Var)),
    assert_true('FQN->int[][] type term',
                map_type_term(TTint2,'int[][]')),
    assert_true('FQN->int[][] type term check',
                 TTint2 = type(basic, int, 2)),
    assert_true('test null type', Var = null),
    assert_true('FQN->type/3', 
                 map_type_term(T,'java.lang.Object[][]')),
    assert_true('FQN->type/3 correct type check',  
      (class(CID, _, 'Object'), T = type(class,CID,2))).

            
  
/**
 * type_with_brackets(?TypeName,?Arity,?FQNBrackets) 
 * (-,-,+) | (+,+,-)
 */
    
type_with_brackets(TypeName,Arity,FQNBrackets) :-
    nonvar(Arity),
    !,
    square_brackets_for_arity(Arity,Brackets),
    atom_concat(TypeName,Brackets,FQNBrackets),
    !.

type_with_brackets(FQN,Arity,FQNBrackets) :-
    nonvar(FQNBrackets),
    !,
    arity_for_square_brackets(Arity,FQNBrackets,FQN),
    !.


square_brackets_for_arity(0,''):-!.
square_brackets_for_arity(Dim,Brackets) :-
    nonvar(Dim),
    !,
    succ(DimDec, Dim),
    square_brackets_for_arity(DimDec,RestBrackets),
    atom_concat(RestBrackets,'[]',Brackets),
    !.
    
arity_for_square_brackets(0,FQN,FQN):-
        not(atom_concat(_,'[]',FQN)),
        !.

arity_for_square_brackets(Dim,Brackets,FQN) :-
    nonvar(Brackets),
    !,
    atom_concat(RestBrackets,'[]',Brackets),
    arity_for_square_brackets(DimDec,RestBrackets,FQN),
    succ(DimDec, Dim),
    !.

     
/**
        bindArgs([ast_arg_descr/4,...],[java_fq_arg1,...],[java_ast_arg1,...])
*/
bindArgs(_, [], []):- !.

bindArgs([_|ArgDefs], [Arg|Args], [_|JavaASTArgs]) :-
    var(Arg),
    !,
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.


bindArgs([ast_arg(_,mult(_,_,ord), id,  Kind)|ArgDefs], [FQNAndIdList|Args], [IdList|JavaASTArgs]) :-
        member(classDefT,Kind),
    !,
        replaceFQNsWithClassIds(FQNAndIdList,IdList),
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.

% in the case the id is null
bindArgs([ast_arg(_,_, id,  _)|ArgDefs], [null|Args], [null|JavaASTArgs]) :-
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.


% attribute is a classDefT
bindArgs([ast_arg(_,_, _,  [classDefT])|ArgDefs], [FQN|Args], [JavaASTArg|JavaASTArgs]) :-
    fullQualifiedName_ri(JavaASTArg, FQN),
        bindArgs(ArgDefs, Args, JavaASTArgs),
    !.
    
    
% type term attribute.
% INFO: the only other attribute kind than typeTermType possible here MUST be nullType 

bindArgs([ast_arg(_,_, _,  Kinds)|ArgDefs], [FQN|Args], [JavaASTArg|JavaASTArgs]) :-
    member(typeTermType,Kinds),
    !,
    
    map_type_term(JavaASTArg,FQN),
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.

% attribute that is not a type term
bindArgs([ast_arg(_,_, attr,  _)|ArgDefs], [JavaASTArg|Args], [JavaASTArg|JavaASTArgs]) :-
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.

% id with several Kinds, but no classDefT
bindArgs([ast_arg(_,_, id,  Kind)|ArgDefs], [JavaASTArg|Args], [JavaASTArg|JavaASTArgs]) :-
        not(member(classDefT,Kind)),
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.

% methodDef id 
bindArgs([ast_arg(_,_, id,  _Kind)|ArgDefs], [JavaASTArg|Args], [JavaASTArg|JavaASTArgs]) :-
        methodDefT(JavaASTArg,_,_,_,_,_,_),
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.

% fieldDefT id
bindArgs([ast_arg(_,_, id,  _Kind)|ArgDefs], [JavaASTArg|Args], [JavaASTArg|JavaASTArgs]) :-
        fieldDefT(JavaASTArg,_,_,_,_),
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.

% id with several Kinds, and a classDefT - TODO: by now expensive!?
bindArgs([ast_arg(_,_, id, _)|ArgDefs], [FQN|Args], [JavaASTArg|JavaASTArgs]) :-
        fullQualifiedName_ri(JavaASTArg,FQN),
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.

bindArgs([ast_arg(_,_, id, _)|ArgDefs], [_FQN|Args], [JavaASTArg|JavaASTArgs]) :-
        \+ number(JavaASTArg), JavaASTArg \= null, 
        format('~nwarning: (bindArgs) probably not an ID: ~w~n',[JavaASTArg]),
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.

bindArgs([ast_arg(_,_, _,  _)|ArgDefs], [JavaASTArg|Args], [JavaASTArg|JavaASTArgs]) :-
        bindArgs(ArgDefs, Args, JavaASTArgs),
        !.
         
replaceFQNsWithClassIds([],[]).
replaceFQNsWithClassIds([FQN|FQNsAndIds],[_|Ids]):-
    nonvar(Id),
        !,
        fullQualifiedName_ri(Id,FQN),
        replaceFQNsWithClassIds(FQNsAndIds,Ids),
        !.
    
replaceFQNsWithClassIds([FQN|FQNsAndIds],[_|Ids]):-
        var(FQN),
        !,
        replaceFQNsWithClassIds(FQNsAndIds,Ids),
        !.
        
        
replaceFQNsWithClassIds([FQN|FQNsAndIds], [Id|Ids]):-
        fullQualifiedName_ri(Id,FQN),
        !,
        replaceFQNsWithClassIds(FQNsAndIds,Ids),
        !.

replaceFQNsWithClassIds([Id|FQNsAndIds], [Id|Ids]):-
        replaceFQNsWithClassIds(FQNsAndIds,Ids),
        !.      

error_occured(S):-
    write(S),
    flush_output,
        throw(S).


java_fq_to_pef(FQ, PEF):-
     t_i_assert_bound(FQ),
     FQ =.. [Functor|Args],
         ast_node_def('Java',Functor,ArgDefs),
         !,
         bindArgs(ArgDefs, Args, JavaASTArgs),
     PEF =.. [Functor|JavaASTArgs].
     
java_fq_to_pef(FQ, PEF):-
     FQ =.. [Functor|Args],
     attribSignature(Functor,_),
     !,
         ast_node_def('JavaAttributes',Functor,ArgDefs),
         bindArgs(ArgDefs, Args, JavaASTArgs),
     PEF =.. [Functor|JavaASTArgs].
     
t_i_assert_bound(FQ) :-
    not(ground(FQ)),
    sformat(Message,'compiler error, term not ground: ~w',[FQ]),    
    t_i_based_error_handling(Message).

t_i_assert_bound(_FQ).
    
     
/**
 * getTypeIfNullEnclClass(+ID|null, +Encl,-FQN)
 * 
 * For use with java_fq abstraction. 
 */

getTypeIfNullEnclClass_fq(null, Stat, FQN) :-
    enclClass(Stat, Encl),
    fullQualifiedName_ri(Encl,FQN).
    
getTypeIfNullEnclClass_fq(Id, _, FQN) :-
    getType_fq(Id, FQN).

/**
 * getType_fq(+ExprId,?FQN)
 * 
 * FQN wrapper for getType/2.
 * Retrieves full-qualified return type 
 * name for the Expression ExprId.
 */
    
getType_fq(Id,FQN):-
    getType(Id, Type),
    map_type_term(Type,FQN).

/**
 * enclClass_fq(Id,FQN)
 * 
 * FQN wrapper for enclClass/2.
 */
enclClass_fq(Id,FQN):-
    enclClass(Id, Class),
    fullQualifiedName_ri(Class, FQN).

/**
 * types_fq(+Exprs, ?FQNs)
 */

types_fq([],[]).
types_fq([Expr|Exprs], [FQN|FQNs]):-    
    getType_fq(Expr,FQN),
    types_fq(Exprs, FQNs).
   
