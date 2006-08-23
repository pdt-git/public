:- module(ast_node_analysis,
  [ast_with_identifiers/3,
  source_location_identifier/6]).
  
/**
 * ast_identifier(?Ast, +ASTKind, ?IdentifierList)
 *
 * @ASTKind e.g. 'Java'
 */
ast_with_identifiers(AST, ASTKind, Identifiers) :-
    getTerm(AST,Term),
    Term =.. [Functor,  AST |Args],
    ast_node_def(ASTKind,Functor, MetaArgs),
    identifier_arguments(MetaArgs, Identifiers, [AST |Args]),
    Term =.. [Functor, AST|Args],
    Term.


/**
 * source_location_identifier(+ASTKind, ?AST, ?Identifier, ?File, ?Start, ?End)
 */
source_location_identifier(ASTKind, AST, Identifier, File, Start, End):-
    ast_with_identifiers(AST, ASTKind, Identifiers), % We assume that there is just one identifier for Java
    member(Identifier,Identifiers),
    sl_argT(AST,identifier(Identifier),Start,End),
    sourceLocation(AST,File,_,_).
    


/**
 * identifier_arguments(+MetaArgs, ?Identifiers, ?Args)
 *
 * Binds all identifier in Args to the variables/atoms in ?Identifiers.
 */
identifier_arguments(_,[],[]).
    
identifier_arguments([ast_arg(name, mult(1,1,no ), attr, [atom]) | MetaArgs], 
          [Identifier|Identifiers], 
          [Identifier|Args]):-
    !,
    identifier_arguments(MetaArgs, Identifiers, Args).
    
identifier_arguments([_|MetaArgs], 
          Identifier, 
          [_|Args]):-
    !,
    identifier_arguments(MetaArgs, Identifier, Args).
    

