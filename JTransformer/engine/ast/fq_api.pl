
 
method_fq(Id, RetType, DeclType, Name, Params, Exceptions, Body):-
    fullQualifiedName(DeclTypeId,DeclType),
    type_term_to_atom(RetTypeTerm,RetType),
    methodDefT(Id,DeclTypeId,Name, Params, RetTypeTerm, Exceptions,Body).
    
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



