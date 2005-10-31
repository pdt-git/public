 /**
  *   atom_list_concat(+AtomList,?Atom)
  *     Ar4 is unifiable with the concatenation of all atoms from
  *     the list in arg1.
  *     -- Günter Kniesel, 28.06.1006
  */
atom_list_concat([],'').
atom_list_concat([One],One).
atom_list_concat([H|T],Atom) :-
   atom_list_concat(T,Tatom),
   atom_concat(H,Tatom,Atom).   
   
/**
        stringAppend(?Atom1, ?Atom2, ?Atom3, ?Atom4)
        
        Atom4 forms the concatination of Atom1, Atom2 and Atom3.
        
        TODO: specify possible binding combinations
*/    
    
stringAppend(_str1, _str2, _str3, _Ret) :-
    stringAppend(_str1, _str2, _tmpName),
    stringAppend(_tmpName, _str3, _Ret).


/**
        stringAppend(?Atom1, ?Atom2, ?Atom3, ?Atom4, ?Atom5)
        
        Atom5 forms the concatination of Atom1, Atom2, Atom3 and Atom4.
        TODO: specify possible binding combinations
*/    

stringAppend(_str1, _str2, _str3, _str4, _Ret) :-
    stringAppend(_str1, _str2, _str3, _tmpName),
    stringAppend(_tmpName, _str4, _Ret).

/**
        stringAppend(?Atom1, ?Atom2, ?Atom3, ?Atom4, ?Atom5, ?Atom6)
        
        Atom6 forms the concatination of Atom1, Atom2, Atom3, Atom4 and Atom5.
        TODO: specify possible binding combinations
*/    

stringAppend(_str1, _str2, _str3, _str4,_str5, _Ret) :-
    stringAppend(_str1, _str2, _str3, _str4, _tmpName),
    stringAppend(_tmpName, _str5, _Ret).


/**
        stringAppend(?Atom1, ?Atom2, ?Atom3, ?Atom4, ?Atom5, ?Atom6, ?Atom7)
        
        Atom6 forms the concatination of Atom1, Atom2, Atom3, Atom4, Atom5 and Atom6.
        TODO: specify possible binding combinations
*/    

stringAppend(_str1, _str2, _str3, _str4,_str5, _str6, _Ret) :-
    stringAppend(_str1, _str2, _str3, _str4, _str5, _tmpName),
    stringAppend(_tmpName, _str6, _Ret).
/*
        first_char_up(?LowerAtom, ?UpperAtom)
        
        At least one of the arguments must be bound to
    to a atom where the first character is from 
    the domain [a-z,A-Z].
        Binds UpperAtom to an atom which is LowerAtom
        with an uppercase first char, respectively
        Binds LowerAtom to an atom which is UpperAtom
        with an lowercase first char.
        
        e.g.
        ?- first_char_up(name, Name).
         Yes
*/
first_char_up(Name, UName) :-
    nonvar(Name),
        atom_length(Name, Len),
    Prec is Len - 1,
        sub_atom(Name, 1,Prec,_,Rest),
        atom_concat(FirstChar,Rest,Name),
        upcase_atom(FirstChar,UpperFirstChar),
        atom_concat(UpperFirstChar,Rest,UName),
        !.
        
first_char_up(Name, UName) :-
    nonvar(UName),
        atom_length(UName, Len),
    Prec is Len - 1,
        sub_atom(UName, 1,Prec,_,Rest),
        atom_concat(FirstChar,Rest,UName),
        downcase_atom(FirstChar,LowerFirstChar),
        atom_concat(LowerFirstChar,Rest,Name),
        !.      
        
