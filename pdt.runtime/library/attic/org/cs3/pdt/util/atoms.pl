%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This file is part of the Prolog Development Tool (PDT)
% 
% Author: Lukas Degener (among others) 
% E-mail: degenerl@cs.uni-bonn.de
% WWW: http://roots.iai.uni-bonn.de/research/pdt 
% Copyright (C): 2004-2006, CS Dept. III, University of Bonn
% 
% All rights reserved. This program is  made available under the terms 
% of the Eclipse Public License v1.0 which accompanies this distribution, 
% and is available at http://www.eclipse.org/legal/epl-v10.html
% 
% In addition, you may at your option use, modify and redistribute any
% part of this program under the terms of the GNU Lesser General Public
% License (LGPL), version 2.1 or, at your option, any later version of the
% same license, as long as
% 
% 1) The program part in question does not depend, either directly or
%   indirectly, on parts of the Eclipse framework and
%   
% 2) the program part in question does not include files that contain or
%   are derived from third-party work and are therefor covered by special
%   license agreements.
%   
% You should have received a copy of the GNU Lesser General Public License
% along with this program; if not, write to the Free Software Foundation,
% Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
%   
% ad 1: A program part is said to "depend, either directly or indirectly,
%   on parts of the Eclipse framework", if it cannot be compiled or cannot
%   be run without the help or presence of some part of the Eclipse
%   framework. All java classes in packages containing the "pdt" package
%   fragment in their name fall into this category.
%   
% ad 2: "Third-party code" means any code that was originaly written as
%   part of a project other than the PDT. Files that contain or are based on
%   such code contain a notice telling you so, and telling you the
%   particular conditions under which they may be used, modified and/or
%   distributed.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
        

/**
 * atom_concat(?a,?b,?c,?d)
 *
 * concats the atoms ?a, ?b and ?c to the atom ?d.
 *
 * Uses the buildin atom_concat/3:
 * atom_concat(_1,_tmp,_4),
 * atom_concat(_2,_3,_tmp).
 */
%measure:

atom_concat(_1,_2,_3,_4) :-
    atom_concat(_1,_tmp,_4),
    atom_concat(_2,_3,_tmp).

atom_concat(_1,_2,_3,_4,_5) :-
    atom_concat(_1,_2,_tmp,_5),
    atom_concat(_3,_4,_tmp).

atom_concat(_1,_2,_3,_4,_5,_6) :-
    atom_concat(_1,_2,_3,_tmp,_6),
    atom_concat(_4,_5,_tmp).


atom_concat(_1,_2,_3,_4,_5,_6,_7) :-
    atom_concat(_1,_2,_3,_4,_tmp,_7),
    atom_concat(_5,_6, _tmp).

atom_concat(_1,_2,_3,_4,_5,_6,_7,_8) :-
    atom_concat(_1,_2,_3,_4,_5,_tmp,_8),
    atom_concat(_6,_7, _tmp).

atom_concat(_1,_2,_3,_4,_5,_6,_7,_8,_9) :-
    atom_concat(_1,_2,_3,_4,_5,_6,_tmp,_9),
    atom_concat(_7,_8, _tmp).
        