/**
 * AccessorInterface - create get methods for all fields
 */
ct(aiget(ClassName), (
            classDefT(CID, _, ClassName,_),
            not(externT(CID)), not(interface(CID)),
            fieldDefT(FID, CID, Type, Name, _),
            not(modifierT(FID, 'final')),
            first_char_up(Name, UName),
			name_concat('get',UName,MethName),
            not(methodDefT(_MID, CID, MethName, _, _, _, _)),
            new_id(MethID), new_id(BodyID), new_id(_ReturnID), new_id(_ReadFID),
            format('~a:~a~n',[ClassName,MethName])
          ),(
            add(methodDefT(MethID, CID, MethName, [], Type, [], BodyID)),
                shareModifier('static', FID, MethID),
                add(modifierT(MethID, 'public')),
                add(blockT(BodyID, MethID, MethID, [_ReturnID])),
                    add(returnT(_ReturnID, BodyID, MethID, _ReadFID)),
                        add(getFieldT(_ReadFID, _ReturnID, MethID, 'null', Name, FID)),
            add_to_class(CID,MethID)
          )).
 
          
num_getter(Num):-
	findall(Name, ( methodDefT(_,Class,Name,_,_,_,_),
					not(externT(Class)),
					name_concat('get',UName,Name),
					first_char_up(LName, UName),
					LName \= UName
			),L),
	length(L,Num).

num_pot_getter(Num):-
	findall(CID, (
			classDefT(CID, _, _,_),
            not(externT(CID)), not(interface(CID)),
            fieldDefT(FID, CID, _, Name, _),
            not(modifierT(FID, 'final')),
            first_char_up(Name, UName),
			name_concat('get',UName,MethName),
            not(methodDefT(_MID, CID, MethName, _, _, _, _))
	),L),
	length(L,Num).
	