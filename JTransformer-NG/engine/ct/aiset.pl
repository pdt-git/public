/**
 * AccessorInterface - create set methods for all fields
 */
ct(aiset(ClassName), (
        classDefT(CID, _, ClassName,_),
        not(externT(CID)), not(interface(CID)),
        fieldDefT(FID, CID, Type, Name, _),
        not(modifierT(FID, 'final')),
        first_char_up(Name, UName),
		name_concat('set',UName,MethName),

        not(methodDefT(_MID, CID, MethName, [_], _, _, _)),
        new_id(MethID), new_id(Body), new_id(Return), 
        new_id(Assign),new_id(GetField),new_id(Param),
        new_id(ParamRef)
      ),(
        add(methodDefT(MethID, CID, MethName, [Param], Type, [], Body)),
        	add(paramDefT(Param,MethID,Type,'value')),
            shareModifier('static', FID, MethID),
            add(modifierT(MethID, 'public')),            
            add(blockT(Body, MethID, MethID, [Return])),
                add(returnT(Return, Body, MethID, Assign)),
                	add(assignT(Assign,Return,MethID,GetField,ParamRef)),
                    add(getFieldT(GetField, Assign, MethID, 'null', Name, FID)),
                    add(identT(ParamRef,Assign,MethID,'value',Param)),
        add_to_class(CID,MethID)
      )).
/**
TODO
Austausch von setField->MethodCall
*/
