/**
 * AccessorCode - replace all implicit writefield accesses with call to set method if existent
 */
ct(acget(ClassName), (
        classDefT(CID, _, ClassName,_), not(externT(CID)),
        fieldDefT(VID, CID, _, Name, _),
        not(modifierT(VID, 'final')),
        first_char_up(Name, UName),
		name_concat('get',UName,MethName),
        methodDefT(MID, CID, MethName, _, _, _, _),
        getFieldT(Get, Parent, Encl, Recv, Name, VID),
        not(assignT(Parent,_,Encl,Get,_)),
        Encl\=MID

    ),(
        replace(getFieldT(Get,Parent, Encl, Recv, Name, VID),
        applyT(Get, Parent, Encl, Recv, MethName, [],MID))
    )).