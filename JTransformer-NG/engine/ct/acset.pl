/**
 * AccessorCode - replace all implicit writefield accesses with call to set method if existent
 */
 
ct(acset(ClassName), (
        classDefT(CID, _, ClassName,_), not(externT(CID)),
        fieldDefT(VID, CID, _, Name, _),
        not(modifierT(VID, 'final')),
        first_char_up(Name, UName),
		name_concat('set',UName,MethName),
        methodDefT(MID, CID, MethName, [_], _, _, _),
        getFieldT(Get, Assign, Encl, Recv, Name, VID),
        assignT(Assign,AssignParent,Encl, Get,Rhs),
        Encl\=MID
    ),(
        replace(applyT(Assign,AssignParent,Encl,Recv,MethName,[Rhs],MID)),
        delete(getFieldT(Get, Assign, Encl, Recv, Name, VID))
    )).
