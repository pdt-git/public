:-multifile cond/1.
:-multifile action/1.
:-multifile subTreeArg/2.

cond(setField(_assignT, _parent, _encl, _Receiver, _field, _value)).
subTreeArg(setField, 4).
subTreeArg(setField, 6).

setField(SetField, RealParent, RealEncl, Receiver, Field,Value) :-
    assignT(SetField, Parent, Encl, GetField, Value),
    getFieldT(GetField, _, _, Receiver, _, Field),
    field(Field,_,_,_,_),
    getRealParent(SetField, Parent,RealParent),
    getRealEncl(SetField, Encl,RealEncl).
%    !,
%    nullIfThis(_receiver, _Receiver).

/*
setField(_setField, _parent, _encl, _Receiver, _field, _value) :-
    forwards(_setField,_,setField,_assign),
    applyT(_setField, _, _, _, [_receiver , _value]),
    getEncl(_setField, _encl),
    assignT(_assign, _,_, _identSelect, _value),
    nullIfThis(_receiver, _Receiver),
    getSymbol(_identSelect,_field).
    
*/

% TODO Port
action(add(setField(AssignT, Parent, Encl, Recv, Field, Value))) :-
    addToBlock(Parent, AssignT),
    field(Field,_,Type,_,_),
    new_id(GetField),
    add(getFieldT(GetField,AssignT,Encl,Recv,Type,Field)),
    add(assignT(AssignT, Parent, Encl, GetField, Value)).
    
action(replace(setField(AssignT, Parent, Encl, Recv, Field, Value))) :-
    field(Field,_,_,NewName,_),
    assignT(AssignT, Parent, Encl, GetField,Value),
    getFieldT(GetField,AssignT,Encl,OldRecv,OldName,OldField),
    replace(getFieldT(GetField,AssignT,Encl,OldRecv,OldName,OldField),
            getFieldT(GetField,AssignT,Encl,Recv,NewName,Field)),
    action(replace(assignT(AssignT, Parent, Encl, GetField, Value))).

action(replace(setField(AssignT, Parent, Encl, _Recv, _Field, Value),
	           setField(AssignT, Parent1, Encl1, Recv1, Field1, Value1))) :-
    field(Field1,_,_,Name1,_),
    assignT(AssignT, Parent, Encl, GetField,Value),
    getFieldT(GetField,AssignT,Encl,OldRecv,OldName,OldField),
    replace(getFieldT(GetField,AssignT,Encl,OldRecv,OldName,OldField),
            getFieldT(GetField,AssignT,Encl1,Recv1,Name1,Field1)),
    action(replace(assignT(AssignT, Parent1, Encl1, GetField, Value1))).

action(delete(setField(AssignT, Parent, Encl, Recv, Field, Value))) :-
    removeFromBlock(Parent, AssignT),
    assignT(AssignT, Parent, Encl, GetField, Value),
    getFieldT(GetField,AssignT,Encl,Recv,Name,Field),
    delete(getFieldT(GetField,AssignT,Encl,Recv,Name,Field)),
    delete(assignT(AssignT, Parent, Encl, GetField, Value)).

cond(getField(_getField, _parent, _encl, _Receiver, _name, _field)).
subTreeArg(getField, 4).

getField(_getField, _Parent, _Encl, _Receiver, _Name,_field) :-
    getFieldT(_getField, _parent, _encl, _receiver, _Name, _field),
    not(assignT(_parent, _,_,_getField,_)),
    getRealParent(_getField, _parent,_Parent),
    getRealEncl(_getField, _encl,_Encl),
    nullIfThis(_receiver, _Receiver).


action(add(getField(_identSelect, _parent, _encl, _recv, _name,_field))) :-
    field(_field,_,_,_name,_),
    addToBlock(_parent, _identSelect),
    add(getField(_identSelect, _parent, _encl, _name, _recv, _field)).
    
%    createIdentSelect(_identSelect, _parent, _encl, _name, _recv, _field).
action(replace(getField(_getField, _parent, _encl, _recv, _field))) :-
    field(_field,_,_,_name,_),
    deleteTree(_getField),
    add(getField(_getField, _parent, _encl, _recv, _field)).
%    replaceIdentSelect(_identSelect, _parent, _encl, _name, _recv, _field).
    
    

action(delete(getField(GetField, _, _, _, _, _))) :-
	getField(GetField, Parent, Encl, Recv, Name ,Field),
    removeFromBlock(_parent, _identSelect),
    delete(getField(GetField, Parent, Encl, Recv, Name ,Field)).


/* execution: entspricht methode*/
cond(execution(_execution, _class, _execution, null, _execution, _params)).
subTreeArg(execution, 6).
execution(_execution, _class, _execution, null, _execution, _params) :-
    methodDefT(_execution, _class, _, _params, _, _exceptions, _),
    not(externT(_class)).


cond(methodCall(_apply, _parent, _encl, _Receiver, _Name, _OrigMethod, _Args)).
subTreeArg(methodCall, 4).
subTreeArg(methodCall, 7).

/* 
   methodCall(?Call, ?Parent, ?Encl, ?Receiver, ?Method, ?Args)

	Wrapper predicate for applyT.
	and newClassT
	Method is the original method, 
	even if apply points to a forwarding method */

methodCall(_methodCall, _parent, _Encl, _Receiver, _method_name, _method, _Args) :-
    applyT(_methodCall, _parent, _encl, _Receiver, _method_name, _args,_method),
    not(_method_name == 'super'),
    not(forwarding(_methodCall)),
%    pc_visible(_encl),
    getRealParent(_methodCall,_parent,_Parent),
    getRealEncl(_methodCall, _encl,_Encl),
    getRealArgs(_methodCall,_args,_Args).

methodCall(_methodCall, _parent, _Encl, null, '<init>', _constructor, _Args) :-
    newClassT(_methodCall, _parent, _encl, _constructor, _args, _typeExpr, _def, _enclosingClass),
%    _Receiver, _method_name, _args,_method),
    not(forwarding(_methodCall)),
%    pc_visible(_encl),
    getRealParent(_methodCall,_parent,_Parent),
    getRealEncl(_methodCall, _encl,_Encl),
    getRealArgs(_methodCall,_args,_Args).

%    !,
%    getEncl(_methodCall, _encl),
%    getReceiverNullIfThis(_identSelect, _Receiver),
%    getSymbol(_identSelect, _method).

/** Uwe TODO does NOT handle forwarding methods */
action(methodCall(_apply, _parent, _encl, _recv, _name, _method, _Args)) :-
    methodDefT(_method, _, _name, _, _, _, _),
    addToBlock(_parent, _apply),
    new_id(_newIdSelect),
%    createIdentSelect(_newIdSelect, _apply, _encl, _name, _recv, _method),
    add(applyT(_apply, _parent, _encl, _recv,_name, _Args, _method)).

action(replace(methodCall(_apply, _parent, _encl, _recv, _name, _method, _args))) :-
	applyT(_apply, _, _, _,_name, _, _),
    enclMethod(_apply, _e),
    deleteTree(_apply),
    methodDefT(_method, _, _name, _, _, _, _),
    new_id(_newIdSelect),
    add(applyT(_apply, _parent, _encl, _recv,_name, _args, _method)).

%    format("replace start ~a,~a,~a,~a,~a,~a~n", [_newIdSelect, _apply, _encl, _name, _recv, ]),
%    action(replaceDiffTree(applyT(_apply, _parent, _encl, _newIdSelect, _Args))).
%    format("replace end ~a,~a,~a,~a,~a,~a~n", [_newIdSelect, _apply, _encl, _name, _recv, ]).
%    action(replace(applyT(_apply, _parent, _encl, _newIdSelect, _Args))).

action(delete(methodCall(_apply, _, _, _, _, _, _))) :-
    removeFromBlock(_parent, _apply),
    delete(applyT(_apply, _, _, _, _, _, _)).

/**************************************************************
 * Facility predicates for the AOP abstractions.
 * Must not be hidden, because LogicAJ uses them 
 * when generating forwarding methods!
 **************************************************************/

    
/**
  getRealParent(_id,_Parent)
  @descr Gibt "realen" parent des durch _id bestimmten
         trees zur�ck. <br>
         Wenn der tree durch eine Forwarding Methode gekapselt wurde
         so wird der urspr�ngliche Parent zur�ckgegeben, sonst der
         im tree angegebene "low level" parent.
*/

getRealParent(_id, _,_Parent) :-
    forwards(_forwCall,_,_,_id),
    !,
    applyT(_forwCall,_Parent,_,_,_,_,_).

getRealParent(_,_parent,_parent).

/**
  @form getRealEncl(_meth,_Encl)
  @constraints
    @ground _meth
    @unrestricted _Encl)
  @descr Gibt die "real" umschlie�ende Methode zur�ck.
         Wenn die durch _meth bestimmte Methode eine Forwarding Methode ist
         so wird die Methode zur�ckgegeben in der sie aufgerufen wird.
         getRealEncl geht hierbei rekursiv vor.
         Bestimmt _meth keine Methode, so wird eine Exception geworfen:
         'getRealEnclMethod: first Argument must be a method id'
*/

getRealEncl(_pc,_,_Encl) :-
    forwards(_forwCall,_,_,_pc),
    !,
    applyT(_forwCall,_,_Encl,_,_,_,_).
%    getRealEncl(_r_encl, _Encl).

getRealEncl(_pc,_encl,_Encl) :-
    forwarding(_encl, _,_stmt),
    forwards(_forwCall, _forwMethod, _type, _stmt),
    applyT(_forwCall,_,_Encl,_,_,_,_).

getRealEncl(_,_encl, _encl):-
    methodDefT(_encl,_,_,_,_,_,_).

getRealArgs(_pc,_args,_Args):-
    forwards(_forwCall,_,_,_pc),
    !,
    applyT(_forwCall,_,_,_,_,[_|[_|_Args]],_).

getRealArgs(_,_Args,_Args).

:- dynamic forwards/4.

/**
  getRealStat(Stat,RealStat)
  
  Binds RealStat to the "real" tree at a joint point.
  This is the first called forwarding method or the
  original element if it has not been wrapped by a
  forwarding method.
*/

getRealStat(_stat, _Apply) :-
    forwards(_apply,_forw,_,_stat),
    !,
    getRealStat(_apply, _Apply).
getRealStat(_stat, _stat).

/*
getRealStatRec(_stat, _Apply) :-
    forwards(_apply,_forw,_,_stat),
    !,
    getRealStatRec(_apply, _Apply).

getRealStatRec(_apply, _apply).
*/
    