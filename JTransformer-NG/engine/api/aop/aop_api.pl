 % Author: Tobias Windeln
% Date: 30.08.02
:- multifile fieldAccess/2.
:- multifile action/1.
:- dynamic forwarding/3.
:- dynamic forwarding/1.
:- dynamic pointcut/1.
:- dynamic merge_advices/0.
:- dynamic visibility/3.
:- dynamic aj_ct_list/1.

%:- assert(merge_advices).
/*

*** AOP Features ***
before                  (#elem, #insert).
after                   (#elem, #insert).
around                  (#elem, #before, #after). //??? Semantik ???

*** AOP Helper ***
createForwardingMethod  (_origMethod, _class, _forwardingMethod)
replaceStatWithForw             (_apply, _newMethod, _oldMethod)

*/

/************************** AOP Features *************************/

/*
    around
*/

action(around(_stat, _aroundStmts,_forwMethod,_forwBody)):-
    around(_stat,_aroundStmts,_forwMethod,_forwBody).

around(_method,_aroundStmts,_,_forwBody) :-
    methodDefT(_method,_,_,_,_,_,_),
    createForwMethodExecution(_method, _forwBody, _aroundStmts).


around(_stat,_aroundStmts,_forwMethod,_forwBody) :-
    (
        (
            not(forwards(_,_,_,_stat)),
            replaceStatementWithForwarding(_stat)
        );
        true
    ),
    getRealStat(_stat,_realStat),
    enclClass(_realStat, _enclClass),
    createAroundMethod(_realStat, _enclClass, _aroundStmts,_forwMethod,_forwBody).
    
action(before(_id, _interID,_forwMethod,_forwBody)) :-
    %printf('before(~a,~a).~n',[_id, _interID]),
    before(_id, _interID,_forwMethod,_forwBody),
    !.
action(after(_id, _interID,_forwMethod,_forwBody)) :-
%    printf('after(~a,~a).~n',[_id, _interID]),
    after(_id, _interID,_forwMethod,_forwBody),
    !.
    
% das wieder eine forw. Meth. erzeugt wurde muss hier erneut getRealStat aufgerufen werden.
%    getRealStat(_realStat,_realRealStat),
%    forwards(_realRealStat,_forwMethod,_,_). %_realStmt forwards to the around forwarding Method


/*
    before

    F�gt vor einem Statement _call (bisher nur MethodCall)
    ein weiteres Statement _insert ein.
    Hierzu wird eine Forwarding Methode erzeugt (falls noch keine existiert)
    in der der Methodenaufruf / Feldzugriff realisiert wird.
    Am Anfang der Forw. Methode wird das _insert Statement einf�gt.
*/

replaceStatementWithForwarding(_stat) :-
    replaceStatementWithForwarding(_stat,_,_).


replaceStatementWithForwarding(_stat,_forwMethod,_forwBody) :-
    var(_forwMethod),
    var(_forwBody),
    new_ids([_forwMethod,_forwBody]),
    replaceStatementWithForwarding(_stat,_forwMethod,_forwBody).

replaceStatementWithForwarding(_stat,_forwMethod,_forwBody) :-
    nonvar(_forwMethod),
    nonvar(_forwBody),
    (
        (forwards(_realStat, _, _,_stat),!);
        _stat = _realStat
    ),
    (
        (merge_advices,not(forwards(_, _, _,_stat)));
        not(merge_advices)
    ),
    enclClass(_realStat, _enclClass),
    !,
    createForwardingMethod(_realStat, _enclClass,_forwMethod,_forwBody).

/***********************
 *     before advice
 ***********************/

/* forwarding method does NOT exist*/
before(_body, _insertList,_,_) :-
    blockT(_body, _p, _e, _stats),
    !,
    prependBlockStatments(_body, _insertList).


/* forwarding method does exist */
before(_stat, _insertList,_forwMethod,_forwBody) :-
    (replaceStatementWithForwarding(_stat,_forwMethod,_forwBody);true),
    %forwards(_, _forwMethod, _,_stat),
    %blockT(_body, _forwMethod, _, _),
    prependBlockStatments(_forwBody, _insertList).

/***********************
 *     after advice
 ***********************/

after(_stat, _insertList,_forwMethod,_finallyBlock) :-
    new_id(_forwBody),
    (replaceStatementWithForwarding(_stat,_forwMethod,_forwBody);true),
    addTryFinallyBlockStmts(_forwMethod, _finallyBlock, _insertList).

/*********************************
 *     before and after advice   *
 *********************************/

%bindForwMethod('around',_,_forwMethod,_forwBody):-
%    !,
%    new_ids([_forwMethod,_forwBody]).


% Ausnahme f�r den execution pointcut
bindForwMethod(_,_Method,_Method,_Body):-
    methodDefT(_Method,_,_,_,_,_,_),
    !,
    new_id(_Body).


bindForwMethod('before',_stat,_forwMethod,_forwBody):-
    merge_advices,
    forwards(_,_forwMethod,_,_stat),
    !,
    blockT(_forwBody,_forwMethod,_forwMethod,_).


bindForwMethod('after',_stat,_forwMethod,_finallyBlock):-
    merge_advices,
    forwards(_,_forwMethod,_,_stat),
    !,
    new_id(_finallyBlock).

bindForwMethod(_,_,_forwMethod,_forwBody):-
    new_ids([_forwMethod,_forwBody]).
/***********************
 *     around advice
 ***********************/

/************************** AOP Helper ***************************/

/*
 * Umschlie�e aktuellen Body mit einem try .. finally block und f�ge _insert in den finally Block ein,
 * wenn _insert ein Block ist wird insert als finally block eingef�gt.
c */

addTryFinallyBlockStmts(_,_, []).
addTryFinallyBlockStmts(_forwMethod,_finallyBlock, _stmts) :-
    addTryFinallyBlock(_forwMethod,_finallyBlock),
    prependBlockStatments(_finallyBlock, _stmts).

addTryFinallyBlock(_forwMethod,_finallyBlock):-
    new_ids([_try, _tryBlock]),
    methodDefT(_forwMethod,_,_,_,_,_,_block),
    blockT(_block, _parent, _encl, _stats), % die id des blocks wird dem try zugewiesen
    set_parent(_stats, _tryBlock),
    delete(blockT(_block, _parent, _encl, _stats)),
    add(blockT(_block, _parent,_encl,[_try])),
        add(tryT(_try, _block,_encl,_tryBlock, [],_finallyBlock)),
            add(blockT(_tryBlock, _try,_encl,_stats)),
            add(blockT(_finallyBlock, _try, _encl, [])).

/*
aroundTryFinallyBlock(_block, _finallyBlock) :-
    blockT(_finallyBlock,_,_,_),
    !,
    new_ids([_try, _tryBlock]),
    blockT(_block, _parent, _encl, _stats), % die id des blocks wird dem try zugewiesen
    set_parent(_stats, _tryBlock),
    retractall(blockT(_block, _,_,_)),
    assert(blockT(_block, _parent,_encl,[_try])),
        assert(tryT(_try, _block,_encl,_block, [],_finallyBlock)),
            assert(blockT(_tryBlock, _try,_encl,_stats)),
            set_parent(_finallyBlock, _try),
            set_encl_method(_finallyBlock, _encl).

aroundTryFinallyBlock(_block, _insert) :-
    new_ids([_try, _tryBlock, _finallyBlock]),
    blockT(_block, _parent, _encl, _stats), % die id des blocks wird dem try zugewiesen
    set_parent(_stats, _tryBlock),
    retractall(blockT(_block, _,_,_)),
    assert(blockT(_block, _parent,_encl,[_try])),
        assert(tryT(_try, _block,_encl,_tryBlock, [],_finallyBlock)),
            assert(blockT(_tryBlock, _try,_encl,_stats)),
            assert(blockT(_finallyBlock, _try, _encl, [])),
%    set_parent(_insert,_finallyBlock),
    prependBlockStatment(_finallyBlock, _insert).
*/



createThisOrGetReceiver(_ident, _newParent, Receiver) :-
    getFieldT(_ident, _parent, _encl, 'null', _name, _sym),
    !,
    enclClass(_encl, _enclClass),
    new_id(Receiver),
    add(identT(Receiver, _newParent, _encl, 'this', _enclClass)).

createThisOrGetReceiver(GetField, NewParent, Receiver) :-
    getFieldT(GetField, _parent, _, Receiver, _, _),
    !,
    set_parent(Receiver, NewParent).


/** BEFORE helper - START*/
createForwBody(_get, _forwMethod, _forwBody, _ForwName, [_thisParam,_recvParam], _Type, _execReturn) :-
    getFieldT(_get, _parent, _enclMethod, _origReceiver,  _origName, _field),
    !,
    new_ids([_forwCall, _forwMethIdent]),
    fieldDefT(_field, _, _Type, _origName, _),
    forwardingMethodName(_get, 'get$', _origName, _ForwName),
    enclClass(_enclMethod,_enclClass), %neu
    createForwMethParams(_enclClass,_forwMethod,_origReceiver, [],[_thisParam,_recvParam]),
    createIdentRefParam(_recvParam,_callSelect, _forwReceiver),
    createThisOrGetReceiver(_get, _forwCall,_receiver),
    replaceId(_parent, _get, _forwCall),
    delete(getFieldT(_get, _parent, _enclMethod, _origReceiver,  _origName, _field)),
    createThisIdent(_this,_forwCall, _enclMethod, _enclClass), %neu
    add(getFieldT(_get, _execReturn, _forwMethod, _forwReceiver, _origName, _field)),
    add(applyT(_forwCall, _parent,_enclMethod, 'null', _ForwName, [_this,_receiver],_forwMethod)),
    updateForwardsFact(_get,getField,_forwCall,_forwMethod).

createForwBody(_set, _forwMethod, _forwBody, _ForwName, [_thisParam,_recvParam,_valueParam], _Type, _execReturn) :-
    setField(_set, _parent, _enclMethod, _origReceiver, _field,_value),
    !,
    new_ids([_forwCall, _forwMethIdent, _selectField,_valueParam]),
    assignT(_set, _, _enclMethod, _lhs, _),
    fieldDefT(_field, _, _, _origName, _),
    getType(_value,_Type),
    forwardingMethodName(_set,'set$', _origName, _ForwName),
    enclClass(_enclMethod,_enclClass), %neu
    createForwMethParams(_enclClass,_forwMethod,_origReceiver, [],[_thisParam,_recvParam]),
    createIdentRefParam(_recvParam,_callSelect, _forwReceiver),
    createThisOrGetReceiver(_lhs, _forwCall,_receiver),
    replaceId(_parent, _set, _forwCall),
    set_parent(_value, _forwCall),
    deleteTree(_lhs),
    add(paramDefT(_valueParam, _forwMethod, _Type, '_value')),
    createIdentRefParam(_valueParam,_set, _forwValue),
    createThisIdent(_this,_forwCall, _enclMethod, _enclClass), %neu
    action(replace(assignT(_set, _execReturn, _forwMethod, _selectField, _forwValue))),
      add(getFieldT(_selectField, _set, _forwMethod, _forwReceiver, _origName, _field)),
    add(applyT(_forwCall, _parent,_enclMethod, 'null',_ForwName, [_this,_receiver,_value],_forwMethod)),
    updateForwardsFact(_set,setField,_forwCall,_forwMethod).

createForwBody(_call, _forwMethod, _forwBody, _ForwName, [_thisParam|[_recvParam|_forwParams]], Type, _execReturn) :- %neu
    (
    	(applyT(_call,_parent,_enclMethod,_expr,_origName,_args, _method_constr),
    	 method(_method_constr,_,_,Type,_,_,_));
	    (newClassT(_call,_parent,_enclMethod,_method_constr,_args,_,_,_),
	    _expr = null,
	    constructor(_method_constr,ConstrClass,_,_,_),
	    Type = type(class,ConstrClass,0),
	     _origName = 'init')
	),
    !,
    getOrigArgs(_call,_args,_origArgs),
    new_ids([_forwCall, _forwMethIdent]),
%    applyT(_call, _parent, _enclMethod, _origReceiver, _),
    forwardingMethodName(_call, 'call$',_origName, _ForwName),
    enclClass(_enclMethod,_enclClass), %neu
    createForwMethParams(_enclClass,_forwMethod,_expr,_origArgs,[_thisParam|[_recvParam|_forwParams]]),
    replaceId(_parent, _call, _forwCall),
    getOrigParams(_call,[_thisParam|[_recvParam|_forwParams]],_origParams),
    replaceForwCall(_call, _parent, _method_constr, _origName, _execReturn, _forwMethod,_origParams,_recvParam),
    createForwArgs(_call,_enclMethod,_enclClass,_forwCall,_expr,_args,_forwArgs),
    set_parent(_forwArgs,_forwCall),
    add(applyT(_forwCall, _parent,_enclMethod, 'null', _ForwName, _forwArgs,_forwMethod)),
    updateForwardsFact(_call,methodCall,_forwCall,_forwMethod).

% execution
createForwMethodExecution(_method, _forwBody,_forwStmts) :- %neu
    methodDefT(_method, _class, _origName, _origParams, _type, _exceptions, _body),
    new_ids([_forwCall, _forwMethIdent,_forwMethod]),

%    applyT(_method, _parent, _method, _origReceiver, _),
    forwardingMethodName(_method, 'exec$',_origName, _forwName),
    enclClass(_method,_class), %neu

% ersetzte alte Methode mit neuem Namen
    rec_set_encl_method(_body,_forwMethod),
    set_parent(_body,_forwMethod),
    
    cloneParams(_method, _origParams,_newParams),
    createThisInstanceParam(_class,_forwMethod,_thisVarDef),
    createReceiverParam(_class, _forwMethod, 'null',_targetVarDef),
    set_parent(_origParams,_forwMethod),
    rec_set_encl_method(_origParams,_forwMethod),

    delete(methodDefT(_method, _class, _origName, _origParams, _type, _exceptions, _body)),

    add(methodDefT(_method, _class, _origName, _newParams, _type, _exceptions, _forwBody)),
    add(methodDefT(_forwMethod, _class, _forwName, [_thisVarDef | [_targetVarDef |_origParams]], _type, _exceptions, _body)),

    cloneModifier(_method,_forwMethod),
    add_to_class(_class, _forwMethod),

    % add forwarding call
    ((_forwStmts == []) -> (
        new_ids([_execReturn,_thisIdent,_targetIdent]),

        reccreateVarDefIdents(_forwCall, _newParams,_forwArgs),
        rec_set_encl_method(_forwArgs,_method),
        add(applyT(_forwCall, _execReturn,_method, 'null', _forwName, [_thisIdent| [_targetIdent | _forwArgs]], _forwMethod)),
        add(identT(_thisIdent, _forwCall, _method, 'this', _class)),
        add(identT(_targetIdent, _forwCall, _method, '   this', _class)),
        createReturnOrExec(_forwBody, _forwMethod, _type, _forwCall, _execReturn),
        _bodyStmts = [_execReturn]
    );
        _bodyStmts = _forwStmts
    ),
    add(blockT(_forwBody, _method,_method, _bodyStmts)),%neu
    updateForwardsFact(_method,execution,_method,_forwMethod).


createAroundBody(_call, _forwMethod, _forwBody, _ForwName,_forwParams, _Type) :-
    applyT(_call,_parent,_enclMethod,_expr,_origName,_args, _method),
    !,
    getOrigArgs(_call,_args,_origArgs),
    new_ids([_forwCall, _forwMethIdent]),
%    applyT(_call, _parent, _enclMethod, _origReceiver, _),
    method(_method, _, _origName, _, _Type, _exc, _),
    forwardingMethodName(_call, 'call$',_origName, _ForwName),
    enclClass(_enclMethod,_enclClass),
    createForwMethParams(_enclClass,_forwMethod,_expr,_origArgs,_forwParams),
    replaceId(_parent, _call, _forwCall),
    createForwArgs(_call,_enclMethod,_enclClass,_forwCall,_origReceiver,_args,_forwArgs),
    set_parent(_forwArgs,_forwCall),
    add(applyT(_forwCall, _parent,_enclMethod, 'null', _ForwName, _forwArgs, _forwMethod)),
%    retractall(applyT(_call, _,_,_,__,_)), %new: remove old forwarding Appl
    updateForwardsFact(_call,methodCall,_forwCall,_forwMethod).%new: update


replaceForwCall(_call, _parent, _method, _origName, _execReturn,_forwMethod,_forwParams,_recvParam):-
    applyT(_call,_,_,_,_,_,_),
      createIdentRefParam(_recvParam,_callSelect, _forwReceiver),
      createVarDefIdents(_call, _forwParams, _argsInForw),
    action(replace(applyT(_call, _execReturn, _forwMethod, _forwReceiver,_origName, _argsInForw,_method))).

replaceForwCall(_call, _parent, _method, _origName, _execReturn,_forwMethod,_forwParams,_recvParam):-
    newClassT(_call,_,_,Constructor,_,TypeExpr,Def,Enclosing),
    createVarDefIdents(_call, _forwParams, _argsInForw),
    action(replace(newClassT(_call, _execReturn, _forwMethod, Constructor,_argsInForw,TypeExpr,Def,Enclosing))).			    


createForwArgs(_call,_,_,_,_,_Args,_Args):-
    forwards(_call,_,_,_),
    !.
createForwArgs(_call,_enclMethod,_enclClass,_forwCall,_origReceiver,_Args,[_This|[Receiver|_Args]]):-
    createThisIdent(_This,_forwCall, _enclMethod, _enclClass),
    ( 
      (_origReceiver = null,
       createThisIdent(Receiver,_forwCall, _enclMethod, _enclClass)
      );
      _origReceiver = Receiver
    ).
    %createThisOrGetReceiver(_origReceiver, _forwCall,_Receiver).


getOrigParams(_call,_Params,_Params):-
    forwards(_call,_,_,_),
    !.

getOrigParams(_,[_|[_|_ForwParams]],_ForwParams).


getOrigArgs(_call,_args,_OrigArgs) :-
    forwards(_call,_,_,_),
    !,
    _args = [_|[_|_OrigArgs]].

getOrigArgs(_call,_Args,_Args).

updateForwardsFact(_call,_,_forwCall,_forwMethod):-
    forwards(_call,_lastForwMethod,_kind,_pc),
    !,
    delete(forwards(_call,_lastForwMethod,_kind,_pc)),
    add(forwards(_forwCall, _forwMethod, _kind, _pc)),
    add(forwarding(_forwMethod, _lastForwMethod,_pc)).
%    add(forwarding(_forwCall)).

updateForwardsFact(_stmt,_kind, _forwCall,_forwMethod):-
    add(forwards(_forwCall, _forwMethod, _kind, _stmt)),
    add(forwarding(_forwMethod, _stmt,_stmt)).
%    add(forwarding(_forwCall)).

% DEBUG commented
createForwardingMethod(_method, _class,_,_forwBody) :-
    methodDefT(_method,_class,_,_,_,_,_),
    !,
    createForwMethodExecution(_method, _forwBody,[]).
%    createForwMethodExecution(_method,_forwMethod,_forwBody).


createForwardingMethod(_stat, _class,_forwMethod,_forwBody) :-
    new_id(_execReturn),
    createForwBody(_stat, _forwMethod, _forwBody, _forwName, _params, _type, _execReturn),
    createReturnOrExec(_forwBody, _forwMethod, _type, _stat, _execReturn),
    add_to_class(_class, _forwMethod),
    add(methodDefT(_forwMethod, _class, _forwName, _params, _type, [], _forwBody)),
    add(modifierT(_forwMethod, 'private')),
    add(blockT(_forwBody, _forwMethod, _forwMethod, [_execReturn])).


createAroundMethod(_stat, _class,_forwStats,_forwMethod,_forwBody) :-
%    new_ids([_forwMethod, _forwBody]),
    createAroundBody(_stat, _forwMethod, _forwBody, _forwName, _params, _type),
%    createReturnOrExec(_forwBody, _forwMethod, _type, _stat, _execReturn),
    add_to_class(_class, _forwMethod),
    add(methodDefT(_forwMethod, _class, _forwName, _params, _type, [], _forwBody)),
    add(modifierT(_forwMethod, 'private')),
    add(blockT(_forwBody, _forwMethod, _forwMethod, _forwStats)).

debugme(_id):-
    format('debug: ~a',[_id]),
    flush_output.


forwardingMethodName(_stat, _prefix, _origName, _ForwName) :-
    new_id(_aNumber),
    int2string(_aNumber, _aString),
    stringAppend(_origName,'$',_aString,_forwName),
    appendForwPrefix(_stat,_prefix, _forwName,_ForwName).

appendForwPrefix(_stat,_, _forwName,_forwName):-
    forwards(_stat,_,_,_),
    !.

appendForwPrefix(_stat,_prefix, _forwName,_ForwName) :-
    stringAppend(_prefix, _forwName, _ForwName).
%    stringAppend('forward$', _prefix, _forwName, _ForwName).

    %    add(_Receiver, _apply, _encl, 'this', _enclClass).


prependBlockStatments(_, []).
prependBlockStatments(_block, _stmts) :-
    blockT(_block, _parent, _encl, _oldStmts),
    append(_stmts,_oldStmts,_newStmts),
    delete(blockT(_block, _parent, _encl, _oldStmts)),
    add(blockT(_block, _parent, _encl, _newStmts)).

prependBlockStatment(_block, _pre) :-
    blockT(_block, _parent, _encl, _stats),
    prepend(_stats, _pre, _newStats),
%    rec_set_encl_method(_pre, _encl),
%    rec_set_parent(_pre, _block),
    delete(blockT(_block, _parent, _encl, _stats)),
    add(blockT(_block, _parent, _encl, _newStats)).


appendBlockStatment(_block, _post) :-
    blockT(_block, _parent, _encl, _stats),
    append(_stats, [_post], _newStats),
    rec_set_encl_method(_post, _encl),
    rec_set_parent(_post, _block),
    delete(blockT(_body, _p, _e, _stats)),
    add(blockT(_body, _p, _e, _newStats)).


    /*
  restriction elements
  only availabe for the precondition
  optimization for PatternParams and ArgsPcd ?
*/

cond(matchParams(_vardefs, _patterns)).
matchParams([], []).

% FIXME sehr schlechter workaround f�r declare error, mock objects
%matchParams([VdHead | VdTail], [typePattern(bindTypeList(FNList), _dim)]) :-
%    paramDefT(VdHead,_,_,_),
%    !,
%    matchParamTypeNameList([VdHead | VdTail],FNList).
% FIXME sehr schlechter workaround f�r declare error, mock objects

matchParams([VdHead | VdTail], [Term | PatTail]) :-
    nonvar(Term),
    Term = type(Type),
    getType_fq(VdHead,Type),
    matchParams(VdTail,  PatTail).
 

matchParams([VdHead | VdTail], [Term | PatTail]) :-
    nonvar(Term),
    Term = params([Head|Tail]),
    (
      var(Head) ->
        Head = VdHead;
        (getType(Head,Type),getType(VdHead,Type))
    ),
    matchParams(VdTail, [params(Tail) | PatTail]).

matchParams(VdHead, [Term | PatTail]) :-
    nonvar(Term),
    Term = params([]),
    matchParams(VdHead, PatTail).


matchParams([VdHead | VdTail], [List | PatTail]) :-
    nonvar(List),
    List = [_pattern , VdHead],
    !,
    matchParams([VdHead | VdTail], [_pattern | PatTail]).

matchParams([VdHead | VdTail], [TypePattern | PatTail]) :-
    nonvar(TypePattern),
    TypePattern = typePattern(_pattern, _dim),
    getType(VdHead, type(_kind, _id,_dim)),
    getTypeName(type(_kind, _id,_dim), _name),
    matchPatterns(_name, _pattern),
    matchParams(VdTail, PatTail).

matchParamTypeNameList([],[]).
matchParamTypeNameList([Head | Tail],[FNHead|FNTail]):-
    paramDefT(Head,_,Type,_),
    getTypeName(Type,FNHead),
	matchParamTypeNameList(Tail,FNTail).
	
	

test(_,[]).
test(_1,[_h|_t]) :-
     _h = 'h',
     test(_1, _t).

matchPatterns(_, []).
matchPatterns(_name, (_pat1;_pat2)) :-
    !,(
    matchPatterns(_name, _pat1);
    matchPatterns(_name, _pat2)
    ).

matchPatterns(_name, (_pat1,_pat2)) :-
    !,
    matchPatterns(_name, _pat1),
    matchPatterns(_name, _pat2).

matchPatterns(_name, _pat) :-
    pattern(_pat, _, _name).

/*
    action elements
*/


action(addAdvice(_adviceType, _pc, _adviceMeth, _adviceArgs)):-
    addAdvice(_adviceType,_pc, _adviceMeth, _adviceArgs).

addAdvice(_adviceType,_pc, _adviceMeth, _adviceArgs) :-
    callToAdviceMethod(_pc, _adviceMeth, _adviceArgs,_exec),
    weave(_adviceType,_pc,_exec).

weave(before, _pc,_exec):-
    before(_pc, _exec).

weave(after, _pc,_exec):-
    after(_pc, _exec).


action(add_advice_param_ref(_pc,_adviceParam,_id,_parent,_encl)):-
    add_advice_param_ref(_pc,_adviceParam,_id,_parent,_encl).


%add_advice_param_ref(_pc,_adviceParam,_id,_parent,_encl) :-
%    add_advice_param_ref(_pc,_adviceParam,_id,_parent,_encl).

/*
add_advice_param_ref(_method,_adviceParam,_id,_parent,_encl):-
%    methodDefT(_method,_class,_,_,_,_,_),
    getForwParamExecution(_method, _adviceParam, _param,_name),
    add(identT(_id,_parent,_encl,_name,_param)).
*/

add_advice_param_ref(_pc,_adviceParam,_id,_parent,_encl):-
    getForwParam(_pc, _adviceParam, _param,_name),
    add(identT(_id,_parent,_encl,_name,_param)).


callToAdviceMethod(_pc, _adviceMeth, _adviceArgs,_exec,_forwMethod,_forwBody) :-
    (replaceStatementWithForwarding(_pc,_forwMethod,_forwBody); true),
    methodDefT(_adviceMeth,Aspect,_adviceMethName, _,_,_,_),
    fieldDefT(_adviceInstanceVar,Aspect,_,'aspectInstance',_),
    classDefT(Aspect,_,AdviceName,_),
    new_ids([_exec, _apply, _selectAdviceMethod,_selectInstField, _ident]),
    enclMethod(_pc,_enclMethod),
    createIdentsReferencingAdviceParams(_pc, _enclMethod,_adviceArgs, _adviceCallArgs),
    add(execT(_exec, 0,0, _apply)),
    add(applyT(_apply, _exec,0, _selectInstField,_adviceMethName, _adviceCallArgs,_adviceMeth)),
    add(getFieldT(_selectInstField, _apply, 0, _ident, 'aspectInstance',_adviceInstanceVar)),
    add(identT(_ident, _selectInstField, 0, AdviceName, Aspect)).


createIdentsReferencingAdviceParams(_,_,[],[]).

createIdentsReferencingAdviceParams(_call,_enclMethod,[_adviceArg|_adviceArgs],[_Ident|_Idents]):-
    new_id(_Ident),
    getForwParam(_call, _adviceArg, _param,_name),
    add(identT(_Ident, _call, _enclMethod, _name, _param)),
    createIdentsReferencingAdviceParams(_call,_enclMethod,_adviceArgs,_Idents).


action(add_proceed_call(_pc,_call,_parent, _enclMethod,_adviceArgs,_proceedArgs)):-
    add_proceed_call(_pc,_call, _parent, _enclMethod,_adviceArgs,_proceedArgs).

add_proceed_call(_method,_call, _parent, _enclMethod,_adviceArgs,_proceedArgs):-
    method(_method, _, _, _forwParams, _, _, _),
    !,
    forwards(_method,_methodToCall,_,_),
    method(_methodToCall, _, _name, [_this|[_target|_params]], _, _, _),
    add_proceed_call_idents(_method,_call,_enclMethod,_adviceArgs,  [_this|[_target|_params]],  [_this|[_target|_forwParams]],_proceedArgs,_args),
    add(applyT(_call, _parent,_enclMethod, _expr, 'null',_args,_methodToCall)),
    add(forwarding(_call)).

add_proceed_call(_pc,_call, _parent, _enclMethod,_adviceArgs,_proceedArgs):-
    forwarding(_enclMethod,_methodToCall,_),
    method(_enclMethod, _, _, _params, _, _, _),
    method(_methodToCall, _, _name, _, _, _, _),
    add_proceed_call_idents(_pc,_call,_enclMethod,_adviceArgs, _params, _params,_proceedArgs,_args),
    add(applyT(_call, _parent,_enclMethod, 'null',_name, _args, _methodToCall)),
    add(forwarding(_call)).


action(add_unboxing_return(_return, _parent, _encl, _expr)):-
    add_unboxing_return(_return, _parent, _encl, _expr).

add_unboxing_return(_return, _parent, _encl, _expr):-
    methodDefT(_encl,_,_, _,type(basic,void,0),_,_),
    !,
    deleteTree(_expr), 
    add(returnT(_return,_parent,_encl,null)).


add_unboxing_return(_return, _parent, _encl, _expr):-
    enclClass(_encl,_enclClass),
    methodDefT(_encl,_,_, _,type(basic,_primitiveType,0),_,_),
    !,
    apply_ct(unboxingMethod(_enclClass,_primitiveType)),
    atom_concat(_primitiveType, 'Value', _primitiveTypeValue),
    methodDefT(_unboxingMethod, _enclClass,_primitiveTypeValue,[_],type(basic, _primitiveType, 0),[],_),
    new_ids([_apply,_select,_ident]),
    atom_concat(_primitiveType,'Value',_methodName),
    add(returnT(_return,_parent,_encl,_apply)),
      add(applyT(_apply,_return,_encl,_ident,_methodName, [_expr],_unboxingMethod)),
          add(identT(_ident,_apply,_encl,'this',_enclClass)),
    set_parent(_expr,_apply).


add_unboxing_return(_return, _parent, _encl, _expr):-
    add(returnT(_return,_parent,_encl,_expr)).


test(add_unboxing) :-
    add(methodDefT(encl, class, name, [], type(basic,int,0), exceptions, body)),
    add(classDefT(class, package, classname, [encl])),
     add_unboxing_return(return, parent, encl, expr),
     gen_tree(class),
     deleteTree(class),
     deleteTree(encl),
     not(tree(parent,_,_)),
     not(tree(encl,_,_)),
     not(tree(expr,_,_)).

% not tested yet
add_boxing(type(basic,_primitiveType,0), _return, _parent, _encl, _expr):-
    boxing_class(_primitiveType, _class),
    classDefT(_class,_,_classname,_),
    new_ids([_new,_ident]),
    methodDefT(_constructor,_class,'<init>', _param,_,[],_),
    paramDefT(_param,_,type(basic,_primitiveType,0),_),
    add(newClassT(_new,_return,_encl,_constructor, [_expr],'null','null','null')),
    add(identT(_ident,_new,_encl,_classname, _class)), % TODO: eigentlich ein full qualified name
    add(returnT(_return,_parent,_encl,_new)).


boxing_class(int, _class):-
    packageT(_pckg, 'java.lang'),
    classDefT(_class,_pckg,'Integer',_).
    
boxing_class(double, _class):-
    packageT(_pckg, 'java.lang'),
    classDefT(_class,_pckg,'Double',_).
boxing_class(float, _class):-
    packageT(_pckg, 'java.lang'),
    classDefT(_class,_pckg,'Float',_).
boxing_class(char, _class):-
    packageT(_pckg, 'java.lang'),
    classDefT(_class,_pckg,'Character',_).
boxing_class(byte, _class):-
    packageT(_pckg, 'java.lang'),
    classDefT(_class,_pckg,'Byte',_).
boxing_class(short, _class):-
    packageT(_pckg, 'java.lang'),
    classDefT(_class,_pckg,'Short',_).
boxing_class(long, _class):-
    packageT(_pckg, 'java.lang'),
    classDefT(_class,_pckg,'Long',_).
boxing_class(boolean, _class):-
    packageT(_pckg, 'java.lang'),
    classDefT(_class,_pckg,'Boolean',_).





add_proceed_call_idents(_,_,_,_,[],[],_,[]).
add_proceed_call_idents(_pc,_call,_enclMethod, _adviceArgs, [_param|_params], [_forwParam|_forwParams], _proceedArgs,[_proceedArg|_Args]):-
    getCompareElement(_pc,_param,_forwParam, _compare),
    findProceedArg(_pc,_compare,_adviceArgs, _proceedArgs,_proceedArg),
%    getForwParam(_pc, _adviceArg, _compare,_),
    !,
    add_proceed_call_idents(_pc,_call,_enclMethod, _adviceArgs, _params,_forwParams, _proceedArgs,_Args).

add_proceed_call_idents(_pc,_call,_enclMethod, _adviceArgs, [_param|_params], [_forwParam|_forwParams], _proceedArgs,[_Arg|_Args]):-
    new_id(_Arg),
    !,
    getCompareElement(_pc,_param,_forwParam, _ref),
    getRefIdentName(_ref,_name),
    add(identT(_Arg, _call, _enclMethod, _name, _ref)),
    add_proceed_call_idents(_pc,_call,_enclMethod, _adviceArgs, _params,_forwParams, _proceedArgs,_Args).


getRefIdentName(_ref,_name):-
    localDefT(_ref, _, _, _, _name, _).
getRefIdentName(_ref,_name):-
    paramDefT(_ref, _, _, _name).
getRefIdentName(_ref,'this'):-
    classDefT(_ref, _, _, _).


getCompareElement(_method,_param, _, _class):-
     method(_method, _class, _, _, _, _, _),
     forwards(_, _forwMethod, _, _method),
     method(_forwMethod, _, _, _params, _, _, _),
    (
        _params = [_param|_];
        _params = [_|[_param|_]]
    ),
    !.

getCompareElement(_method,_, _forwParam,_forwParam):-
    method(_method, _, _, _, _, _, _),
    !.

getCompareElement(_pc,_param,_, _param).


findProceedArg(_pc,_compare,[_adviceArg|_adviceArgs], [_proceedArg|_proceedArgs],_proceedArg) :-
    getForwParam(_pc, _adviceArg, _compare,_),
    !.

findProceedArg(_pc,_compare,[_|_adviceArgs], [_|_proceedArgs],_proceedArg) :-
    findProceedArg(_pc,_compare,_adviceArgs, _proceedArgs,_proceedArg).


createForwMethParams(_enclClass,_forwMethod,_origReceiver, _args,[_InstanceVarDef|[_ReceiverVarDef|_Params]]):-
    createThisInstanceParam(_enclClass, _forwMethod, _InstanceVarDef),
    createReceiverParam(_enclClass, _forwMethod, _origReceiver, _ReceiverVarDef),
    createForwParams(_forwMethod, _args, _Params).

createThisInstanceParam(_enclClass,_forwMethod,_InstanceVarDef):-
    new_id(_InstanceVarDef),
    add(paramDefT(_InstanceVarDef, _forwMethod, type(class,_enclClass,0), '_this')). %neu


createReceiverParam(_enclClass, _forwMethod, 'null',_ReceiverVarDef):-
    new_id(_ReceiverVarDef),
    add(paramDefT(_ReceiverVarDef,  _forwMethod, type(class,_enclClass,0), '_target')),
    !.

createReceiverParam(_enclClass, _forwMethod, _origReceiver,_ReceiverVarDef):-
    new_id(_ReceiverVarDef),
    getType(_origReceiver, _type),
    add(paramDefT(_ReceiverVarDef,  _forwMethod, _type, '_target')).

createForwParams(_forwMethod, _args, _Params) :-
    createForwParams(_forwMethod, _args, _Params,0).

createForwParams(_, [],[],_).
createForwParams(_forwMethod, [_arg|_args], [_Param | _Params],_counter) :-
    createForwParam(_forwMethod,_arg, _Param,_counter),
    plus(_counter, 1, _next),
    createForwParams(_forwMethod, _args, _Params,_next).

createForwParam(_forwMethod, _arg, _Param,_counter) :-
    getType(_arg,_type),
    new_id(_Param),
    appendNum('x', _counter, _name),
    add(paramDefT(_Param, _forwMethod, _type, _name)),
    !.

% DEBUG commented

getForwParam(_method, _adviceArg, _IdentRef,_IdentName):-
    % Ausnahme f�r den execution pointcut
    methodDefT(_method, _class, _, _params, _, _, _),
    !,
    forwards(_, _forwMethod, _, _method),
    methodDefT(_forwMethod, _, _, [_|[_|_origParams]], _, _, _),
    findParamExecution(_adviceArg,_class,_params,_origParams,_IdentName,_IdentRef).


getForwParam(_pc, _adviceArg, _forwParam,_forwParamName):-
    forwards(_forwCall, _forwMethod, _, _pc),
    methodDefT(_forwMethod, _, _, [_thisParam|[_targetParam|_params]], _, _, _),
    applyT(_forwCall, _,_, _, _,[_|[_|_args]],_),
    findParam(_adviceArg,_thisParam,_targetParam,_params,_args,_forwParam),
    paramDefT(_forwParam, _,_type, _forwParamName).

findParamExecution('_this',_class, _,_,'this',_class):- !.
findParamExecution('_target',_class,_,_,'this',_class):- !.
findParamExecution(_param,_class,_params,_origParams, _forwParamName,_Param):-
    _param \= '_this',
    _param \= '_target',
    findParam(_param,_,_,_params,_origParams,_Param),
    paramDefT(_Param, _, _type, _forwParamName).


findParam('_this',_ThisParam,_,_,_,_ThisParam):-
    !.
findParam('_target',_,_TargetParam,_,_,_TargetParam):-
    !.
findParam(_arg,_,_,[_Param|_params],[_arg1|_args],_Param):-
    _arg \= '_this',
    _arg \= '_target',
    _arg == _arg1,
    !.
findParam(_arg,_thisParam,_targetParam,[_param|_params],[_a|_args],_ForwParam):-
    !,
    findParam(_arg,_thisParam,_targetParam,_params,_args,_ForwParam).

/*
  pc_visible(?Encl,?Class, ?Package)

  Is true when a join point (pointcut) is visible
  */

pc_visible(_encl,_,_):-
    enclClass(_encl,_enclClass),
    modifierT(_enclClass,'aspect'),
    !,
    fail.

pc_visible(_encl,_,_):-
    not(visibility(_encl,_,_)),
    !.

pc_visible(_encl,_,_):-
    visibility(_encl,'hidden',_),
    !,
    fail.

pc_visible(_encl,_,_pckg):-
    visibility(_encl,'package',_pckg),
    !.
    
pc_visible(_encl,_currentAspectClass,_):-
    visibility(_encl,'protected',_aspectClass),
    !,
    subtype(_currentAspectClass,_aspectClass).

pc_visible(_encl,_aspectClass,_):-
    visibility(_encl,'private',_aspectClass),
    !.


set_visibility(_pc, _type, _ref):-
    forwards(_,_forwMethod,_,_pc),
    !,
    add(visibility(_forwMethod, _type, _ref)).

set_visibility(_var, _type, _ref):-
    add(visibility(_var, _type, _ref)).


%    current_aspect(_aspectClass,_).

%TODO: erzeuge Argument-Listen: TESTEN

create_ref_idents(_pc, _apply, _encl, [], []).

create_ref_idents(_pc, _apply, _encl, [_param_id|_rest], [_ident_id | _rest_ids]) :-
    paramDefT(_param_id,_,_, _name),
    !,
    add(identT(_ident_id, _apply, _encl, _name, _param_id)),
    create_ref_idents(_pc, _apply, _encl, _rest, _restids).

% leere Parameter oder Argumentliste
create_ref_idents(_pc, _apply, _encl, [[]|_rest], [[] | _rest_ids]) :-
    !,
    create_ref_idents(_pc, _apply, _encl, _rest, _restids).

create_ref_idents(_pc, _apply, _encl, [_fn |_rest], [GetField | _rest_ids]) :-
    not(tree(_fn,_,_)),
    encl_class(_encl,_encl_class),
    resolve_field(_fn,_encl_class,_field),
    add(getFieldT(GetField, _apply, _encl, 'null',_fn, _field)),
    create_ref_idents(_pc, _apply, _encl, _rest, _restids).

create_ref_idents(_pc, _apply, _encl, [_arg_id|_rest], [GetField | _rest_ids]) :-
    add_advice_param_ref(_pc, _arg_id,GetField,_apply, _encl),
    create_ref_idents(_pc, _apply, _encl, _rest, _restids).

create_ref_idents(_pc, _apply, _encl, [[_h |_t] | _rest], [ _param_ids | _rest_ids]) :-
    !,
    create_ref_idents(_pc, _apply, _encl, [_h|_t], _param_ids),
    create_ref_idents(_pc, _apply, _encl, _rest, _restids).



%%%%%%%%%%%%%%%%%

% leere Parameter oder Argumentliste
extract_types(_encl_class, [], []).
extract_types(_encl_class, [[]|_rest], [[] | _Rest]) :-
    !,
    create_ref_idents(_encl, _rest, _Rest).

extract_types(_encl, [_fn|_rest], [_Type | _Rest]) :-
    not(tree(_fn,_,_)),
    !,
    encl_class(_encl,_encl_class),
    resolve_field(_fn,_encl_class,_field),
    fieldDefT(_field,_,_Type, _,_),
    create_ref_idents(_encl, _rest, _Rest).

extract_types(_encl, [_arg|_rest], [_Type | _Rest]) :-
    get_type(_arg,_Type),
    create_ref_idents(_encl, _rest, _Rest).

extract_types(_encl, [[_h |_t] | _rest], _Rest) :-

    !,
    create_ref_idents(_encl, [_h | _t], _rest_1),
    create_ref_idents(_encl, _rest, _rest2),
    append(_rest_1,_rest_2, _Rest).
    

constructor(_constructor,_class,_params):-
    methodDefT(_constructor,_class,'<init>', _paramsConstructor,_,[],_),
    matchParams(_params, _paramsConstructor).


/*************** cond    ************/
cond(subtype_name(_sub, _super)).
subtype_name(_sub, _sub).
subtype_name(_sub, _super) :-
    java_fq(extendsT(_sub,_super)).
subtype_name(_sub, _super) :-
    java_fq(implementsT(_sub,_super)).
subtype_name(_sub, _super) :-
    java_fq(extendsT(_sub,_subsuper)),
    subtype_name(_subsuper, _super).
subtype_name(_sub, _super) :-
    java_fq(implementsT(_sub,_subsuper)),
    subtype_name(_subsuper, _super).

cond(types(_ElementList,_TypeList)).

/**
 * types(?ElementList,?TypeList).
 * 
 * uses full qualified type names (java_fq API).
 */
 
types([],[]).
types([Element|ElementTail],[Type|TypeTail]):-
	getType_fq(Element,Type),
	types(ElementTail,TypeTail).

/*************** actions ************/

action(addArgList(FnArgs, PcArgs, Idents, Parent, ForwMethod)):-
	addArgList(FnArgs, PcArgs, Idents, Parent, ForwMethod).
	
action(addArg(FnArg,PcArgs,Ident,Parent,ForwMethod)):-
	addArg(FnArg,PcArgs,Ident,Parent,ForwMethod).	    
/**
 * addArgList(FnArg, PcArgs, IdList, Parent, Encl)
 */
addArgList([], _, [], _,_).
addArgList([FnArg|FnArgs], PcArgs, [Ident|Idents], Parent, ForwMethod) :-
    addArg(FnArg,PcArgs,Ident,Parent,ForwMethod),
    addArgList(FnArgs, PcArgs, Idents, Parent, ForwMethod).

/**
 * addArg(FnArg, PcArgs, Id, Parent, Encl)
 */    
 
addArg(FnArg,PcArgs,Ident,Parent,ForwMethod):-
    methodDefT(ForwMethod,_,_,[_This,_Target|ArgParams],_,_,_),
    lookupForwParameter(ForwMethod, FnArg,PcArgs,ArgParams,Param, Name),
    add(identT(Ident,Parent,ForwMethod,Name,Param)).

lookupForwParameter(Arg,_,[],[],_, _):-
    format('forwarding parameter lookup failed: ~a~n',[Arg]).
lookupForwParameter(_,FnArg,[FnArg|_],[Param|_],Param, Name):-
    paramDefT(Param,_,_,Name).
lookupForwParameter(ForwMethod,FnArg,[_|PcArgs],[_|ArgParams],Param, Name):-
    lookupForwParameter(ForwMethod,FnArg,PcArgs,ArgParams,Param, Name).
   
   
/**
 * action(showError(+Kind, +ID,+Msg))
 */
 
action(showError(Kind,ID,Msg)):-
    showError(Kind,ID,Msg).

/**
 * showError(+Kind, +ID,+Msg)
 */
showError(_,ID,Msg):-    
	var(ID),
	write('ID NOT BOUND: '),
	write(Msg),
	flush_output.

showError(Kind,ID,Msg):-    
    enclMethod(ID,Meth),
    method(Meth,Class,Name,_,_,_,_),
    fullQualifiedName(Class,Fqn),
    format('~a in method ~a.~a  ',[Kind,Fqn,Name]),
    sourceLocation(ID, File,Start,_),
    format('(~a:~a)~n~n~a~n', [File,Start,Msg]),
    gen_tree(ID),
    flush_output,

    /**
    * Added Dec 20, 2004 to store all errors/warnings
    * and move it to the Eclipse Problems View. (AL)
    */
    assert(isErrorWarningMessage(Kind, ID, Msg)).
    

/**
 * action(addParamList(+Params, +Ids,+Parent))
 */

action(addParamList(Params, Ids,Parent)) :-
    addParamList(Params, Ids,Parent).
    
/**
 * addParamList(+Params, +Ids,+Parent)
 */

addParamList([],[],_).
addParamList([Param|Params], [Id|Ids],Parent) :-
    paramDefT(Param,_,Type,Name),
    add(paramDefT(Id,Parent,Type,Name)),
    addParamList(Params, Ids,Parent).    
    
/**
 * action(addParamReferenceList(+Refs, +Params, +Parent,+Encl))
 *
 * Adds a list of parameter accesses.
 * Parent is the parent id and Encl the 
 * enclosing id of the created identT/5 facts.
 */
    
action(addParamReferenceList(Refs, Params, Parent,Encl)) :-
	addParamReferenceList(Refs, Params, Parent,Encl).
	
addParamReferenceList([], [], _Parent,_Encl).

addParamReferenceList([Ref|Refs], [Param|Params], Parent,Encl) :-
    paramDefT(Param,_,_,Name),
    add(identT(Ref,Parent,Encl,Name,Param)),
	addParamReferenceList(Refs, Params, Parent,Encl).
    
    
action(add_to_class_fq(Class,Member)):-
    add_to_class_fq(Class,Member).

%add_to_class_fq(_class, _) :- not(classDefT(_class, _, _, _)), !.
/*
  add_to_class_fq(+Class,+Member|+MemberList)
  
  Adds Member(s) to the class, if the Member is not already in the 
  member list.
  Fails if Class or Member is not bound and if Class is not a
  of type classDefT.
*/

add_to_class_fq(_, []):- !.
add_to_class_fq(Class, [Member|Rest]) :-
    add_to_class_fq(Class,Member),
    add_to_class_fq(Class,Rest).

add_to_class_fq(_class, _id) :-
    nonvar(_class),
    nonvar(_id),
    java_fq(classDefT(_class, _, _, _members)),
    member(_id, _members),
    !.
add_to_class_fq(_class, _id) :-
    nonvar(_class),
    nonvar(_id),
    java_fq(classDefT(_class, _p,_n,_members)),
    delete(java_fq(classDefT(_class, _p,_n,_members))),
    append(_members, [_id], _newMembers),
    add(java_fq(classDefT(_class, _p, _n, _newMembers))).
 
 
test(add_to_class_fq) :- fail.    
/**
 * apply_aj_cts.
 * 
 * debugging predicate
 * applies all cts in the aj_ct_list 
 * fact in the given order.
 */
apply_aj_cts :-
    aj_ct_list(A),
    apply_ctlist(A).