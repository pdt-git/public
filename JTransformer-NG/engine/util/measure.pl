:- module(measure,[
    start/0,
    stop/1]).
    
:- dynamic lastTime/3.

/**
  start
 
  Starts the time measuring.
*/

start :-
    get_time(_t),
    convert_time(_t, _, _, _, _, _min, _sec, _msec),
    assert(lastTime(_min,_sec,_msec)).


/**
 stop(-Msg)
 
Stops the time measuring and binds
the resulting time difference to Msg.
*/
stop(_msg) :-
    get_time(_t),
    convert_time(_t, _, _, _, _, _min, _sec, _msec),
    lastTime(_min0,_sec0,_msec0),
    retractall(lastTime(_,_,_)),
    get_diff(0         , _msec0, _msec, _div_msec, _carryMsec, 1000),
    get_diff(_carryMsec, _sec0 , _sec , _div_sec ,_carrySec  , 60),
    get_diff(_carrySec , _min0 , _min , _div_min ,_, 0),
    writef('%w: %w min %w.%2r sec', [_msg, _div_min, _div_sec, _div_msec]).

get_diff(_carry, _start, _end, _Out,_carryOut, _add) :-
    plus(_end, _carry, _endPlus),
    plus(_start, _diff, _endPlus),
    ifNegAdd(_diff, _Out, _carryOut, _add).
    
/*
 ifNegAdd(+Val1, +Val2, -IsNeg, -Add)
 */
 
ifNegAdd(_val, _val, 0, _) :-
    _val >= 0.
ifNegAdd(_val, _Out, 1, _add) :-
    plus(_val, _add, _Out).
    