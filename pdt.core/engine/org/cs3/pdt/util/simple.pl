/**
 * Very simple predicates which should be include in 
 * the using modules. (Other suggestions???)
 *
 * Currently they are globally visible, thats why 
 * more than one definition is not allowed.
 */

append_num(_str, _num, _Strnum) :-
    int2string(_num, _num_s),
    stringAppend(_str, _num_s, _Strnum).
     
     
/********************************************
 ************* PEF independend **************
 ********************************************/
:- dynamic countvar/1.
:- multifile countvar/1.
countvar(0).

incCounter(_i) :-
    not(nonvar(_i)),
    countvar(_old),
    retractall(countvar(_)),
    plus(_old, 1, _i),
    assert(countvar(_i)).


counter(_i) :-
    nonvar(_i),
    !,
    retractall(countvar(_)),
    assert(countvar(_i)).

counter(_i) :-
    not(nonvar(_i)),
    !,
    countvar(_i).

/*
        count(+pred)
        
        Counts all binding for the predicate pred.
*/
        
count(_pred) :-
    findall(_pred, call(_pred), _list),
    length(_list, _len),
    writef("found %d results.\n",[_len]),
    fail.
    
/*
count_bag(_pred) :-
    bagof(_pred, call(_pred), _list),
    length(_list, _len),
    writef("found %d results.\n",[_len]),
    fail.
*/      

