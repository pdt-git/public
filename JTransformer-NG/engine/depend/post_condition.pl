
test('postCondition#1') :- postCondition([a],[b],[a,b]).
test('postCondition#2') :- postCondition([],[a,b],[a,b]).
test('postCondition#3') :- postCondition([a,b],[],[a,b]).
test('postCondition#4') :- postCondition([a,b],[b],[a,b]).
test('postCondition#5') :- postCondition([a,b],[a],[a,b]).
test('postCondition#6') :- postCondition([a],[not(a)],[not(a)]).
test('postCondition#7') :- postCondition([a],[not(b)],[a, not(b)]).
test('postCondition#8') :- postCondition([a,b],[not(a)],[not(a),b]).
test('postCondition#9') :- postCondition([a,b],[not(b)],[a, not(b)]).
test('postCondition#10'):- postCondition([a,b],[not(b), not(a)],[not(a), not(b)]).

%test('postCondition#11'):- postCondition([class(A,B,C)],[not(class(D,E,F))],[not(class(D,E,F))]), A==D, B==E, C==F.
%test('postCondition#12'):- postCondition([class(A,B,C), method(M,A)],[not(class(D,E,F))],[class(A,B,C), method(M,A), not(class(D,E,F))]), A\==D, B==E, C==F.


/* uwe */

/**
* Compute PostCondition - uwe change: does not unify any more ( tests via == )
*/
postCondition(_pre, _act, _Post) :-
%    mark_shared(_pre),
%    mark_shared(_act),
    applyAction(_pre, _act, _pre2),
    append(_pre2, _act, _post),
    list_to_set_save(_post, _Post).
%    retractall(sharedVar(_)),
%    retractall(knownVar(_)).
applyAction([], _act, []).
applyAction([not(_h)|_t], _act, [_h|_T]) :-
    member_save(_h, _act),
    !,
    applyAction(_t, _act, _T).
applyAction([_h|_t], _act, [not(_h)|_T]) :-
    _h \= not(_),
    member_save(not(_h), _act),
    !,
    applyAction(_t, _act, _T).
applyAction([_h|_t], _act, [_h|_T]) :-
    applyAction(_t, _act, _T).
    
/*
shared_variables(_term, []) :-
    functor(_h, _f, _a),
    _a == 0,
    !.
shared_variables(_term) :-
    sh
*/


:- dynamic sharedVar/1.
:- dynamic knownVar/1.


mark_shared([]).
mark_shared([_h|_t]) :-
    mark_shared(1, _h),
    mark_shared(_t).

mark_shared(_i, not(_term)) :-
    !,
    mark_shared(_i, _term).
mark_shared(_i, _h) :-
    arg(_i, _h, _arg),
    term_to_atom(_arg, _var),
    knownVar(_var),
    !,
    assert(sharedVar(_var)),
    _next is _i+1,
    mark_shared(_next, _h).
mark_shared(_i, _h) :-
    arg(_i, _h, _arg),
    !,
    term_to_atom(_arg, _var),
    assert(knownVar(_var)),
    _next is _i+1,
    mark_shared(_next, _h).
mark_shared(_i, _h).

    

    


unify_non_shared(_i, not(_m), _h) :-
     !,
     unify_non_shared(_i, _m, _h).
unify_non_shared(_i, _m, not(_h)) :-
     !,
     unify_non_shared(_i, _m, _h).
unify_non_shared(_i, _m ,_h) :-
     arg(_i, _m, _arg),
     term_to_atom(_arg, _t),
     not(sharedVar(_t)),
     !,
     arg(_i, _h, _arg),
     _next is _i +1,
     unify_non_shared(_next, _m, _h).
unify_non_shared(_i, _m ,_h) :-
     arg(_i, _m, _arg),
     !,
     _next is _i +1,
     unify_non_shared(_next, _m, _h).
unify_non_shared(_i, _m ,_h).









/**
* Negate PreCondition/PostAction
*/
negate(_cond, _PostAct) :-
        negate(_cond, [], _PostAct).
negate([], _postAct, _postAct).
negate([not(_elem)|_tail], _postAct, _PostAct) :-
        negate(_tail, [_elem|_postAct], _PostAct).
negate([_elem|_tail], _postAct, _PostAct) :-
        not(equals(_elem, not(_x))),
        negate(_tail, [not(_elem)|_postAct], _PostAct).
        
        
