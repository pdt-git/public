
expand(_ct1,_ct2):-
    ct(_ct1, _c1, _t1),
    ct(_ct2, _c2, _t2),
    expandAbstractionsAndDNF(_c1, _c2, _t2, _c1e, _c2e, _t2e),
    term_to_atom(_c1,_c1a),
    term_to_atom(_c1e,_c1ea),
    term_to_atom(_c2,_c2a),
    term_to_atom(_c2e,_c2ea),
    term_to_atom(_t2,_t2a),
    term_to_atom(_t2e,_t2ea),
    format('cond: ~a~nexpanded cond: ~a~n',[_c1a,_c1ea]).
%    format('cond: ~a~nexpanded cond: ~a~n',[_c2a,_c2ea]),
%    format('cond: ~a~nexpanded cond: ~a~n',[_t2a,_t2ea]).

%expand( 'test/tests/basic/advice/BasicAspectAround_around0_ajc', 'test/tests/basic/advice/BasicAspectAround_after0_ajc').