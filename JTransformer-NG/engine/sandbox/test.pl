% Author: Tobias
% Date: 18.05.2003

test(a).

abstraction(c1(_a,_b,_c)).

c1(_a,_b,_c):-
    test(_a,_b,_c).
    

clausetest :-
    abstraction(_a),
    clause(_a,_body),
    term_to_atom(_body,_ba),
    write(_ab).
