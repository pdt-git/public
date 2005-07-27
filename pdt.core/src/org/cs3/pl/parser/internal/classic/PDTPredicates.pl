?- dynamic dynamic/1. ?- multifile test/1.
?- aha(test).
?- aha(test/ 1).
?- aha(test/_A/b).

atom_concat(_1,_2,_3,_4) :-
    atom_concat(_1,_tmp,_4),
    atom_concat(_2,_3,_tmp).

test(atom_concat4):-
    atom_concat(a,b,c,abc),
    atom_concat(a,_1,c,abc),
    _1 == b,
    atom_concat(a,b,_c,abc),
    _c == c.

atom_concat(_1,_2,_3,_4,_5) :-
    atom_concat(_1,_2,_tmp,_5),
    atom_concat(_3,_4,_tmp).

test(atom_concat5):-
    atom_concat(a,b,c,d,abcd),
    atom_concat(a,_1,c,d,abcd),
    _1 == b,
    atom_concat(a,b,_c,d,abcd),
    _c == c.

atom_concat(_1,_2,_3,_4,_5,_6) :-
    atom_concat(_1,_2,_3,_tmp,_6),
    atom_concat(_4,_5,_tmp).

atom_concat(_1,_2,_3,_4,_5,_6,_7) :-
    atom_concat(_1,_2,_3,_4,_tmp,_7),
    atom_concat(_5,_6, _tmp).

atom_concat(_1,_2,_3,_4,_5,_6,_7,_8) :-
    atom_concat(_1,_2,_3,_4,_5,_tmp,_8),
    atom_concat(_6,_7, _tmp).

atom_concat(_1,_2,_3,_4,_5,_6,_7,_8,_9) :-
    atom_concat(_1,_2,_3,_4,_5,_6,_tmp,_9),
    atom_concat(_7,_8, _tmp).


test(atom_concat7):-
    atom_concat(1,2,3,4,5,6,123456).

get_file_pos(_pred,_arity, _file,_pos,_dynamic, _multifile) :-
    findall(_e, explain(_pred,_e),[_|_list]),
    member(_info,_list),
    atom_concat(_basic_info,'\n        ',_filepos,_info),
%    print(_basic_info),
    atom_concat(_module,':',_pred,'/',_arity,' is a ',_dynMul,'predicate defined in',_basic_info),
    
    (atom_concat('dynamic',_,_dynMul) ->
        _dynamic = 1;
        _dynamic = 0),
    (atom_concat(_,'multifile ',_dynMul) ->
        _multifile = 1;
        _multifile = 0),
    atom_concat(_file_we,'.pl:',_pos,_filepos),
    atom_concat(_file_we,'.pl',_file).




% +,-,-
get_pred(_file, _name,_arity,_pos,_dyn,_mul) :-
    source_file(_pred, _file),
    functor(_pred,_name,_arity),
%    !,
    dynamic(test),
    1 mod 2,
    mod(1),
    get_file_pos(_name,_arity, _file,_pos,_dyn,_mul).
    
