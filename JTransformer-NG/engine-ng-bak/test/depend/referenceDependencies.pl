% Author:
% Date: 03.09.2002

ct(referenceDepCT1, (
    class(_cid, _, 'Test'),
    method(_mid, _cid, _name, _, _, _, _)
),(true)).
ct(referenceDepCT2, (
    class(_cid, _, 'Test'),
        field(657678,_cid,_,_,_)
),(
    class(1234, 343, 'MyClass'),
        method(555, 1234, myMeth, [], type(basic, void , 0), [], [777]),
            blockT(777, 555, 555, [888]),
                getField(888, 777, 555, null, 657678)
)).
ct(referenceDepCT3, (
    class(_cid, _,'MyClass')
),(
    method(555, _cid, myMeth, [], type(basic, void , 0), [], null)
)).
ct(referenceDepCT4, (
    class(_cid, _, 'Test')
),(
    method(555, _cid, myMeth, [], type(basic, void , 0), [], null)
)).
ct(referenceDepCT5, (
    method(_mid, _, anotherMethod, _, _, _, _),
    getField(_, _, _mid, _, _)
),(true)).
ct(referenceDepCT6, (
    class(_cid, _, 'Test', _),
    method(_mid, _cid, myMeth, _, _, _, _),
    getField(_, _, _mid, _, _)
),(true)).

ct(referenceDepCT7, (
    getField(_id, _pid, _mid, _, _)
),(true)).

test('referenceDependencies') :- findall((A,B), (ct_same_prefix('referenceDepCT',A,B), depend(A,B,_)), [
(referenceDepCT1, referenceDepCT4),
(referenceDepCT3, referenceDepCT2),
(referenceDepCT6, referenceDepCT4),
(referenceDepCT7, referenceDepCT2)
]).

