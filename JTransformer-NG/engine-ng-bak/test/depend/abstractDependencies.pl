% Author:
% Date: 03.09.2002

ct(abstractDepCT1, (
    class(_cid, _, 'Test'),
    method(_mid, _cid, _name, _, _, _, _)
),(true)).

ct(abstractDepCT3, (
    class(_cid, _, 'MyClass')
),(true)).

ct(abstractDepCT5, (
    method(_mid, _, anotherMethod, _, _, _, _),
    getField(_, _, _mid, _, _)
),(true)).

ct(abstractDepCT6, (
    class(_cid, _, 'Test'),
    method(_mid, _cid, myMeth, _, _, _, _),
    getField(_, _, _mid, _, _)
),(true)).

ct(abstractDepCT7, (
    getField(_id, _pid, _mid, _, _)
),(true)).

ct(abstractDepCT8, (
    class(_cid, _pid, 'Test')
),(
    delete(class(_cid, _pid, 'Test'))
)).
ct(abstractDepCT9, (
    class(_cid, _pid, _name, _defs),
    _name \= 'Test'
),(
    not(class(_cid, _pid, _name, _defs))
)).
ct(abstractDepCT10, (
    method(ID, PID, myMeth, _a, _b, _c, _d)
),(
    not(method(ID, PID, myMeth, _a, _b, _c, _d))
)).

test('abstractDependencies#1') :- findall((A,B), (ct_same_prefix('abstractDepCT',A,B), negDepend(A,B,_) ), [
(abstractDepCT1, abstractDepCT8),
(abstractDepCT1, abstractDepCT10),
(abstractDepCT3, abstractDepCT9),
(abstractDepCT5, abstractDepCT8),
(abstractDepCT5, abstractDepCT9),
(abstractDepCT6, abstractDepCT8),
(abstractDepCT6, abstractDepCT10),
(abstractDepCT7, abstractDepCT8),
(abstractDepCT7, abstractDepCT9),
(abstractDepCT7, abstractDepCT10),
(abstractDepCT10, abstractDepCT8),
(abstractDepCT10, abstractDepCT9)
]).
test('abstractDependencies#2') :- findall((A,B), (ct_same_prefix('abstractDepCT',A,B), posDepend(A,B,_) ), [
]).


