% Author:
% Date: 07.10.02
/*
ct(mixedHighLowDepCT1, [
   field(_field, _, _, _, _),
   method(_encl, _, foo, _, _, _, _),
   whileLoopT(_wid, _, _encl, _,_)
],[
   getField(_gfid, _wid, _encl, this, _field)
]).

ct(mixedHighLowDepCT2, [
   getField(_gfid, _wid, _encl, this, _field)
],[
]).

ct(mixedHighLowDepCT3, [
   method(_encl, _, m, _, _, _, _),
   getField(_gfid, _wid, _encl, this, _field)
],[
]).

ct(mixedHighLowDepCT4, [
   forLoopT(_wid, _, _encl, _,_,_,_),
   getField(_gfid, _wid, _encl, this, _field)
],[
]).

test('mixedHighLowDepCT') :- findall((A,B), (ct_same_prefix('mixedHighLowDepCT',A,B), depend(A,B,_)), [
(mixedHighLowDepCT2, mixedHighLowDepCT1)
]).

*/