% Author:
% Date: 20.09.02

ct(replaceDepCT0, (
    packageT(PID, 'old'),
    packageT(PID2, 'new'),
    class(ID, PID, 'Test')
),(
    replace(class(ID, PID2, 'MyClass'))
)).
ct(replaceDepCT3, (
    class(_, PID, _),
    packageT(PID, 'old')
),(true)).

ct(replaceDepCT4, (
    class(_, PID, _),
    packageT(PID, 'new')
),(true)).

/* macht mit neuem class/3 Predikat keinen Sinn mehr */
/*
ct(replaceDepCT1, (
    class(ID, PID, 'Hugo',_),
    compute(MYDefs)
%    field(FID,ID,xy,.....)
),(
    replace(class(ID, PID, 'Hugo', [MYDefs]))
)).
*/
ct(replaceDepCT2, (
    class(ID, _, _),
    method(_, ID, _, _, _, _, _)
),(true)).

test('replaceDependencies#1') :- findall((A,B), (ct_same_prefix('replaceDepCT',A,B), posDepend(A,B,_)), [
(replaceDepCT4, replaceDepCT0),
(replaceDepCT2, replaceDepCT1)
]).
test('replaceDependencies#2') :- findall((A,B), (ct_same_prefix('replaceDepCT',A,B), negDepend(A,B,_)), [
(replaceDepCT3, replaceDepCT0),
(replaceDepCT2, replaceDepCT1)
]).




