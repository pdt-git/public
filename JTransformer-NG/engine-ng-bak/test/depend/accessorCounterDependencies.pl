% Author:
% Date: 23.09.02

:- ['../../../ct/counterCT.pl'].
:- ['../../../ct/lava/accessorCT.pl'].

test('accessorCounterDependencies#1') :- findall(_order, topo_sort([aiget, acget, icounter, ccounter], _order), [[icounter,aiget,acget,ccounter]]).


