% Author: Daniel Speicher
% Date: 02.11.2003

:- ['tstListBagWorld.pl'].
:- ['TypeConstraints.pl'].
:- ['DeclarationElements.pl'].

% ==========================================================================
%  Ableitung der Aussagen in Fig. 3 auf Seite 8.
%  Hier sortiert nach den angewendeten Regeln zur Ableitung von
%  Type-Constraints.
%  ALLE dort angegebenen Type-Constraints werden auf diese Weise ermittelt.
% ==========================================================================

testTceqName(_declelname1, _declelname2, _rule) :-
    tceq(_declel1, _decel2, _, _rule),
    declarationElementName(_declel1, _declelname1),
    declarationElementName(_decel2,  _declelname2).

testTcleqName(_subname, _supername, _rule) :-
    tcleq(_subdecl, _superdecl, _, _rule),
    declarationElementName(_subdecl,   _subname),
    declarationElementName(_superdecl, _supername).

testTcleqsomeName(_subname, _supernames, _rule) :-
    tcleqsome(_subdecl, _superdecls, _, _rule),
    declarationElementName(_subdecl,   _subname),
    findall(_supername,
        (member(_superdecl, _superdecls),
        declarationElementName(_superdecl, _supername)), _supernames).


test('TypeConstraints.tceq(08)') :-
  findall((_declelname1, _declelname2),
    testTceqName(_declelname1, _declelname2, '(08)'),
    [
        (element, e),
        (l0, l1)
    ]
  ).

test('TypeConstraints.tceq(09)') :-
  findall((_declelname1, _declelname2),
    testTceqName(_declelname1, _declelname2, '(09)'),
    [
        ('List.iterator', 'Bag.iterator'),
        ('List.add',      'Bag.add'),
        ('List.addAll',   'Bag.addAll')
    ]
  ).

test('TypeConstraints.tcleq(01)') :-
  findall((_subname, _supername),
    testTcleqName(_subname, _supername, '(01)'),
    [
        (newObjects,          elems),
        ('List',              'tmp$0'),
        (l3,                  l2),
        ('Iterator',          'tmp$0'),
        ('List.iterator',     i),
        ('Client.createList', l4),
        ('List.add',          l7),
        ('List.iterator',     iter)
    ]
  ).

test('TypeConstraints.tcleq(03/14)') :-
  findall((_subname, _supername),
    testTcleqName(_subname, _supername, '(03/14)'),
    [
        (l4, l5),
        (l4, l6),
        (l4, l8),
        (l4, l9),
        (l7, l1)
    ]
  ).

test('TypeConstraints.tcleq(06)') :-
  findall((_subname, _supername),
    testTcleqName(_subname, _supername, '(06)'),
    [
        ('tmp$0', 'List'),
        (l2,      'List'),
        (l2,      'List'),
        ('tmp$0', 'Iterator')
    ]
  ).

test('TypeConstraints.tcleq(07)') :-
  findall((_subname, _supername), testTcleqName(_subname, _supername, '(07)'),
    [
        ('Iterator', 'List.iterator'),
        ('List',     'List.add'),
        ('List',     'List.addAll'),
        ('List',     'Client.createList')
    ]
  ).

test('TypeConstraints.tcleq(12)') :-
  findall((_subname, _supername), testTcleqName(_subname, _supername, '(12)'),
    [
        ('List', l3)
    ]
  ).

test('TypeConstraints.tcleqsome(04/15)') :-
  findall((_subname, _supernames),
    testTcleqsomeName(_subname, _supernames, '(04/15)'),
    [
        (l1,         ['Bag']),
        (i,          []),
        ('List',     ['Bag']),
        (i,          []),
        (e2,         []),
        ('Client',   ['Client']),
        ('Client',   ['Client']),
        ('Client',   ['Client']),
        ('Client',   ['Client']),
        ('Client',   ['Client']),
        ('List.add', ['Bag']),
        (l5,         ['Bag']),
        ('List.add', ['Bag']),
        ('List',     ['Bag']),
        (l6,         ['Bag']),
        (l8,         ['List']),
        (l9,         ['Bag']),
        (iter,       []),
        (iter,       [])
    ]
  ).

test('TypeConstraints') :-
    test('TypeConstraints.tceq(08)'),
    test('TypeConstraints.tceq(09)'),
    test('TypeConstraints.tcleq(01)'),
    test('TypeConstraints.tcleq(03/14)'),
    test('TypeConstraints.tcleq(06)'),
    test('TypeConstraints.tcleq(07)'),
    test('TypeConstraints.tcleq(12)'),
    test('TypeConstraints.tcleqsome(04/15)').
    

