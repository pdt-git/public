% Author: Daniel Speicher
% Date: 03.11.2003

:- ['tstListBagWorld.pl'].
:- ['TypeConstraints.pl'].
:- ['DeclarationElements.pl'].


testSetUp('DeclarationElements', _package, _class, _interface) :-
  packageT(_package, 'org.joint.exintexp'),
  classDefT(_class,      _package, 'List', _),
  classDefT(_interface,  _package, 'Bag',  _).


% Test: Ermittle alle Deklarationen, die ein Element als von der Klasse 'List'
% deklarieren.
% (Stimmt mit der Liste auf Seite 4 unten, Abschnitt 2.1, überein.)
test('DeclarationElements.declarationElement') :-
  testSetUp('DeclarationElements', _package, _class, _interface),
  findall(_declname,
    (
        declarationElement(_decl, _class), declarationElementName(_decl, _declname)
    ),
    [
        'l0', 'tmp$0', 'l1', 'l2', 'l3', 'l4', 'l5', 'l6', 'l7', 'l8', 'l9',
        'Bag.add', 'Bag.addAll', 'List.add', 'List.addAll', 'Client.createList'
    ]
    ).


%% Test: Ermittlere alle Deklarationen im Beispiel ListBag.java,
%% bei denen statt der Klasse 'List' nicht das Interface 'Bag'
%% verwendet werden kann.
%% (Stimmt mit der Liste auf Seite 9, unteres Drittel, überein.)
test('DeclarationElements.declarationNonUpdateable') :-
  testSetUp('DeclarationElements', _package, _class, _interface),
  findall(_declelname,
  (
    declarationNonUpdateable(_declel, _class, _interface),
    declarationElementName(_declel, _declelname)
  ),
  ['l8', 'tmp$0', 'l2', 'l2', 'l3', 'l3', 'l4', 'Client.createList']).


%% Test: Wir oben. Mit zusätzlicher Angabe der Gründe für den Ausschluss.
%% Die Großbuchstaben beziehen sich auf die Auschlussregel, die
%% folgenden Zahlen geben die Stelle im Programmcode an gefolgt von der
%% Regel, die daraus den Type-Constraint folgert.
test('DeclarationElements.declarationNonUpdateableExt') :-
  testSetUp('DeclarationElements', _package, _class, _interface),
  findall((_declelname, _reasoning),
    (
        declarationNonUpdateable(_declel, _class, _interface, _, _reasoning),
        declarationElementName(_declel, _declelname)
    ),
    [
        ('l8',      [('A', 100294, '(04/15)')]),
        ('tmp$0',   [('B', 100079, '(06)')]),
        ('l2',      [('B', 100192, '(06)')]),
        ('l2',      [('B', 100202, '(06)')]),
        ('l3',      [('B', 100192, '(06)'), ('C', 100183, '(01)')]),
        ('l3',      [('B', 100202, '(06)'), ('C', 100183, '(01)')]),
        ('l4',      [('A', 100294, '(04/15)'), ('C', 100251, '(03/14)')]),
        ('Client.createList',
                    [('A', 100294, '(04/15)'), ('C', 100251, '(03/14)'),
                     ('C', 100238, '(01)')])
    ]
  ).


test('DeclarationElements') :-
    test('DeclarationElements.declarationElement'),
    test('DeclarationElements.declarationNonUpdateable'),
    test('DeclarationElements.declarationNonUpdateableExt').


