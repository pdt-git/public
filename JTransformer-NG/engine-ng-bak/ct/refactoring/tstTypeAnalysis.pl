% Author: Daniel Speicher
% Date: 31.10.2003

:-['TypeAnalysis.pl'].         % Zu testendes Modul
:-['tstHierarchyWorldX.pl'].   % Testdaten


testSetUp('TypeAnalysis', _package, _ileft, _itop,
      _imiddle, _ibottom, _theclass) :-
  packageT(_package, 'org.joint.exintexp.tstHierarchy'),
  classDefT(_ileft,    _package, 'ILeft',    _),
  classDefT(_itop,     _package, 'ITop',     _),
  classDefT(_imiddle,  _package, 'IMiddle',  _),
  classDefT(_ibottom,  _package, 'IBottom',  _),
  classDefT(_theclass, _package, 'TheClass', _).


testIdToName(_id, _name) :- classDefT(_id, _, _name, _).


test('TypeAnalysis.realsubtype') :-
    testSetUp('TypeAnalysis', _package, _ileft, _itop, _imiddle,
        _ibottom, _theclass),
    findall(_subtypes,
        (
            member(_super, [_ileft, _itop, _imiddle, _ibottom, _theclass]),
            findall(_subtype, realsubtype(_subtype, _super), _subtypes)
        ),
        [
            [_ibottom, _theclass, _theclass],  % l    TODO: Doppelt gewünscht?
            [_imiddle, _ibottom, _theclass],   % t
            [_ibottom, _theclass],             % m
            [_theclass],                       % b
            []                                 % c
        ]
    ).


testOverridenMethodName(_subclass, _superclass, _methodname) :-
    methodDefT(_methodsub, _subclass, _, _, _, _, _),
    overrides(_methodsub, _methodsuper),
    methodDefT(_methodsuper, _superclass, _methodname, _, _, _, _).


test('TypeAnalysis.overrides'):-
    testSetUp('TypeAnalysis', _package, _ileft, _itop, _imiddle,
        _ibottom, _theclass),
    findall(_methodlist,
        (
            member(_super, [_ileft, _itop, _imiddle, _ibottom, _theclass]),
            member(_sub,   [_ileft, _itop, _imiddle, _ibottom, _theclass]),
            findall(_method,
                testOverridenMethodName(_sub, _super, _method),
            _methodlist)
        ),
        [
            [],       % l
            [],
            [],
            [lb, ltmb],
            [l, l, lt, lt, lm, lm, lb, lb, ltmb, ltmb], % TODO: Doppelt gewünscht?
            [],       % t
            [],
            [ltmb],
            [tb, ltmb],
            [lt, ltmb, t, tb],
            [],       % m
            [],
            [],
            [ltmb],
            [lm, ltmb, m, tb, l],
            [],       %b
            [],
            [],
            [],
            [b, lb, ltmb, tb],
            [],       %c
            [],
            [],
            [],
            []
        ]
    ).




test('TypeAnalysis.rootDefinition') :-
    testSetUp('TypeAnalysis', _package, _ileft, _itop, _imiddle,
        _ibottom, _theclass),
    findall((_methodname, _rootdefs),
        (
            methodDefT(_method, _theclass, _methodname, _, _, _, _),
            findall(_root,
                (rootDefinition(_method, _rootmethod),
                methodDefT(_rootmethod, _root, _, _, _, _, _)),
            _rootdefs)
        ),
        [
            ('<init>',   [_theclass]),
            (c,          [_theclass]),  % TODO: Doppelt gewünscht?
            (l,          [_ileft, _ileft]),
            (b,          [_ibottom]),
            (lt,         [_ileft, _ileft, _itop]),
            (lm,         [_ileft, _imiddle, _ileft]),
            (lb,         [_ileft, _ileft]),
            (ltmb,       [_ileft, _ileft, _itop]),
            (t,          [_itop]),
            (tb,         [_itop]),
            (m,          [_imiddle]),
            (tb,         [_imiddle]),   % wegen abweichender Signatur
            (l,          [_imiddle]),   % wegen abweichender Signatur
            ('<clinit>', [_theclass]),
            ('<clinit>', [_theclass])
        ]
    ).




test('TypeAnalysis') :-
    test('TypeAnalysis.overrides'),
    test('TypeAnalysis.realsubtype'),
    test('TypeAnalysis.rootDefinition').



