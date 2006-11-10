:- multifile setUp/1.
:- multifile test/1.
:- multifile tearDown/1.

/*******************fullQualifiedName *********************/
setUp(fullQualifiedName_class_in_default_package) :- 
    add(classDefT(c1,null,'DummyClassX',[])),
    add(globalIds(c1,'DummyClassX')).
    add(ri_globalIds('DummyClassX',c1)).
test(fullQualifiedName_class_in_default_package):-
    fullQualifiedName(c1,FQN),
    assert_true((FQN = 'DummyClassX')).

tearDown(fullQualifiedName_class_in_default_package) :- 
    rollback.
    
setUp(fullQualifiedName_class_in_package) :- 
    add(classDefT(c2,p2,'DummyClassX',[])),
    add(globalIds(c2,'testpckg.DummyClassX')),
    add(ri_globalIds('testpckg.DummyClassX',c2)),
    add(packageT(p2,'testpckg')).    
test(fullQualifiedName_class_in_package):-
    fullQualifiedName(c2,FQN),
    assert_true((FQN = 'testpckg.DummyClassX')).
tearDown(fullQualifiedName_class_in_package) :- 
    rollback.

setUp(fullQualifiedName_anon_class) :- 
    add(classDefT(anon, newC, 'ANONYMOUS$1',[])),
    add(newClassT(newC, block, enclMeth, constr, [], typeExpr, anon, null)),
    add(methodDefT(enclMeth,enclClass,a,b,c,d,e)),
    add(classDefT(enclClass,null, 'EnclClass',[enclMeth])).
test(fullQualifiedName_anon_class):-
    fullQualifiedName(anon,FQNANON),
    assert_true((FQNANON = 'EnclClass.ANONYMOUS$1')),
    fullQualifiedName(ANON,'EnclClass.ANONYMOUS$1'),
    assert_true((ANON = anon)).
tearDown(fullQualifiedName_anon_class):-
    rollback.
    
