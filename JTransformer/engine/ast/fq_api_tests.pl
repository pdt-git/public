
test(atom_to_type_term_):-
	lookupJavaLangString(JavaLangString),
	assert_true(atom_to_type_term_('java.lang.String[]',Type)),
	assert_true(Type = type(class,JavaLangStringId,Arity)),
	assert_true('JavaLangStringId should equal the ID of the class java.lang.String',JavaLangStringId = JavaLangString),
	assert_true('Arity should be 1',Arity = 1),
	assert_true('',atom_to_type_term_('int[][]',type(basic,Int,IntArity))),
	assert_true('expect the value int',Int = int),
	assert_true('Arity should be 2.',IntArity = 2),
	assert_true('simple basic type double',atom_to_type_term_('double',type(basic,double,0))),
	assert_true('simple java.lang.String type',atom_to_type_term_('java.lang.String',type(class,JavaLangString,0))).	

test(type_term_to_atom_):-
	lookupJavaLangString(JavaLangString),
    assert_true(
       	type_term_to_atom_(type(class,JavaLangString,1),FQN)),
    assert_true(FQN = 'java.lang.String[]'),
    assert_true(
       	type_term_to_atom_(type(basic,int,0),Int)),
    assert_true(Int = 'int').
       	

test(type_term_to_atom):-
	lookupJavaLangString(JavaLangString),
    assert_true(
       	type_term_to_atom(type(class,JavaLangString,1),FQN)),
    assert_true(FQN = 'java.lang.String[]'),
    assert_true(
       	type_term_to_atom(TypeTerm,int)),
    assert_true(type(basic,int,0) = TypeTerm).
       	
       	

lookupJavaLangString(JavaLangString):-
     assert_true('look for class java.lang.String',
     (class(JavaLangString,JavaLang,'String'),
      packageT(JavaLang,'java.lang'))).
    