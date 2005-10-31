test(java_fq1):-
   assert_true('full instantiated fieldDefT',(
			java_fq(methodDefT(_,Class,Name,_,Type,[Ex1,Ex2|_],_)),
			 Class= 'java.lang.Class',
			java_fq(classDefT(Class,P,_,_)),packageT(P,'java.lang'),
			Name = newInstance,
			Ex1 = 'java.lang.InstantiationException',
			Ex2 = 'java.lang.IllegalAccessException',
			 Type = 'java.lang.Object'
			 )).

test(java_fq2):-
   assert_true('full instantiated fieldDefT',(
			java_fq(methodDefT(_,Class,Name,_,Type,['java.lang.InstantiationException',Ex2|_],_)),
			 Class= 'java.lang.Class',
			java_fq(classDefT(Class,P,_,_)),packageT(P,'java.lang'),
			Name = newInstance,
			Ex2 = 'java.lang.IllegalAccessException',
			 Type = 'java.lang.Object'
			 )).
			 
test(java_fq3):-
   assert_true('check for field java.langClassLoader.classAssertionStatus and try full instantiated java_fq fieldDefT',(
   	class(C,P,'ClassLoader'),
   	packageT(P,'java.lang'),
   	fieldDefT(F, C, _, classAssertionStatus, null),
    java_fq(fieldDefT(F, 'java.lang.ClassLoader', 'java.util.Map', classAssertionStatus, null)))),
    
    assert_true('full instantiated fieldDefT except id',
      java_fq(fieldDefT(_, 'java.lang.ClassLoader', 'java.util.Map', classAssertionStatus, null))),
      
    assert_true('',
	   java_fq(fieldDefT(_,_,'java.lang.Class',_,_))).
       	

lookupJavaLangString(JavaLangString):-
     assert_true('look for class java.lang.String',
     (class(JavaLangString,JavaLang,'String'),
      packageT(JavaLang,'java.lang'))).
    
    
test(java_fq_type_with_brackets1):-
    assert_true(type_with_brackets(int,2,IntBrackets)),
    assert_true(('int[][]' ==IntBrackets)).
    
test(java_fq_type_with_brackets2):-
    assert_true(type_with_brackets(Type,Arity,'int[]')),
    assert_true(('int' == Type)),
    assert_true((1 == Arity)).
    
test(java_fq_java_fq_to_pef):-
    assert_true((
    	class(Class,Package,'Object'),
    	packageT(Package,'java.lang'),
    	methodDefT(A,Class,hashCode,[],type(basic,int,0),[],null),
    	java_fq_to_pef(
    	   methodDefT(A,'java.lang.Object',hashCode,[],int, [],null),
    	   Term))),
    assert_bound(Term),
    assert_true((Term = methodDefT(A,Class,hashCode,[],type(basic,int,0), [],null))).
    
test(java_fq_map_type_term):-
    assert_true(
       map_type_term(T, 'java.util.Map')),
    assert_true(T = type(class,CId,0)),
    assert_true((class(CId,P,'Map'),packageT(P,'java.util'))).