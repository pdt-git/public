test(java_fq1):-
   assert_true('full instantiated fieldDefT',(
			java_fq(methodDefT(_,Class,Name,_,Type,[Ex1,Ex2|_],_)),
			 Class= 'java.lang.Class',
			java_fq(classDefT(Class,P,_,_)),packageT(P,'java.lang'),
			Name = newInstance,
			Ex1 = 'java.lang.InstantiationException',
			Ex2 = 'java.lang.IllegalAccessException',
			 Type = type(class, 'java.lang.Object', 0)
			 )).

test(java_fq2):-
   assert_true('full instantiated fieldDefT',(
			java_fq(methodDefT(_,Class,Name,_,Type,['java.lang.InstantiationException',Ex2|_],_)),
			 Class= 'java.lang.Class',
			java_fq(classDefT(Class,P,_,_)),packageT(P,'java.lang'),
			Name = newInstance,
			Ex2 = 'java.lang.IllegalAccessException',
			 Type = type(class, 'java.lang.Object', 0)
			 )).
test(java_fq3):-
   assert_true('check for field java.langClassLoader.classAssertionStatus and try full instantiated java_fq fieldDefT',(
   	class(C,P,'ClassLoader'),
   	packageT(P,'java.lang'),
   	fieldDefT(F, C, _, classAssertionStatus, null),
    java_fq(fieldDefT(F, 'java.lang.ClassLoader', type(class,'java.util.Map',0), classAssertionStatus, null)))),
    assert_true('full instantiated fieldDefT except id',
      java_fq(fieldDefT(_, 'java.lang.ClassLoader', type(class,'java.util.Map',0), classAssertionStatus, null))),
    assert_true('',
	   java_fq(fieldDefT(_,_,type(class,'java.lang.Class',0),_,_))).
       	

lookupJavaLangString(JavaLangString):-
     assert_true('look for class java.lang.String',
     (class(JavaLangString,JavaLang,'String'),
      packageT(JavaLang,'java.lang'))).
    