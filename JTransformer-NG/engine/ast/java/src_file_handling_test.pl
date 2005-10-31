test(dirty_class):-
    findall(C,dirty_class(C),Classes),
    assert_true('excepted two dirty classes',[class,class2] = Classes).
    
setUp(dirty_class):-
    remove_dirty_flags,
	assert(classDefT(class,null,cname,[method1,method2])),
    assert(methodDefT(method1,class,name1,[],type(basic,int,0),[],null)),
    assert(methodDefT(method2,class,name2,[],type(basic,int,0),[],null)),
    assert(dirty_tree(method1)),
    assert(dirty_tree(method2)),

	assert(classDefT(class2,null,cname2,[method3])),
    assert(methodDefT(method3,class2,name3,[],type(basic,int,0),[],null)),
    assert(dirty_tree(method3)).
    
tearDown(dirty_class):-
	retract(classDefT(class,null,cname,[method1,method2])),
    retract(methodDefT(method1,class,name1,[],type(basic,int,0),[],null)),
    retract(methodDefT(method2,class,name2,[],type(basic,int,0),[],null)),
    retract(dirty_tree(method1)),
    retract(dirty_tree(method2)),

	retract(classDefT(class2,null,cname2,[method3])),
    retract(methodDefT(method3,class2,name3,[],type(basic,int,0),[],null)),
    retract(dirty_tree(method3)),
    remove_dirty_flags.
    