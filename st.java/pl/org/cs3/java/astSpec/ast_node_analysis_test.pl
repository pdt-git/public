:- use_module(ast_node_analysis).

setUp(ast_with_identifiers):-
    retractall(debug_rollback_output),
    add(classDefT(c1,2,cname,[])),
    add(methodDefT(m1,2,mname,4,5,6,7)),
    add(identT(i1,2,3,iname,5)),
    add(packageT(p1,pname)),
    add(paramDefT(param1,2,3, paramname)),
    add(applyT(a1,2,3,applyname,5,6,7)),
    add(selectT(s1,2,3,selectname,5,6)),
    add(localDefT(l1,2,3,type(class,2,3), localname,6)),
    add(fieldDefT(f1,2,3,fname,5)).
    

test(ast_with_identifiers) :-
    assert_true('', ast_with_identifiers(c1,'Java', cname)),
    assert_true('', ast_with_identifiers(m1,'Java', mname)),
    assert_true('', ast_with_identifiers(f1,'Java', fname)),
    assert_true('', ast_with_identifiers(p1,'Java', pname)),
    assert_true('', ast_with_identifiers(param1,'Java', paramname)),
    assert_true('', ast_with_identifiers(a1,'Java', applyname)),
    assert_true('', ast_with_identifiers(s1,'Java', selectname)),
    assert_true('', ast_with_identifiers(l1,'Java', localname)),
    assert_true('', ast_with_identifiers(i1,'Java', iname)).

tearDown(ast_with_identifiers):-
    rollback.