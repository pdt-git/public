:- multifile test/1.
:- multifile setUp/1.
:- multifile tearDown/1.

setUp(abba:get_defining_file):-
   add(abba:property(lId(4),position(17,333))),
   add(abba:property(lId(4),file('/LajTeaching/src/pckg/TestAspect.aj'))),
   add(abba:node(lId(4),'laj_aspect','org.aspectj.compiler.crosscuts.ast.AspectDec')),
   add(abba:node(lId(5),'laj_body','org.aspectj.compiler.base.ast.ExprStmt')),
   add(abba:property(lId(5),position(320,12))),
   add(abba:edge(lId(114),parent,'???',lId(5),lId(27))),
   add(abba:node(lId(27),'laj_effect','advice:_default_')),
   add(abba:property(lId(27),position(191,153))),
   add(abba:edge(lId(115),parent,'???',lId(27),lId(4))).   

test(abba:get_defining_file) :-
   abba:get_defining_file(lId(5), File),
   assert_true(equals(File, '/LajTeaching/src/pckg/TestAspect.aj')).

tearDown(abba:get_defining_file):-
   rollback.