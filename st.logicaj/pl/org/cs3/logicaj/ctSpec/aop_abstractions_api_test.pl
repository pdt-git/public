:- multifile setUp/1.
:- multifile test/1.
:- multifile tearDown/1.

/*************  getRealEncl *****************/
setUp('getRealEncl'):-
    assert(methodDefT(methorig,class,methname,[],type(class,whatever,0),[],body)),
    assert(applyT(applyorig,parent,methforw,null,methname,args,methorig)),
    assert(applyT(applyforw,parent,methorig,null,methname,args,methforw)),
    assert(forwards(applyforw,methforw,methodCall,applyorig)).

test('getRealEncl'):-
%    assert_true('enclosing method of a method', getRealEncl(applyorig,methorig,methorig)),
    assert_true('enclosing method of a forwarding method', getRealEncl(applyorig,methforw,methorig)).
    
    
tearDown('getRealEncl'):-
    retract(methodDefT(methorig,class,methname,[],type(class,whatever,0),[],body)),
    retract(applyT(applyorig,parent,methforw,null,methname,args,methorig)),
    retract(applyT(applyforw,parent,methorig,null,methname,args,methforw)),
    retract(forwards(applyforw,methforw,methodCall,applyorig)).


