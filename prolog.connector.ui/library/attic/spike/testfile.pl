node(N):-
	node_arity(A).
    functor(N,node,A).



magic(Node,AttrName,ds(Node,AttrName,AttrVal)):-
    attribute(Node,AttrName,AttrVal).

unmagic(ds(Node,_,_)):-
    arg(1,Node,Color),
    writeln(Color).


connect:-
    node(N),
    magic(N,key,DS),
    unmagic(DS).
   
node_arity(5).
attribute_argnum(color,1).
attribute_argnum(left,2).
attribute_argnum(key,3).
attribute_argnum(value,4).
attribute_argnum(right,5).

node(N):-
	node_arity(A),
    functor(N,node,A).

red(N):-
	node(N),
    attribute(N,color,red).
    
black(N):-
	node(N),
    !,
    attribute(N,color,black).    
black(N):-
	nil(N).    

nil([]).
    

attribute(Node,_Attr,_Val):-
    nil(Node),
    !,
    fail.
attribute(Node,Attr,Val):-
    attribute_argnum(Attr,Num),
    arg(Num,Node,Val).

set_attribute(Nil,_Attr,_Val,_Out):-
    nil(Nil),
    !,
    fail.
set_attribute(In,Attr,Val,Out):-
	attribute_argnum(Attr,Num),
	node_arity(Arity),
	node(Out),
	set_attribute(In,Num,Val,Out,1,Arity).


set_attribute(_In,_Num,_Val,_Out,N,Arity):-
    N>Arity,
    !.
set_attribute(In,Num,Val,Out,Num,Arity):-    
    !,
    arg(Num,Out,Val),
    Next is Num + 1,
	set_attribute(In,Num,Val,Out,Next,Arity).
set_attribute(In,Num,Val,Out,N,Arity):-
    arg(N,In,Val),
    arg(N,Out,Val),
    Next is N + 1,
	set_attribute(In,Num,Val,Out,Next,Arity).
	

