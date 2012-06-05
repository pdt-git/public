:- dynamic bla/2,bla/3.
/* 
:- module(modname,[pred1/1,pred2/2]).
:-x([a|[b,c,d|[]]]).
:-x([a|[b,c,d|A]]).
:-x([a|[b,c,d|pseudo]]).
C is 1.
infix2:-writeln(a+b), C is 1+2, a+n.
prefix1:- writeln(\+ 1),  (dynamic v/2).
qual1:-mod:pred(x).
mod:qual2:-pred(x).
la(a):-before(a),after(c).
la(a):-before(a),\+ after(c).
ite:-i->t;e.
ite2:-before,(i->t;e),after.
ite2:-before,(\+ i->t;e),after.
ite2:-before,(i,i2->t,t2;i3->t4;e),after.
neg1:- \+(a;b;c).
forall1:-forall(a,b).
findall1:-findall(a,(b,c,d),e).
*/
    