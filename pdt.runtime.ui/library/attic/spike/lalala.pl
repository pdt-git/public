


/*
angenommen, die Klausel wurde für diese Instanz schon vollständig expandiert.
weiter angenomman, wir wüßten daß f(X) projeziert auf PVar zyklisch ist.
Unter welchen Umständen führt eine weitere instanziierung des Arguments zu
neuen (relevanten) Dereferenzierungen?
*/
p(f(X)):-
    body(X).
    usw.
    
/*
- wenn body(X) ein literal nonvar(X) enthält?
 im einfachsten fall würde eine weitere iteration auf jeden Fall reichen.
*/    

p(f(X)):-
    do_something(X),
    nonvar(X),
    do_something_else(X),
    usw.

/*
 - was ist mit rekursion?
 kein problem im kontrahierende fall:
*/
p(f(X)):-
    do_something(X),
    nonvar(X),
    p(X),
    do_something_else(X),
    usw.
p(f(f(X))):-
    do_something(f(X)),
    nonvar(f(X)),
    p(f(X)),
    do_something_else(f(X)),
    usw.
p(f(f(X))):-
    do_something(f(X)),
    nonvar(f(X)),
    p(f(X)),
    do_something_else(f(X)),
    usw.

    
/*
 -nicht kontrahierender fall
 -auch hier reicht offenbar eine weitere iteration.
*/
p(f(X)):-
    do_something(X),
    nonvar(X),
    p(f(f(X))),
    do_something_else(X),
    usw.
    

/*
nächstes problem: Variable wird dereferenziert.

im einfachen Fall reicht eine weitere iteration
*/    

p(f(X)):-
    do_something(X),
    X,    
    do_something_else(X),
    usw.
/*
kontrahierende rekursion
*/    
/*
p(f(X)):-
    do_something(X),
    X,    
    p(X),
    do_something_else(X),
    usw.


p(X):-
	p(f(X)).
	
p(1)->{X=1},p(f(X))
	->{X_=f(X)},p(f(X_))
	->{X__=f(X_)},p(f(X__))

p(A)->{X=A},p(f(X))
	->{X_=f(X)},p(f(X_))
	->{X__=f(X_)},p(f(X__))	

p(X):-
    call(X),
	p(f(X)).

p(1)->{X=1},call(X),p(f(X))
	->{X=1,mark(p(1))},p(f(X))
	->{X_=f(X)},call(X_),p(f(X_))
	->{X_=f(X),mark(p(f(X)))},p(f(X_))
	->{X__=f(X_)},call(X__),p(f(X__))

p(f(1))->{X=f(1)},call(X),p(f(X))
	->{X=1,mark(p(1))},p(f(X))
	->{X_=f(X)},call(X_),p(f(X_))
	->{X_=f(X),mark(p(f(X)))},p(f(X_))
	->{X__=f(X_)},call(X__),p(f(X__))
	*/
	
resolve(Cx,Head,Module:Head2):-
    Cx:strip_module(Head,Cx2,Head2),
    %to avoid loading of auto-load predicates (slooooooow).
    % we use  the iso-complient current_predicate/1.
    functor(Head2,Name,Arity),
    Cx2:current_predicate(Name/Arity), 
    (	Cx2:predicate_property(Head2,imported_from(Module))
    ;	Cx=Module
    ),
    !.
    






t:-
    load(X0),mod(p,X0,X1),check(X1), || load(Y0),
    mod(X1,Y0,Y1),
    store(X1),mod(bla,X1,X2),writeln(X2), ||    mod_nondet(blu,Y1,Y2),check(Y2),store(Y2).

	
    

    
/*
y: load (Y0), mod(X1,Y0,Y1), mod(blu,Y1,Y2)
*/
t:-
    load(X0),
    load(Y0),
    mod(p,X0,X1),
    check(X1),
    mod(X1,Y0,Y1),
    mod(blu,Y1,Y2),
    store(Y1),
    store(X1),
    mod(bla,X1,X2),
    writeln(X2).



bla:-
    fx,fail.