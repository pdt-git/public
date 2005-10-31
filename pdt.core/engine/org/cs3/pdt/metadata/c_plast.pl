:-module(c_plast,[
	c_plast_node/1,
	c_plast_prop/2,
	c_variable_symbol/2
]).

:- use_module(plast).
:- use_module(plast_utils).

c_plast_node(c_node(A,_,_)):-
    plast_node(A).

c_plast_prop(c_node(A,Links,Path),Prop):-
    (	variable_symbol(A,Sym),
        link(Sym,Links,Path,Value)
   	->	c_plast_prop(c_node(Value,Links,[Sym|Path]),Prop)
   	;	plast_prop(A,PProp),
	   	wrap_prop(A,Links,Path,PProp,Prop)
   	).

c_variable_symbol(c_node(Var,_,Path),var_sym(Sym,Path)):-
	variable_symbol(Var,Sym).
   	
link(Sym,Links,Path,Value):-
    memberchk([Sym|Path]=Value,Links).


    
wrap_prop(A,Links,Path,parent(P),parent(PW)):-
    (	Path=[Var|VPath],
    	link(Sym,Links,VPath,A),
    	variable_symbol(Var,Sym)
    ->	c_plast_prop(c_node(Var,Links,VPath),PW)
    ;	PW=c_node(P,Links,[Var|Path])
    ).
    
wrap_prop(_,Links,Path,child(C),child(c_node(C,Links,Path))).    
wrap_prop(_,Links,Path,argument(C),argument(c_node(C,Links,Path))).
wrap_prop(_,Links,Path,tail(C),tail(c_node(C,Links,Path))).
wrap_prop(_,Links,Path,member(C),member(c_node(C,Links,Path))).
wrap_prop(_,Links,Path,elements(C),elements(WC)):-
    wrap_elements(Links,Path,C,WC).
wrap_prop(_,Links,Path,arguments(C),arguments(WC)):-
    wrap_elements(Links,Path,C,WC).

wrap_elements(_,_,[],[]).
wrap_elements(Links,Path,[Node|Tail],[c_node(Node,Links,Path)|WTail]):-
    wrap_elements(Links,Path,Tail,WTail).