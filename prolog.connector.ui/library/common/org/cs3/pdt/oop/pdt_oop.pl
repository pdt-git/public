% experimental. Don't lough :-)

% no docs yet, just a braindump (german)
/*
prolopg "oop": ich brauche:
 - polymorphismus
 - effiziente Type-checks
 - Liskov-ersetzbarkeit.
 - instanzen auf dem stack.
 
Idee: Kontextterme + forwarding.

Die grundidee: instanzen tragen typinformationen, insb. das modul das die jeweilige Klasse definiert). 
Aufrufe von abstrakten Prädikaten können so immer an die konkreteste implementierung weitergeleitet werden.
Module von Überklassen werden zu den Import Modulen von Unterklassen hinzugefügt.
Es gibt nur eine Klasse pro modul.

Instanzfelder werden durch kontext terme realisiert. Hierbei muß nur Sorge getragen werden, daß die accessor 
prädikate in abstrakteren "Klassen" mit den Kontexttermen konkretere IMplementierungen arbeiten können. 
Die von pdt_util_context generierten accessor prädikate müssen erweitert werden, so daß es mit 
folgenden termen umgehen kann:
 $pdt_object(Modul:CxName,Data)
 
 und die accessor aufrufe entsprechend forwarden kann.
Anders ausgedrückt: Es wird (automatisch) dafür gesorgt, daß alle accessor prädikate überschrieben werden. 
Da anders nicht auf die felder zugegriffen werden "kann", ist zumindest syntaktische Liskov-ersetzbarkeit 
gewährleistet.


*/

%:- module(pdt_oop,[pdt_define_class/1]).

:- module_transparent '<-'/2.

pdt_class(Fields):-
    context_module(Cx),
    pdt_class(Cx,Fields).

pdt_class(user,_):-
    throw(error(doofheit(module_user_as_class))).    
pdt_class(Cx,Fields):-
    Template=..[$data|Fields],
    pdt_define_context(Cx,Template),
	assert(current_class(Cx,Fields)).
	
pdt_inherit(Cx,Module):-
    (	current_class(Module,InheritedFields)
    ->	true
    ;	throw(error(existence_error(class,Module)))
    ),
    (	current_class(Cx,NewFields)
    ->	true
    ;	throw(error(existence_error(class,Cx)))
    ),
    append(InheritedFields,NewFields,Fields),
    retractall(current_class(Cx,_)),
    assert(current_class(Cx,Fields)),
	Template=..[$data|Fields],
   	pdt_define_context(Cx,Template),	
	assert(class_inherits(Cx,Module)).	
    

'<-'($pdt_object(Module,Data),Goal):-
    context_module(Context),
    Goal=..[Name|Args],
    append(Args,[$pdt_object(Module,Data),Context],Args2),
    Goal2=..[Name|Args2],    
	Module:Goal2.

forward_accessors(Module):-
    current_class(Module,Fields),
    forall(member(Field,Fields),(    	
    	forward_getter(Module,Field),
    	forward_setter(Module,Field)    	
    )).
	forward_multigetter(Module,Fields),
	forward_multisetter(Module,Fields).



forward_getter(Module,Field):-
    functor(Head,Field,2),
    arg(1,Head,Value),
    atom_concat('$data_',Field,GetterName),
    functor(Getter,GetterName,1),
    arg(1,Getter,Value),
    Module:assert(':-'(Head,Getter)).

forward_setter(Module,Field):-
    functor(Head,Field,1),
    arg(1,Head,Value),
    atom_concat('$data_set_',Field,SetterName),
    functor(Setter,SetterName,1),
    arg(1,Setter,Value),
    Module:assert(':-'(Head,Getter)).


% 
% 	my_mode:foo_new(foo(_,_,_,_)).
% 	my_mode:foo_bar(foo(B,_,_,_),B).
% 	my_mode:foo_baz(foo(_,B,_,_),B).
% 	my_mode:foo_rumpel(foo(_,_,B,_),B).
% 	my_mode:foo_knarz(foo(_,_,_,B),B).
% 	my_mode:foo_set_bar(foo(_,A,B,C),Bar,(Bar,A,B,C)).
% 	my_mode:foo_set_baz(foo(A,_,B,C),Baz,(A,Baz,B,C)).
% 	my_mode:foo_set_rumpel(foo(A,B,_,C),Rumpel,(A,B,Rumpel,C)).
% 	my_mode:foo_set_knarz(foo(A,B,C,_),Knarz,(Bar,A,B,C)).
% 
% 	my_mode:foo_get(Foo,FieldValueList):-
%   	pdt_util_context:pdt_context_get_values(my_mode,Foo,FieldValueList).
% 
% 	my_mode:foo_set(FooIn,FieldValueList,FooOut):-
%   	pdt_util_context:pdt_context_set_values(my_mode,FooIn,FieldValueList,FooOut).
%
	