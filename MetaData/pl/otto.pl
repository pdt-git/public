:-module(otto,[
arsch/1,
source_folder/1,
unfug/1,
predicate/1,
find_module/1
]).


unfug(_):-
    true.
    
arsch(lala):-writln(ja). arsch(lala):-writln(ja). arsch(lala):-writln(ja).

source_folder(A):-
	source_file(F),
	file_directory_name(F,A).
	
predicate(Mod:Pred):-
    writeln(kaka),
    nonvar(Pred),
    !,
    find_module(Mod:Pred).
/**
predicate(Mod:Pred):-
    writeln(kaka),
    nonvar(Mod),
    !,
    find_module(Mod:Pred).
*/

find_module(Mod:Pred):-
    writeln(pipi),
    user:predicate_property(Pred,imported_from(Mod)).

find_module(Mod:Pred):-
    writeln(popel),
    user:source_file(File),
    user:source_file(Mod:Pred,File).    
    
find_module(user:Pred):-
    writeln(eiter),
    user:current_predicate(_,Pred),
    (	user:predicate_property(Pred,imported_from(_))
    ->	fail
    ;	true
    ).