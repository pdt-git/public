:- style_check(-atom).
 /*ld:
  * here we define class_type_name/3 and its requirements.
  * This predicate should be used to get the identifier
  * by which one can refer to a given type from a given
  * location (scope) in the ast.
  *
  * In particular this handles correct qualifiying of nested, local and
  * surrounding types.  (which is crucial, see JT-128) and in the long term
  * should cover global types aswell (which is rather a cosmetic problem).
  * Where should be ground and specify the id of the refering scope 
  * (e.g. a localDefT, a methodDefT, ...)
  * Type should be ground and a type term.
  * The predicate succeeds if Name can be unified with the minimal qualified name
  * that unambigously references Type. (Implementation incomplete: for 
  * non-local types the name will always be fully qualified.)
  */     
type_name(_,type(basic,Type,0),Type):-!.
type_name(Where,type(class,Type,0),Name):-
    !,
    class_type_name(Where,Type,Name).
type_name(Where,type(Kind,Type,Dim),Name):-
    type_name(Where,type(Kind,Type,0),ElmTypeName),
    empty_square_brackets(Dim,Brackets),
    atom_concat(ElmTypeName,Brackets,Name).
    
empty_square_brackets(0,'') :-!.
empty_square_brackets(_dim,A) :-
    succ(_dimDec, _dim),
    empty_square_brackets(_dimDec,AA),
    atom_concat('[]',AA,A).

%FIXME: temporary for the TRANSFORMATION WS DEMO June 05:
class_type_name(_Where,Type,Name) :-
    classDefT(Type,Package,Name,_),
    packageT(Package,'java.lang'),
    !.

  
class_type_name(Where,Type,Name):-
    next_common_scope(Where,Type,Scope),
    !,
    (	packageT(Scope,_)
    ->	class_type_name_package(Where,Type,Scope,Name)
    ;	encl_path_down(Scope,Type,DeclPath),
	    encl_path_down(Scope,Where,RefPath),
    	class_type_name_local(DeclPath,RefPath,Name)
    ).
class_type_name(_Where,Type,Name):-
    %%getToplevel(Where,Toplevel),
    class_type_name_global(_,Type,Name).

class_type_name_package(_,Type,_,Name):-
    %%now special treatment for package scope right now
    %%getToplevel(Where,Toplevel),
    class_type_name_global(_,Type,Name).

class_type_name_global(_,Type,Name):-
   %% no attempt is made to see if we can use an
   %% unqualified name. atm we lack classpath information
   %% to ensure the absence of type name conflicts.
   %% so all non-local class names are fully qualified for now.
    fullQualifiedName(Type, Name).


class_type_name_local([H|[]],_,Name):-
	!,
	class_type_name_recursive([H],Name).   	
class_type_name_local([Root,DeclHead|DeclTail],[Root|RefTail],Name):-
    classDefT(DeclHead,_,CName,_),
     obscured_on_path(CName,RefTail),
     !,
     class_type_name_recursive([Root,DeclHead|DeclTail],Name).     
class_type_name_local([Root,DeclHead|DeclTail],[Root|RefTail],Name):-
   	methodDefT(Root,_,_,_,_,_,_),
   	newClassT(DeclHead,_,Root,_,_,_,C,_),
   	classDefT(C,_,CName,_),
     obscured_on_path(CName,RefTail),
     !,
     class_type_name_recursive([Root,DeclHead|DeclTail],Name).     
class_type_name_local([Root,DeclHead|DeclTail],[Root|RefTail],Name):-
   	methodDefT(Root,_,_,_,_,_,_),
   	execT(DeclHead,_,Root,C),
   	classDefT(C,DeclHead,CName,_),
     obscured_on_path(CName,RefTail),
     !,
     class_type_name_recursive([Root,DeclHead|DeclTail],Name).     
class_type_name_local([_|DeclTail],_,Name):-   	
     class_type_name_recursive(DeclTail,Name).     
     
     
class_type_name_recursive([],''):-
	  !,
	  fail.      
class_type_name_recursive([H|T],Name):-
	execT(H,_,_,_),
	!,
	class_type_name_recursive(T,Name).
class_type_name_recursive([H|T],Name):-
	newClassT(H,_,_,_,_,_,_,_),
	!,
	class_type_name_recursive(T,Name).	  
class_type_name_recursive([H|T],Name):-
	classDefT(H,_,CName,_),
	class_type_name_recursive(T,TName),
	!,
	concat_atom([CName,'.',TName],Name).
class_type_name_recursive([H|_],Name):-
	classDefT(H,_,Name,_).

obscured_on_path(CName,[H|_]):-
    classDefT(H,_,CName,_).
obscured_on_path(CName,[H|_]):-
    classDefT(_,H,CName,_).
obscured_on_path(CName,[H|_]):-
   	methodDefT(H,_,_,_,_,_,_),
   	newClassT(_,_,H,_,_,_,C,_),
   	classDefT(C,_,CName,_).
obscured_on_path(CName,[H|_]):-
   	methodDefT(H,_,_,_,_,_,_),
   	execT(E,_,H,C),
   	classDefT(C,E,CName,_).
obscured_on_path(CName,[_|T]):-
    obscured_on_path(CName,T).


next_common_scope(NodeA,NodeB,Scope):-
    encl_path_down(NodeA,PathA),
    encl_path_down(NodeB,PathB),
    common_head(PathA,PathB,Head),
    lastElement(Head,Scope).

common_head([A|S],[A|T],[A|X]):-
    !,
    common_head(S,T,X).
common_head(_,_,[]).

encl_path_down(Node,Path):-
	encl_path_up(Node,PathUp),
	revertList(PathUp,Path).

encl_path_down(Root,Node,Path):-
	encl_path_up(Node,Root,PathUp),
	revertList(PathUp,Path).
      
encl_path_up(Node,[Node]):-
    packageT(Node,_),!.
encl_path_up(Node,[Node|Tail]):-
    enclosing(Node,Enc),
    encl_path_up(Enc,Tail).


encl_path_up(Node,Node,[Node]):-
    !.
encl_path_up(Node,_,[Node]):-
    packageT(Node,_),
    !,
    fail.
encl_path_up(Node,Root,[Node|Tail]):-
    enclosing(Node,Enc),
    encl_path_up(Enc,Root,Tail).

	   
