add_class_example_cts(PackageName, ClassName) :-
    apply_ctlist([addPackage(PackageName),addClass(ClassName,PackageName)]).


ct(deleteClass(Package,ClassName),
	(
	  packageT(PID,Package),
	  class(Class,PID,ClassName)
	),
	(
	  delete(class(Class,PID, ClassName))
	)
).
    

ct(addClass,
	(
	  packageT(PID,pckg),
	  not(class(_CID,PID,'NewClass')),
	  new_id(NewCID)
	),
	(
%	  add(modifierT(NewCID,'public')),
	  add(class(NewCID,PID, 'NewClass'))
	)
).

ct(addClass(Name,PackageName),(
	   	packageT(PID,PackageName),
		not(class(_CID,PID,Name)),
		new_id(NewCID)
	),
	(
	  add(modifierT(NewCID,'public')),
	  add(class(NewCID,PID, Name))
	)
).

/*
ct(addClass(Name,PackageName),
	(
	  (
	  	packageT(PID,PackageName);
  	    new_id(PID)
	  ),
	  not(class(_CID,null,Name)),
	  new_id(NewCID)
	),
	(
	  add(packageT(PID,PackageName)),
	  add(class(NewCID,null, Name))
	)
).
*/

ct(addPackage(PackageName), (
	   	not(packageT(PID,PackageName)),
		new_id(PID)
	),
	(
	  add(packageT(PID,PackageName))
	)
).



ctOrSeq( addClassWithPackage(ClassName,PackageName), 
         [addPackage(PackageName),addClass(ClassName,PackageName)]
       ).