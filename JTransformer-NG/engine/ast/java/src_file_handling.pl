/**
 * Currently also includes tracking of dirty elements.
 */

:- dynamic created_file/1.
:- multifile created_file/1.

:- dynamic modified_toplevel/1.
:- multifile modified_toplevel/1.

/**
 * delete_toplevel(+Path)
 * 
 * Deletes all facts related to the toplevel 
 * specified by Path.
 * Path is the full path to the toplevel: 
 * /Project/OptionalSourcepaths/Package1/../Classname.java"
 */

delete_toplevel(Path):-
	rollback, 
	toplevelT(ID, _, Path, _), 
	deepRetract(ID).

delete_toplevel(_).

/**
 * delete_source_facts
 *
 * deletes all source facts.
 */
 delete_source_facts:-
     rollback,
    toplevelT(ID, _, _, _), 
	deepRetract(ID).
	
	
add_new_class_and_file(Id, Owner, Name, Defs) :-
    add(classDefT(Id, Owner, Name, Defs)),
    ((
       (globalIds(FQN,Id)->
       		true;
       		(fullQualifiedName(Id,FQN)->
       		(add(globalIds(FQN,Id)),
       		 add(ri_globalIds(Id,FQN)));true)
       		),
       modifierT(Id,'public'),
       not(getToplevel(Id,_)),
       getPackage(Id,PID),
       (Owner = null;Owner = PID),
       fullPathOfClass(Id,FullPath),
       print(' added new toplevel: '),       
       print(FullPath),
	   defaultProjectSourceFolder(Project,SourceFolder,FullSourceFolder),
       sformat(S, '/~w/~w.java',[FullSourceFolder,FullPath]),
       string_to_atom(S,Filename),
       new_id(TID),
       add(toplevelT(TID, PID,Filename,[Id])),
       assert(created_file(Filename)),
       add(projectLocationT(TID, Project,SourceFolder))
     );true).


/*
    Helper predicates for file handling
*/

/*
	created_file(-Filename,-Src)
*/

created_file(Filename,Src):-
    created_file(Filename),
    toplevelT(Toplevel,_,Filename,_),
    gen_tree(Toplevel,Src).

/*
	modified_file(-Filename,-Src)
*/

modified_file(Filename,Src):-
    dirty_class(Class),
    getToplevel(Class,Toplevel),
    toplevelT(Toplevel,_,Filename,_),
    not(created_file(Filename)),
    gen_tree(Toplevel,Src).
    
modified_or_dirty_toplevel(Toplevel):-
    dirty_class(Class),
    getToplevel(Class,Toplevel).

modified_or_dirty_toplevel(Toplevel):-
	modified_toplevel(Toplevel).	 
	
sorted_modified_toplevels(Toplevel):-
	setof(Toplevel, (modified_or_dirty_toplevel(Toplevel),toplevelT(Toplevel,_,_,_)),TLs),
	member(Toplevel,TLs).

/*
	dirty_class(-Class)
	
	Binds all dirty classes.
	No class will be bound more than once.
*/

dirty_class(Class):-
    bagof(Tree,(dirty_tree(Tree),enclClass(Tree,Class)),Tree).


/*
	retract_api_meta_data.
*/

retract_api_meta_data :-
    retractall(deleted_file(_)),
    retractall(created_file(_)),
    retractall(dirty_tree(_)),
%    retractall(rollback(_)),
    retractall(modified_toplevel(_)),
    retractall(changed(_)),
    write('RETRACTED CLASS DELTA DATA'),
    nl.
    
    
    
/**
 * file_on_disk(+Toplevel,-Dirfile)
 *
 * Retrieve the absolute file on disk of Toplevel.
 *
 */
file_on_disk(Toplevel,Dirfile) :-
    toplevelT(Toplevel, _Package,Filename, _Defs),
    projectLocationT(Toplevel,Project,_),
    projectT(Project,_,_,OutProjectPath),
    concat('/',Project,FilenameWOProject, Filename),
    appendDir(OutProjectPath, FilenameWOProject, Dirfile).