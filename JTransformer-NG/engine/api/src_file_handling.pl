/**
 * Currently als includes tracking of 
 * dirty elements.
 * Refactoring necessary!
 */


:- dynamic created_file/1.
:- multifile created_file/1.


add_new_class_and_file(_id, _owner, _name, Defs) :-
    add(classDefT(_id, _owner, _name, Defs)),
    ((
       (globalIds(FQN,_id)->
       		true;
       		(fullQualifiedName(_id,FQN)->
       		add(globalIds(FQN,_id));true)
       		),
       modifierT(_id,'public'),
       not(getToplevel(_id,_)),
       getPackage(_id,PID),
       (_owner = null;_owner = PID),
       fullPathOfClass(_id,FullPath),
       print(' added new toplevel: '),       
       print(FullPath),
	   defaultProjectSourceFolder(Project,SourceFolder,FullSourceFolder),
       sformat(S, '/~a/~a.java',[FullSourceFolder,FullPath]),
       string_to_atom(S,Filename),
       new_id(TID),
       add(toplevelT(TID, PID,Filename,[_id])),
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

/*
	dirty_class(-Class)
	
	Binds all dirty class.
	No class will be bound more than once.
*/

dirty_class(Class):-
    bagof(Tree,(dirty_tree(Tree),enclClass(Tree,Class)),Tree).

test(dirty_class):-
    findall(C,dirty_class(C),Classes),
    assert_true('excepted two dirty classes',[class,class2] = Classes).
    
setUp(dirty_class):-
    remove_dirty_flags,
	assert(classDefT(class,null,cname,[method1,method2])),
    assert(methodDefT(method1,class,name1,[],type(basic,int,0),[],null)),
    assert(methodDefT(method2,class,name2,[],type(basic,int,0),[],null)),
    assert(dirty_tree(method1)),
    assert(dirty_tree(method2)),

	assert(classDefT(class2,null,cname2,[method3])),
    assert(methodDefT(method3,class2,name3,[],type(basic,int,0),[],null)),
    assert(dirty_tree(method3)).
    
tearDown(dirty_class):-
	retract(classDefT(class,null,cname,[method1,method2])),
    retract(methodDefT(method1,class,name1,[],type(basic,int,0),[],null)),
    retract(methodDefT(method2,class,name2,[],type(basic,int,0),[],null)),
    retract(dirty_tree(method1)),
    retract(dirty_tree(method2)),

	retract(classDefT(class2,null,cname2,[method3])),
    retract(methodDefT(method3,class2,name3,[],type(basic,int,0),[],null)),
    retract(dirty_tree(method3)),
    remove_dirty_flags.
    
    


/*
	retract_api_meta_data.
*/

retract_api_meta_data :-
    retractall(deleted_file(_)),
    retractall(created_file(_)),
    retractall(dirty_tree(_)),
    retractall(rollback(_)),
    retractall(changed(_)).