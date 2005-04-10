/*
main api

packageT(id, name).

class(#id, #owner, name).
interface(#id).
inner(#id).
local(#id).
anonymous(#id).

fullQualifiedName(#id, fqn).

extends(#class, #extendedClass).
implements(#class, #interfaceClass).

method     (#id, #class, name, [#param], TYPE, [#exceptions], #body).
constructor(#id, #class, [#param], [#exceptions], #body).

field   (#id, #class,    TYPE, name, #init).
param   (#id, #method,   TYPE, name).
localVar(#id, #parent, #enclMeth, TYPE, name, #init).

getField  (#identSelect, #parent, #enclMeth, #recv, #field).
setField  (#identSelect, #parent, #enclMeth, #recv, #field,  #valueID).
methodCall(#apply,       #parent, #enclMeth, #recv, Name, #method, [#args]).
execution (#methodDef,   #parent, #enclClass, #method, [#params]).
newCall   (#newClass,    #parent, #enclMeth, #recv, #method, [#args]).

helper clauses:
enclMethod                  (_id, _Encl)
getReceiver                 (_ident, _receiver)
createReturn                (_parent, _encl, _type, _exec, _ExecParent)
notInForwardingMethod       (_tree, 'null')
findEnclAndOrigMethod       (_apply, _identSelect, _EnclMethod, _OrigMethod)
enclMethodOrNull            (_tree, _enclMethod)
createIdentIfNeeded         (_valueID, _parent, _encl, _new_id, _new_id)
updateParentIdentSelect     (_selected, _newParent, _newEncl)
identSelect                 (_id, _parent, _encl, null, _name, _symbol)
addToMethodArgs             (_method, _id)
removeFromMethodArgs        (_method, _id)
addToBlock                  (_block, _id)
removeFromBlock             (_block, _id)
add_to_class                  (_class, _id)
removeFromClass             (_class, _id)
cloneVarDef                 (_newParent, _varDef, _Copy)
createVarDefIdents           (_newParent, _oldList, _newList)
*/

%:- ['garbage_collect.pl'].

:- multifile subTreeArg/2.
:- multifile forwards/4.
:- dynamic forwards/4.
:- dynamic forwarding/1.
:- dynamic forwarding/2.
:- multifile action/1.
:- multifile cond/1.
:- multifile abstraction/1.
:- dynamic abstraction/1.
:- dynamic deleted_file/1.
:- multifile deleted_file/1.
:- dynamic created_file/1.
:- multifile created_file/1.

:- dynamic isErrorWarningMessage/3.


cond(src(_type)).

src(Type):-   
    not(java_fq(externT(Type))).

/**
 * Flatten List of Lists/Elements.
 * e.g [[a,b],c,[d,e,f]] -> [a,b,c,d,e,f].
 * concat(+Lists,?List)
 */
 
cond(concat_lists(_Lists,_List)).

concat_lists([Element],[Element]) :-
    var(Element).

concat_lists([Element|Tail],[Element|TailFlat]) :-
    var(Element),
    !,
	concat_lists(Tail,TailFlat).

concat_lists([[]],[]):-!.
concat_lists([[Head|Tail]],[Head|Tail]):-!.
concat_lists([Elem],[Elem]).

concat_lists([[HeadHead|HeadTail]|Tail],FlatList) :-
    !,
	concat_lists(Tail,TailFlat),
    append([HeadHead|HeadTail],TailFlat,FlatList).
    
concat_lists([[]|Tail],TailFlat):-
	concat_lists(Tail,TailFlat).

concat_lists([Head|Tail],[Head|TailFlat]):-
	not(is_list(Head)),
	concat_lists(Tail,TailFlat).    
    
test(concat_list_2):-
    assert_true('[a,b],[d,e,f]',concat_lists([[a,b],[d,e,f]],
    	[a,b,d,e,f])),
    assert_true('[a,b],[d,e,f]',(concat_lists([[a,b],[d,e,f]],
    	Flat),!,Flat=[a,b,d,e,f])),

    assert_true('[a,b],c,[d,e,f]', concat_lists([[a,b],c,[d,e,f]],
    	[a,b,c,d,e,f])),
    assert_true('[a,B],C,[d,e,f]',concat_lists([[a,B],C,[d,e,f]],
    	[a,B,C,d,e,f])),
   assert_true('[a,B],C,[d,e,f]',(concat_lists([[a,B],C,[d,e,f]],
    	Flat2), !,Flat2=[a,B,C,d,e,f])),
   assert_true('[],[],[d,e,f]',(concat_lists([[],[],[d,e,f]],Flat3),
    !,Flat3=[d,e,f])),
   assert_true('[A],[A]',(concat_lists([V1],[V2]),
    !,V1 == V2)).

/**
 * action(set_parent(+ID,+NewParent))
 */

action(set_parent(ID,Parent)):-
    set_parent(ID,Parent).
    
/*

meta interpreter

exists(PEF) :- 
    trivialPEF(PEF,Trivial),
    getHighLevelType(PEF,HLType),
    (var(HLType), ... 
    ;
     nonvar(HLType), ...
     ).
*/

cond(name_concat(_Head,_Rest, _Name)).
name_concat(Head,Rest, Name) :-
    atom_concat(Head,Rest,Name).

cond(package(_ClassName,_PackageName)).

package(ClassName,PackageName):-
	class(_,Package,ClassName),
	packageT(Package,PackageName).
	
cond(concat(_Sub,_Left,_Right,_Complete)).	
concat(Left,Sub,Right,Atom):-
    sub_atom(Atom,Before,LenSub,After,Sub),
    sub_atom(Atom,0,Before,_,Left),
    plus(Before,LenSub,StartRight),
    sub_atom(Atom,StartRight,After,_a,Right).
    
cond(concat(_Sub,_Left,_Right)).	

cond(class(_Name)).	
class(Name):-
    java_fq(classDefT(Name,_,_,_)).

action(delete(bodyFact(_encl))) :-
    !,
    action_all(delete(getField(_, _, _encl, _, _, _))),
    action_all(delete(setField(_, _, _encl, _, _, _))),
    action_all(delete(newCall(_, _, _encl, _, _, _))),
    action_all(delete(methodCall(_, _, _encl, _, _, _, _))).
% body fact is not callable - prevent standard action_all treatment
action_all(delete(bodyFact(_encl))) :-
    !,
    action(delete(bodyFact(_encl))).

action_all(delete(_a)) :-
    findall(_a, (call(_a), action(delete(_a))), _l).

abstraction(class(_id, _owner, _name)).

cond(class(_id, _owner, _name,_body)).

class(_id, _owner, _name,_body) :-
    classDefT(_id, _owner, _name, _body).

subTreeArg(class, 4).
class(_id, _owner, _name) :-
    classDefT(_id, _owner, _name, _).

action(add(class(_id, _owner, _name,Defs))) :-
    !,
    add_classDefT(_id, _owner, _name,Defs).
        
action(add(class(_id, _owner, _name))) :-
    !,
    add_classDefT(_id, _owner, _name,[]).
 
action(add(class(Id, Owner, Name,Defs))) :-
    !,
    add_classDefT(Id, Owner, Name,Defs).
    
add_classDefT(_id, _owner, _name,Defs):-
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
    add(classDefT(_id, _owner, _name, _defs)),
    ((
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
*/

/*
	sourceFolder(+Toplevel,-Sourcefolder)
	
	retrieves the source folder of the toplevel.
	If no source folder is specified the default
	source folder is bound.
*/

sourceFolder(Toplevel, Project):-
    projectLocationT(Toplevel,Project,''),
    !.

sourceFolder(Toplevel, SourceFolder):-
    projectLocationT(Toplevel,Project,Folder),
    atom_concat(Project,'/',Tmp),
    atom_concat(Tmp,Folder,SourceFolder),
    !.


/*
	defaultProjectSourceFolder(-Project,-SourceFolder,-FullSourceFolder)
	
	Binds Project and SourceFolder to the default project and source folder.
	FullSourceFolder is bound to Project/Sourcefolder.
	
	TODO: by now it picks the first 
	projectLocationT fact and binds its
	source folder.
*/

defaultProjectSourceFolder(Project,'',Project) :-
    projectLocationT(_,Project,''),
    !.
    
defaultProjectSourceFolder(Project, Folder, FullPath) :-
    projectLocationT(_,Project,Folder),
    atom_concat(Project,'/',Tmp),
    atom_concat(Tmp,Folder,FullPath).

action(delete(class(_id, _owner, _name))) :-
    deleteToplevelOfClass(_id),
    classDefT(_id, _owner, _name, Defs),
    delete(classDefT(_id, _owner, _name, Defs)),
    findall(Modifier,(modifierT(_id,Modifier),delete(modifierT(_id,Modifier))),_).

deleteToplevelOfClass(Id) :-
    modifierT(Id,public),
    getToplevel(Id, Tl),
    !,
    findall(Import,(importT(Import,Tl,ClassPckg),delete(importT(Import,Tl,ClassPckg))),_),
%    findall(Class,(classDefT(Class,Package,Name,List),delete(classDefT(Class,Package,Name,List))),_),
    toplevelT(Tl,TlPackage,Filename, Defs),
    assert(deleted_file(Filename)),
    delete(toplevelT(Tl,TlPackage,Filename, Defs)).

deleteToplevelOfClass(_).    
    
action(replace(class(_id, _owner, _name),class(_id, _owner1, _name1))) :-
    classDefT(_id,_,_,_defs),
    !,
    action(delete(classDefT(_id, _owner, _name, _defs))),
    action(add(class(_id, _owner1, _name1, _defs))).
    
action(replace(class(_id, _owner, _name))) :-
    classDefT(_id,_,_,_defs),
    !,
    action(replace(classDefT(_id, _owner, _name, _defs))).

action(replace(class(_id, _owner, _name))) :-
    action(replace(classDefT(_id, _owner, _name, []))).


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
 	helper.
*/

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
       

/*    
action(delete(class(_id, _owner, _name))) :-
    %format('defs ~a~n',[_defs]),
%    classDefT(_id, _owner, _name, _defs),
    action_all(delete(method(_, _id, _, _, _, _, _))),
    action_all(delete(field(_, _id, _, _, _))),
    action_all(delete(interface(_id))),
    action_all(delete(extends(_id, _))),
%    classDefT(_id, _owner, _name, _alteredDefs),
    %format('defs ~a~n',[_alteredDefs]),
    delete(classDefT(_id, _owner, _name, [])).
% todo ??? inner classes
%    action(delete(class(_, _id, _, _, _))),
*/

cond(extends(_class, _super)).
extends(_class, _super) :-
    extendsT(_class, _super).
action(add(extends(_class, _super))) :-
    add(extendsT(_class, _super)).
action(delete(extends(_class, _super))) :-
    delete(extendsT(_class, _super)).

cond(implements(_class, _super)).
implements(_class, _super) :-
    implementsT(_class, _super).
action(add(implements(_class, _super))) :-
    add(implementsT(_class, _super)).
action(delete(implements(_class, _super))) :-
    delete(implementsT(_class, _super)).

cond(subtype(_sub, _super)).

/**
 * subtype(?Sub, ?Super)
 *
 * Binds Super to any direct or 
 * indirect super type of Sub.
 * 
 * Only for object types.
 */

subtype(_sub, _sub).
subtype(_sub, _super) :-
    extendsT(_sub,_super).
subtype(_sub, _super) :-
    implementsT(_sub,_super).
subtype(_sub, _super) :-
    extendsT(_sub,_subsuper),
    subtype(_subsuper, _super).
subtype(_sub, _super) :-
    implementsT(_sub,_subsuper),
    subtype(_subsuper, _super).

%:- dynamic shareModifier/3.
%:- multifile shareModifier/3.

cond(shareModifier(_modifier, _tree1, _tree2)).

shareModifier(_modifier, _tree1, _tree2) :-
    modifierT(_tree1,_modifier),
    modifierT(_tree2,_modifier), !.
shareModifier(_modifier, _tree1, _tree2) :-
    not(modifierT(_tree1,_modifier)),
    not(modifierT(_tree2,_modifier)), !.
shareModifier(_modifier, _tree1, _tree2) :-
    modifierT(_tree1,_modifier),
    add(modifierT(_tree2,_modifier)),!.
shareModifier(_modifier, _tree1, _tree2) :-
    not(modifierT(_tree1,_modifier)),
    delete(modifierT(_tree2,_modifier)),!.

action(shareModifier(_modifier, _tree1, _tree2)) :-
	shareModifier(_modifier, _tree1, _tree2).

cond(interface(_id)).
interface(_id) :-
    class(_id,_,_),
    interfaceT(_id).
action(add(interface(_id))) :-
    add(interfaceT(_id)).
action(delete(interface(_id))) :-
    % remove all implementing incomming edges
    %action_all(delete(implements(_, _id))),
    % remove all implementing outgoing edges
    action_all(delete(implements(_id, _))),
    delete(interfaceT(_id)).


abstraction(method(_id, _class, _name, _params, _type, _exceptions, _body)).
subTreeArg(method, 4).
subTreeArg(method, 6).
subTreeArg(method, 7).
method(_id, _class, _name, _params, _type, _exceptions, _body) :-
    methodDefT(_id, _class, _name, _params, _type, _exceptions, _body),
    _name \= '<clinit>',
    _name \= '<init>'.

action(add(method(_id, _class, _name, _params, _type, _exceptions, _body))) :-
    add(methodDefT(_id, _class, _name, _params, _type, _exceptions, _body)),
    add_to_class(_class, _id).

action(replace(methodDefT(_id, _class, _name, _params, _type, _exceptions, _body),
    methodDefT(_id, _class1, _name1, _params1, _type1, _exceptions1, _body1))).

action(replace(method(_id, _class, _name, _params, _type, _exceptions, _body))) :-
    action(replace(methodDefT(_id, _class, _name, _params, _type, _exceptions, _body))).

action(delete(method(_id, _class, _name, _params, _type, _exceptions, _body))) :-
%    action_all(delete(bodyFact(_id))),
    delete(methodDefT(_id, _class, _name, _params, _type, _exceptions, _body)),
    removeFromClass(_class, _id).


cond(constructor(_id, _class, _params, _exceptions, _body)).
subTreeArg(constructor, 3).
subTreeArg(constructor, 4).
subTreeArg(constructor, 5).
constructor(_id, _class, _params, _exceptions, _body) :-
    methodDefT(_id, _class, '<init>', _params, type(basic,void,0), _exceptions, _body).
action(add(constructor(_id, _class, _params, _exceptions, _body))) :-
    add(methodDefT(_id, _class, '<init>', _params, type(basic,void,0), _exceptions, _body)),
    add_to_class(_class, _id).
action(replace(constructor(_id, _class, _params, _exceptions, _body))) :-
    action(replace(methodDefT(_id, _class, '<init>', _params, type(basic,void,0), _exceptions, _body))).
action(delete(constructor(_id, _class, _params, _exceptions, _body))) :-
    action_all(delete(method(_id, _class, '<init>', _params, type(basic,void,0), _exceptions, _body))),
    removeFromClass(_class, _id).


abstraction(field(_id, _class, _RetType, _name, _init)).
subTreeArg(field, 5).
field(_id, _class, _RetType, _name, _init) :-
    fieldDefT(_id, _class, _RetType, _name, _init),
    class(_class, _,_).

action(add(field(_id, _class, _RetType, _name, _init))) :-
    add(fieldDefT(_id, _class, _RetType, _name, _init)),
    add_to_class(_class, _id).

action(replace(field(_id, _class, _RetType, _name, _init),
    field(_id, _class1, _RetType1, _name1, _init1))) :-

action(replace(fieldDefT(_id, _class, _RetType, _name, _init),
    fieldDefT(_id, _class1, _RetType1, _name1, _init1))).

action(delete(field(_id, _class, _RetType, _name, _init))) :-
%    action_all(delete(bodyFact(_id))),
    delete(fieldDefT(_id,  _class, _RetType, _name, _init)),
    removeFromClass(_class, _id).


cond(param(_id, _method, _type, _name)).

param(_id, _method, _type, _name) :-
    paramDefT(_id, _method, _type, _name),
    method(_method,_,_,_,_,_,_).
    
action(add(param(_id, _method, _type, _name))) :-
    add(paramDefT(_id, _method, _type, _name)),
    addToMethodArgs(_id, _method).
    
action(replace(param(_id, _method, _type, _name))) :-
    action(replace(paramDefT(_id, _method, _type, _name))).
    
action(delete(param(_id, _method, _type, _name))) :-
    delete(paramDefT(_id, _method, _type, _name)),
    removeFromMethodArgs(_id, _method).


/************************ MethodBody Facts *****************************/

cond(localVar(_id, _parent, _encl, _type, _name, _init)).
subTreeArg(localVar, 6).
localVar(_id, _parent, _encl, _type, _name, _init) :-
    localDefT(_id, _parent, _encl, _type, _name, _init),
    methodDefT(_encl, _, _, _, _, _, _),
    _encl \= _parent.
action(     localVar(_id, _parent, _encl, _type, _name, _init)) :-
    add(localDefT(_id, _parent, _encl, _type, _name, _init)),
    addToBlock(_parent, _id).
action(replace(localVar(_id, _parent, _encl, _type, _name, _init))) :-
    action(replace(localDefT(_id, _parent, _encl, _type, _name, _init))).
action(delete(localVar(_id, _parent, _encl, _type, _name, _init))) :-
%    action_all(delete(bodyFact(_id))),
    delete(localDefT(_id,_parent, _encl, _type, _name, _init)),
    removeFromBlock(_parent, _id).


cond(setField(_assignT, _parent, _encl, _Receiver, _field, _value)).
subTreeArg(setField, 4).
subTreeArg(setField, 6).

setField(SetField, RealParent, RealEncl, Receiver, Field,Value) :-
    assignT(SetField, Parent, Encl, GetField, Value),
    getFieldT(GetField, _, _, Receiver, _, Field),
    field(Field,_,_,_,_),
    getRealParent(SetField, Parent,RealParent),
    getRealEncl(SetField, Encl,RealEncl).
%    !,
%    nullIfThis(_receiver, _Receiver).

/*
setField(_setField, _parent, _encl, _Receiver, _field, _value) :-
    forwards(_setField,_,setField,_assign),
    applyT(_setField, _, _, _, [_receiver , _value]),
    getEncl(_setField, _encl),
    assignT(_assign, _,_, _identSelect, _value),
    nullIfThis(_receiver, _Receiver),
    getSymbol(_identSelect,_field).
    
*/

% TODO Port
action(add(setField(AssignT, Parent, Encl, Recv, Field, Value))) :-
    addToBlock(Parent, AssignT),
    field(Field,_,Type,_,_),
    new_id(GetField),
    add(getFieldT(GetField,AssignT,Encl,Recv,Type,Field)),
    add(assignT(AssignT, Parent, Encl, GetField, Value)).
    
action(replace(setField(AssignT, Parent, Encl, Recv, Field, Value))) :-
    field(Field,_,_,NewName,_),
    assignT(AssignT, Parent, Encl, GetField,Value),
    getFieldT(GetField,AssignT,Encl,OldRecv,OldName,OldField),
    replace(getFieldT(GetField,AssignT,Encl,OldRecv,OldName,OldField),
            getFieldT(GetField,AssignT,Encl,Recv,NewName,Field)),
    action(replace(assignT(AssignT, Parent, Encl, GetField, Value))).

action(replace(setField(AssignT, Parent, Encl, _Recv, _Field, Value),
	           setField(AssignT, Parent1, Encl1, Recv1, Field1, Value1))) :-
    field(Field1,_,_,Name1,_),
    assignT(AssignT, Parent, Encl, GetField,Value),
    getFieldT(GetField,AssignT,Encl,OldRecv,OldName,OldField),
    replace(getFieldT(GetField,AssignT,Encl,OldRecv,OldName,OldField),
            getFieldT(GetField,AssignT,Encl1,Recv1,Name1,Field1)),
    action(replace(assignT(AssignT, Parent1, Encl1, GetField, Value1))).

action(delete(setField(AssignT, Parent, Encl, Recv, Field, Value))) :-
    removeFromBlock(Parent, AssignT),
    assignT(AssignT, Parent, Encl, GetField, Value),
    getFieldT(GetField,AssignT,Encl,Recv,Name,Field),
    delete(getFieldT(GetField,AssignT,Encl,Recv,Name,Field)),
    delete(assignT(AssignT, Parent, Encl, GetField, Value)).

cond(getField(_getField, _parent, _encl, _Receiver, _name, _field)).
subTreeArg(getField, 4).

getField(_getField, _Parent, _Encl, _Receiver, _Name,_field) :-
    getFieldT(_getField, _parent, _encl, _receiver, _Name, _field),
    not(assignT(_parent, _,_,_getField,_)),
    getRealParent(_getField, _parent,_Parent),
    getRealEncl(_getField, _encl,_Encl),
    nullIfThis(_receiver, _Receiver).


action(add(getField(_identSelect, _parent, _encl, _recv, _name,_field))) :-
    field(_field,_,_,_name,_),
    addToBlock(_parent, _identSelect),
    add(getField(_identSelect, _parent, _encl, _name, _recv, _field)).
    
%    createIdentSelect(_identSelect, _parent, _encl, _name, _recv, _field).
action(replace(getField(_getField, _parent, _encl, _recv, _field))) :-
    field(_field,_,_,_name,_),
    deleteTree(_getField),
    add(getField(_getField, _parent, _encl, _recv, _field)).
%    replaceIdentSelect(_identSelect, _parent, _encl, _name, _recv, _field).
    
    

action(delete(getField(GetField, _, _, _, _, _))) :-
	getField(GetField, Parent, Encl, Recv, Name ,Field),
    removeFromBlock(_parent, _identSelect),
    delete(getField(GetField, Parent, Encl, Recv, Name ,Field)).


/* execution: entspricht methode*/
cond(execution(_execution, _class, _execution, null, _execution, _params)).
subTreeArg(execution, 6).
execution(_execution, _class, _execution, null, _execution, _params) :-
    methodDefT(_execution, _class, _, _params, _, _exceptions, _),
    not(externT(_class)).


cond(methodCall(_apply, _parent, _encl, _Receiver, _Name, _OrigMethod, _Args)).
subTreeArg(methodCall, 4).
subTreeArg(methodCall, 7).

/* 
   methodCall(?Call, ?Parent, ?Encl, ?Receiver, ?Method, ?Args)

	Wrapper predicate for applyT.
	and newClassT
	Method is the original method, 
	even if apply points to a forwarding method */

methodCall(_methodCall, _parent, _Encl, _Receiver, _method_name, _method, _Args) :-
    applyT(_methodCall, _parent, _encl, _Receiver, _method_name, _args,_method),
    not(_method_name == 'super'),
    not(forwarding(_methodCall)),
%    pc_visible(_encl),
    getRealParent(_methodCall,_parent,_Parent),
    getRealEncl(_methodCall, _encl,_Encl),
    getRealArgs(_methodCall,_args,_Args).

methodCall(_methodCall, _parent, _Encl, null, '<init>', _constructor, _Args) :-
    newClassT(_methodCall, _parent, _encl, _constructor, _args, _typeExpr, _def, _enclosingClass),
%    _Receiver, _method_name, _args,_method),
    not(forwarding(_methodCall)),
%    pc_visible(_encl),
    getRealParent(_methodCall,_parent,_Parent),
    getRealEncl(_methodCall, _encl,_Encl),
    getRealArgs(_methodCall,_args,_Args).

%    !,
%    getEncl(_methodCall, _encl),
%    getReceiverNullIfThis(_identSelect, _Receiver),
%    getSymbol(_identSelect, _method).

/** Uwe TODO does NOT handle forwarding methods */
action(methodCall(_apply, _parent, _encl, _recv, _name, _method, _Args)) :-
    methodDefT(_method, _, _name, _, _, _, _),
    addToBlock(_parent, _apply),
    new_id(_newIdSelect),
%    createIdentSelect(_newIdSelect, _apply, _encl, _name, _recv, _method),
    add(applyT(_apply, _parent, _encl, _recv,_name, _Args, _method)).

action(replace(methodCall(_apply, _parent, _encl, _recv, _name, _method, _args))) :-
	applyT(_apply, _, _, _,_name, _, _),
    enclMethod(_apply, _e),
    deleteTree(_apply),
    methodDefT(_method, _, _name, _, _, _, _),
    new_id(_newIdSelect),
    add(applyT(_apply, _parent, _encl, _recv,_name, _args, _method)).

%    format("replace start ~a,~a,~a,~a,~a,~a~n", [_newIdSelect, _apply, _encl, _name, _recv, ]),
%    action(replaceDiffTree(applyT(_apply, _parent, _encl, _newIdSelect, _Args))).
%    format("replace end ~a,~a,~a,~a,~a,~a~n", [_newIdSelect, _apply, _encl, _name, _recv, ]).
%    action(replace(applyT(_apply, _parent, _encl, _newIdSelect, _Args))).

action(delete(methodCall(_apply, _, _, _, _, _, _))) :-
    removeFromBlock(_parent, _apply),
    delete(applyT(_apply, _, _, _, _, _, _)).


cond(newCall(_newClass, _parent, _encl, _Receiver, _constructor, _args)).
subTreeArg(newCall, 4).
subTreeArg(newCall, 6).
newCall(_newClass, _parent, _encl, _Receiver, _constructor, _args) :-
    newClassT(_newClass, _parent, _encl, _method, _args, _identSelect, _, _),
    getReceiver(_identSelect, _Receiver),
    constructor(_constructor, _, _args, _, _).
action(newCall(_newClass, _parent, _encl, _recv, _constructor, _args)) :-
    addToBlock(_parent, _newClass),
    add(newClassT(_newClass, _parent, _encl, _constructor, _args, _recv, 'null', 'null')).
action(replace(newCall(_newClass, _parent, _encl, _recv, _constructor, _args))) :-
    action(replace(newClassT(_newClass, _parent, _encl, _constructor, _args, _recv, 'null', 'null'))).
action(delete(newCall(_newClass, _parent, _encl, _recv, _constructor, _args))) :-
    removeFromBlock(_parent, _newClass),
    delete(newClassT(_newClass, _parent, _encl, _method, _args, _recv, _, _)).


%cond(sout(_id, _pid, _encl, _args)).

action(sout(_apply, _pid, _encl, _str)) :-

    stringType(_t),

    packageT(_syspid, 'java.lang'),
    classDefT(_sysID, _syspid, 'System', _),
%    classDefT(_sysid3, _syspid, 'String', _),
    fieldDefT(_outID, _sysID, type(class, _sysid2, 0), 'out', _),

    packageT(_syspid2, 'java.io'),
    classDefT(_sysid2, _syspid2, 'PrintStream', _),
    methodDefT(_println, _sysid2, 'println', [_param1], _, _, _),
    paramDefT(_param1, _meth, _t, _),

    
    new_id(_get),new_id(_ident),

    add(applyT(__apply, _pid , _encl, _get, 'println',[_str],_println)),
    add(getFieldT(_get , _apply, _encl, _ident, 'out', _outID)),
    add(identT(_ident , _get, _encl, 'System', _sysID)).

%    add(operationT(_op, _id, _encl, [_lit, _arg], '+',
%    add(literalT(_lit, _id, _encl, type(class, _sysid3, 0), _str)),

action(str(_id, _pid, _encl, _str)) :-
    stringType(_t),
    add(literalT(_lit, _id, _encl, _t, _str)).

stringType(type(class, _id, 0)) :-
    stringClass(_id).
stringClass(_ID) :-
    packageT(_syspid, 'java.lang'),
    classDefT(_ID, _syspid, 'String', _).               


action(add_to_class(_class,_elem)) :-
    add_to_class(_class,_elem).
   
action(add_body(_elem,_body)) :-
    add_body(_elem,_body).


action(set_visibility(_forwMethod, _type, _ref)):-
    set_visibility(_forwMethod, _type, _ref).

/* *** op actions ***
    a recursive run of set_parent / enclMethod
    MUST be executed after these actions.
    
    @deprecated
*/

cond(opAnd(_id,_p,_e,_arg1, _arg2)).
opAnd(_id,_p,_e,_arg1, _arg2) :-
    operationT(_id,_p,_e, [_arg1, _arg2], '&&', 0).
action(opAnd(_id,_p,_e,_arg1, _arg2)) :-
    add(operationT(_id,_p,_e, [_arg1, _arg2], '&&', 0)).
action(delete(opAnd(_id,_p,_e,_arg1, _arg2))) :-
    delete(operationT(_id,_p,_e, [_arg1, _arg2], '&&', 0)).

cond(opEq(_id,_p,_e,_arg1, _arg2)).
opEq(_id,_p,_e,_arg1, _arg2) :-
    operationT(_id,_p,_e, [_arg1, _arg2], '==', 0).
action(opEq(_id,_p,_e,_arg1, _arg2)) :-
    add(operationT(_id,_p,_e, [_arg1, _arg2], '==', 0)).
action(delete(opEq(_id,_p,_e,_arg1, _arg2))) :-
    delete(operationT(_id,_p,_e, [_arg1, _arg2], '==', 0)).

cond(opNotEq(_id,_p,_e,_arg1, _arg2)).
opNotEq(_id,_p,_e,_arg1, _arg2) :-
    operationT(_id,_p,_e, [_arg1, _arg2], '!=', 0).
action(opNotEq(_id,_p,_e,_arg1, _arg2)) :-
    add(operationT(_id,_p,_e, [_arg1, _arg2], '!=', 0)).
action(delete(opNotEq(_id,_p,_e,_arg1, _arg2))) :-
    delete(operationT(_id,_p,_e, [_arg1, _arg2], '!=', 0)).

cond(ident(_id,_p,_e, _sym)).
ident(_id,_p,_e,'true') :-
    identT(_id,_p,_e,'true', _sym),!.
ident(_id,_p,_e,'false') :-
    identT(_id,_p,_e,'false', _sym),!.
ident(_id,_p,_e,_sym) :-
    identT(_id,_p,_e,_, _sym), !.
action(ident(_id,_p,_e,'true')) :-
    add(identT(_id,_p,_e, 'true', 'null')), !,
    format('DEBUG Ident true: ~a, ~a, ~a, ~a, ~a ~n', [_id,_p,_e, 'true', 'null']).
action(ident(_id,_p,_e,'false')) :-
    add(identT(_id,_p,_e, 'false', 'null')), !,
    format('DEBUG Ident false: ~a, ~a, ~a, ~a, ~a ~n', [_id,_p,_e, 'false', 'null']).

action(ident(_id,_p,_e,_sym)) :-
%    format('DEBUG Ident class: ~a, ~a, ~a, ~a ~n', [_id,_p,_e, _sym]),
    classDefT(_sym, _, _,_),
    !,
    format('DEBUG Ident this: ~a, ~a, ~n', [_id,_p,_e, _sym]),
    add(identT(_id,_p,_e, 'this', _sym)).
action(ident(_id,_p,_e,_sym)) :-
%    format('DEBUG Ident 1: ~a, ~a, ~n', [_id, _sym]),
    getSymbolName(_sym, _symName),
    format('DEBUG Ident std: ~a, ~a, ~a, ~a, ~a~n', [_id,_p,_e, _symName, _sym]),
    add(identT(_id,_p,_e, _symName, _sym)).
action(delete(ident(_id,_p,_e,_sym))) :-
    delete(identT(_id,_p,_e, _, _sym)).


cond(opNot(_id,_p,_e,_arg)).
opNot(_id,_p,_e,_arg) :-
    operationT(_id,_p,_e, [_arg], '!', -1).
action(opNot(_id,_p,_e,_arg)) :-
    format('DEBUG onNot: ~a, ~a, ~n', [_id, _arg]),
    add(operationT(_id,_p,_e, [_arg], '!', -1)).
action(delete(opNot(_id,_p,_e,_arg))) :-
    delete(operationT(_id,_p,_e, [_arg], '!', -1)).


/********************** NO ACTIONS - START ***********************/

cond(inner(_id)).
inner(_id) :-
    class(_id,_parent,_),
    class(_parent,_,_).

cond(local(_id)).
local(_id) :-
    class(_id,_parent,_),
    (
      blockT(_parent,_,_,_);
      forLoopT(_parent,_,_,_,_,_,_)
    ).

cond(anonymous(_id)).
anonymous(_id)  :-
    class(_id,_parent, _),
    expression(_parent).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     add_params
%
%  erzeugt aus einer Parameterliste (1. arg) und einer Id (2. arg) ids
%  eine neue Liste von Parametern (paramDefT), deren Namen und Typ mit denen aus
%  _params ?bereinstimmen und deren umschlie?ende Methode
%  _method (3. arg) ist.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ??? abstraction(add_params([],[],_)).
action(add_params(_params,_ids,_method)):-
    add_params(_params,_ids,_method).

add_params([],[],_).
add_params([_param|_params],[_id| _ids],_method):-
    paramDefT(_param,_,_type,_name),
    add(paramDefT(_id,_method,_type,_name)),
    add_params(_params,_ids,_method).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     types_equal
%
%  pr?ft f?r zwei Listen ob der Typ der Elemente ?bereinstimmt.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

types_equal([],[]).
types_equal([_h1|_t1],[_h2|_t2]) :-
    getType(_h1,_type),
    getType(_h2,_type),
    types_equal(_t1,_t2).


abstraction(fullQualifiedName(_id, _Fqn)).

 /**
  * fullQualifiedName(?Id, ?Fqn)
  *
  * at least one of the arguments must be bound.
  */

fullQualifiedName(Id, Fqn) :-
	nonvar(Fqn),
	globalIds(Fqn,Id),
	!.
%	class(Id,_,_). %  to be sure the index is up to date
%   Tobias: DONT DO THIS!!!! At this time the index may NOT be up to date.
%   Especially when I use the java_fq abstraction !!! 

fullQualifiedName(_id, Fqn) :-
    nonvar(_id),
    classDefT(_id, null, Fqn,_).

fullQualifiedName(_id, _Fqn) :-
    nonvar(_id),
    classDefT(_id, _parent, _name,_),
    packageT(_parent, _pckgname),
    stringAppend(_pckgname, '.', _name, _Fqn),
    !.
	
	
fullQualifiedName(_id, _Fqn) :-
    classDefT(_id, _parent, _name,_),
    packageT(_parent, _pckgname),
    !,
%    not(_pckgname == 'null'),
%    !,
    stringAppend(_pckgname, '.', _name, _Fqn).

% enclosing class is anomynous
fullQualifiedName(_id, _Fqn) :-
    classDefT(_id, _parent, _name,_),
    classDefT(_parent, _, _,_),
%    !,
    fullQualifiedName(_parent, _OuterFqn),
    stringAppend(_OuterFqn, '.', _name, _Fqn).

fullQualifiedName(_id, _Fqn) :-
    classDefT(_id, _parent, _name,_),
    not(classDefT(_parent, _, _,_)),
%    !,
    enclClass(_parent,ParentClass),
    fullQualifiedName(ParentClass, _OuterFqn),
    stringAppend(_OuterFqn, '.', _name, _Fqn).

fullQualifiedName(_id, _name) :-
    classDefT(_id, 'null', _name,_),
    !.


/**
 * fullQualifiedNames(?ExceptionClassIdList,?ExceptionNameList)
 * 
 * at least one of the arguments must be bound.
 */

fullQualifiedNames([],[]).
fullQualifiedNames([Exception|Exceptions], [ExceptionName|ExceptionNames]) :-
    fullQualifiedName(Exception,ExceptionName),
    fullQualifiedNames(Exceptions,ExceptionNames).


/*
  fullPathOfClass(?Id, ?Path)
*/

fullPathOfClass(_id, _Fqn) :-
    classDefT(_id, _parent, _name,_),
    packageT(_parent, _pckgname),
    packagePath(_pckgname,PckgPath),
%    not(_pckgname == 'null'),
%    !,
    stringAppend(PckgPath, '/', _name, _Fqn).

fullPathOfClass(_id, _Fqn) :-
    classDefT(_id, _parent, _name,_),
    classDefT(_parent, _, _,_),
%    !,
    fullQualifiedName(_parent, _OuterFqn),
    stringAppend(_OuterFqn, '$', _name, _Fqn).

fullPathOfClass(_id, _name) :-
    classDefT(_id, 'null', _name,_),
    !.

/*
	packagePath(+Pckgname,-PckgPath)
*/

packagePath(Pckgname,PckgPath) :-
    atom_concat(First,'.',RestName, Pckgname),
    !,
  	packagePath(RestName,RestPath),
    atom_concat(First,'/',Rest1),
    atom_concat(Rest1, RestPath, PckgPath).
  

packagePath(Name,Name).
    
test(packagePath):-
    packagePath('pckg1.pckg2.Name','pckg1/pckg2/Name').
/**************************** tests *********************/
test(fullQualifiedName1):-
    fullQualifiedName(c1,'Test').

setUp(fullQualifiedName1) :- setUpFQN.
tearDown(fullQualifiedName1) :- tearDownFQN.
    
test(fullQualifiedName2):-
    fullQualifiedName(c1,'pckg.Test').
setUp(fullQualifiedName2) :- setUpFQN.
tearDown(fullQualifiedName2) :- tearDownFQN.


setUpFQN :-
    assert(classDefT(c1,p1,'Test',[])),
    assert(packageT(p1,'pckg')),
    assert(classDefT(c1,null,'Test',[])).
    
tearDownFQN :-
    retract(classDefT(c1,p1,'Test',[])),
    retract(packageT(p1,'pckg')),
    retract(classDefT(c1,null,'Test',[])).
	    



%fullQualifiedName(TypeID, Fqn) :-
%    globalIds(Fqn,TypeID),
%    printf(Fqn),
%    printf(' ').

/************************ Helper ***************************/


enclMethodOrNull(_tree, _enclMethod) :-
    enclMethod(_tree, _enclMethod),
    !.
enclMethodOrNull(_tree, 'null').


createReturnOrExec(_parent, _encl, type(basic, void, 0), _stat, _exec) :-
    !,
    add(execT(_exec, _parent, _encl, _stat)).

createReturnOrExec(_parent, _encl, _type, _stat, _return) :-
    add(returnT(_return, _parent, _encl, _stat)).
