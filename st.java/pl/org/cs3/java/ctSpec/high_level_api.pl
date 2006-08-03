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
add_to_class                (_class, _id)
remove_from_class           (_class, _id)
cloneVarDef                 (_newParent, _varDef, _Copy)
createVarDefIdents          (_newParent, _oldList, _newList)
*/

%:- ['garbage_collect.pl'].

:- dynamic sourceLocation/4.
:- multifile sourceLocation/4.

%%% source location %%%
:- dynamic slT/3.
:- multifile slT/3.

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

:- dynamic isErrorWarningMessage/3.


/**
 * types(?ExprOrDeclarationList, ?typeTermList)
 */

cond(types(_Exprs,_Types)).

cond(src(_type)).

src(Type):-   
    not(java_fq(externT(Type))).



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

cond(type(_Name)).	
type(Name):-
    java_fq(classDefT(Name,_,_,_)).

cond(class(_Name)).	
class(Name):-
    java_fq(classDefT(Name,_,_,_)),
    not(java_fq(interfaceT(Name))).

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
    add_new_class_and_file(_id, _owner, _name, Defs).

/*
	sourceFolder(+Toplevel,-Sourcefolder)
	
	retrieves the source folder of the toplevel.
	If no source folder is specified the default
	source folder is bound.
*/

sourceFolder(Toplevel, ''):-
    projectLocationT(Toplevel,_Project,''),
    !.

sourceFolder(Toplevel, SourceFolder):-
    projectLocationT(Toplevel,_Project,SourceFolder),
    !.

/*
sourceFolder(Toplevel, Project):-
    projectLocationT(Toplevel,Project,''),
    !.

sourceFolder(Toplevel, SourceFolder):-
    projectLocationT(Toplevel,Project,Folder),
    atom_concat(Project,'/',Tmp),
    atom_concat(Tmp,Folder,SourceFolder),
    !.
*/
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


action(delete(class(_id, _owner, _name,_content))) :-
	action(delete(class(_id, _owner, _name))).
    

action(replace(class(_id, _owner, _name,_content),class(_id, _owner1, _name1,_content1))) :-
	action(replace(class(_id, _owner, _name),class(_id, _owner1, _name1))).

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



/**
 * subtype(?Sub, ?Super)
 *
 * Binds Super to any direct or 
 * indirect super type of Sub.
 * 
 * Only for object types.
 */

cond(subtype(_sub, _super)).

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
    remove_from_class(_class, _id).


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
    remove_from_class(_class, _id).


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
    remove_from_class(_class, _id).


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


action(add_to_class(_class,_elem)) :-
    add_to_class(_class,_elem).
   
action(add_body(_elem,_body)) :-
    add_body(_elem,_body).

action(remove_from_class(Class,Elem)) :-
	remove_from_class(Class,Elem).

action(set_visibility(_forwMethod, _type, _ref)):-
    set_visibility(_forwMethod, _type, _ref).

/*
        Part of the action/1 definition.
*/
action(replace(selectT(_id,_pid,_encl,_v1,_v2,_v3))):-        delete(selectT(_id,_,_,_,_,_)), !,        add(selectT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(identT(_id,_pid,_encl,_v1,_v2))):-             delete(identT(_id,_,_,_,_)), !,           add(identT(_id,_pid,_encl,_v1,_v2)).
action(replace(methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5))):-   delete(methodDefT(_id,_,_,_,_,_,_)), !,   add(methodDefT(_id,_pid,_v1,_v2,_v3,_v4,_v5)).
action(replace(localDefT(_id,_pid,_encl,_v1,_v2,_v3))):-      delete(localDefT(_id,_,_,_,_,_)), !,        add(localDefT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(fieldDefT(_id,_pid,_v1,_v2,_v3))):-              delete(fieldDefT(_id,_,_,_,_)), !,        add(fieldDefT(_id,_pid,_v1,_v2,_v3)).
action(replace(paramDefT(_id,_pid,_v1,_v2))):-                  delete(paramDefT(_id,_,_,_)), !,        add(paramDefT(_id,_pid,_v1,_v2)).
action(replace(classDefT(_id,_pid,_v1,_v2))):-                delete(classDefT(_id,_,_,_)), !,          add(classDefT(_id,_pid,_v1,_v2)).
action(replace(toplevelT(_id,_pid,_v1,_v2))):-                delete(toplevelT(_id,_,_,_)), !,          add(toplevelT(_id,_pid,_v1,_v2)).
action(replace(blockT(_id,_pid,_encl,_v1))):-                 delete(blockT(_id,_,_,_)), !,             add(blockT(_id,_pid,_encl,_v1)).
action(replace(doLoopT(_id,_pid,_encl,_v1,_v2))):-            delete(doLoopT(_id,_,_,_,_)), !,          add(doLoopT(_id,_pid,_encl,_v1,_v2)).
action(replace(whileLoopT(_id,_pid,_encl,_v1,_v2))):-         delete(whileLoopT(_id,_,_,_,_)), !,       add(whileLoopT(_id,_pid,_encl,_v1,_v2)).
action(replace(forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4))):-   delete(forLoopT(_id,_,_,_,_,_,_)), !,     add(forLoopT(_id,_pid,_encl,_v1,_v2,_v3,_v4)).
action(replace(labelT(_id,_pid,_encl,_v1,_v2))):-          delete(labelT(_id,_,_,_,_)), !,        add(labelT(_id,_pid,_encl,_v1,_v2)).
action(replace(switchT(_id,_pid,_encl,_v1,_v2))):-            delete(switchT(_id,_,_,_,_)), !,          add(switchT(_id,_pid,_encl,_v1,_v2)).
action(replace(caseT(_id,_pid,_encl,_v1))):-              delete(caseT(_id,_,_,_)), !,            add(caseT(_id,_pid,_encl,_v1)).
action(replace(synchronizedT(_id,_pid,_encl,_v1,_v2))):-      delete(synchronizedT(_id,_,_,_,_)), !,    add(synchronizedT(_id,_pid,_encl,_v1,_v2)).
action(replace(tryT(_id,_pid,_encl,_v1,_v2,_v3))):-           delete(tryT(_id,_,_,_,_,_)), !,           add(tryT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(catchT(_id,_pid,_encl,_v1,_v2))):-             delete(catchT(_id,_,_,_,_)), !,           add(catchT(_id,_pid,_encl,_v1,_v2)).
action(replace(ifT(_id,_pid,_encl,_v1,_v2,_v3))):-            delete(ifT(_id,_,_,_,_,_)), !,            add(ifT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(conditionalT(_id,_pid,_encl,_v1,_v2,_v3))):-   delete(conditionalT(_id,_,_,_,_,_)), !,   add(conditionalT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(execT(_id,_pid,_encl,_v1))):-                  delete(execT(_id,_,_,_)), !,              add(execT(_id,_pid,_encl,_v1)).
action(replace(returnT(_id,_pid,_encl,_v1))):-                delete(returnT(_id,_,_,_)), !,            add(returnT(_id,_pid,_encl,_v1)).
action(replace(breakT(_id,_pid,_encl,_v1,_v2))):-             delete(breakT(_id,_,_,_,_)), !,           add(breakT(_id,_pid,_encl,_v1,_v2)).
action(replace(continueT(_id,_pid,_encl,_v1,_v2))):-          delete(continueT(_id,_,_,_,_)), !,        add(continueT(_id,_pid,_encl,_v1,_v2)).
action(replace(throwT(_id,_pid,_encl,_v1))):-                 delete(throwT(_id,_,_,_)), !,             add(throwT(_id,_pid,_encl,_v1)).
action(replace(applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4))):-             delete(applyT(_id,_,_,_,_,_,_)), !,           add(applyT(_id,_pid,_encl,_v1,_v2,_v3,_v4)).
action(replace(newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5))):-!, delete(newClassT(_id,_,_,_,_,_,_,_)), !,  add(newClassT(_id,_pid,_encl,_v1,_v2,_v3,_v4,_v5)).
action(replace(newArrayT(_id,_pid,_encl,_v1,_v2,_v3))):-  delete(newArrayT(_id,_,_,_,_,_)), !,    add(newArrayT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(assignT(_id,_pid,_encl,_v1,_v2))):-            delete(assignT(_id,_,_,_,_)), !,          add(assignT(_id,_pid,_encl,_v1,_v2)).
action(replace(assignopT(_id,_pid,_encl,_v1,_v2,_v3))):-      delete(assignopT(_id,_,_,_,_,_)), !,      add(assignopT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(operationT(_id,_pid,_encl,_v1,_v2,_v3))):-     delete(operationT(_id,_,_,_,_,_)), !,     add(operationT(_id,_pid,_encl,_v1,_v2,_v3)).
action(replace(typeCastT(_id,_pid,_encl,_v1,_v2))):-          delete(typeCastT(_id,_,_,_,_)), !,        add(typeCastT(_id,_pid,_encl,_v1,_v2)).
action(replace(typeTestT(_id,_pid,_encl,_v1,_v2))):-          delete(typeTestT(_id,_,_,_,_)), !,        add(typeTestT(_id,_pid,_encl,_v1,_v2)).
action(replace(indexedT(_id,_pid,_encl,_v1,_v2))):-           delete(indexedT(_id,_,_,_,_)), !,         add(indexedT(_id,_pid,_encl,_v1,_v2)).
action(replace(literalT(_id,_pid,_encl,_v1,_v2))):-           delete(literalT(_id,_,_,_,_)), !,         add(literalT(_id,_pid,_encl,_v1,_v2)).
action(replace(assertT(_id,_pid,_encl,_v1,_v2))):-           delete(assertT(_id,_,_,_,_)), !,         add(assertT(_id,_pid,_encl,_v1,_v2)).
action(replace(importT(_id,_pid,_v1))):-                      delete(importT(_id,_,_)), !,              add(importT(_id,_pid,_v1)).
action(replace(_tree)) :-
    !,
    arg(1, _tree, _id),
    deleteTree(_id),
    add(_tree).

action(replaceDiffTree(_tree)) :-
    !,
    arg(1, _tree, _id),
  %  format('rdt: ~a ~a', [_id, _tree]),
    deleteTree(_id),
    add(_tree).


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
    classDefT(_id, null, Fqn,_),
    !.

fullQualifiedName(_id, _Fqn) :-
    nonvar(_id),
    classDefT(_id, _parent, _name,_),
    packageT(_parent, _pckgname),
    stringAppend(_pckgname, '.', _name, _Fqn),
    !.
	
/*	
fullQualifiedName(_id, _Fqn) :-
    classDefT(_id, _parent, _name,_),
    packageT(_parent, _pckgname),
    !,
%    not(_pckgname == 'null'),
%    !,
    stringAppend(_pckgname, '.', _name, _Fqn).
*/

fullQualifiedName(_id, _Fqn) :-
    classDefT(_id, _parent, _name,_),
    classDefT(_parent, _, _,_),
%    !,
    fullQualifiedName(_parent, _OuterFqn),
    stringAppend(_OuterFqn, '.', _name, _Fqn).

% enclosing class is anomynous
fullQualifiedName(_id, _Fqn) :-
    classDefT(_id, _parent, _name,_),
    not(classDefT(_parent, _, _,_)),
    not(packageT(_parent,_)),
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




%fullQualifiedName(TypeID, Fqn) :-
%    globalIds(Fqn,TypeID),
%    printf(Fqn),
%    printf(' ').

    
/**
 * sourceLocation(?ID, ?File, ?Begin, ?Length)
 *
 * Binds File to the defining file and Begin and Lenght to the position of Pef.
 */
sourceLocation(Tree,File,Start,End):-
    slT(Tree,Start,End),
    !,
    getToplevel(Tree,TL),
    toplevelT(TL,_,File,_).
% temporary necessary while aspects are not 
% completely represented in prolog factbase
sourceLocation(ID,File,Start,Length):-
        slAspectT(ID, File,Start, Length).

/*********************************
 * ID generation.                *
 *********************************/
    
/**
 * new_id(-Id)
 *
 * Binds Id to a unique number.
 * Throws already_bound_exception(Msg)
 * if Id is already bound.
 *
 */     

new_id(New) :-
        nonvar(New),
        New = [_|_],
        !,
        term_to_atom(New,Term),
        sformat(Msg,'new_id: variable is a list: ~a~n',Term),
        debugme,
        print(Msg),
        flush_output,
        throw(already_bound_exception(Msg)).
        
new_id(New) :-
        nonvar(New),
        !,
        sformat(Msg,'new_id: variable already bound: ~w~n',[New]),
        print(Msg),
        flush_output,
        throw(already_bound_exception(Msg)).

new_id(_New) :-
        findall(ID,lastID(ID),[H|[H2|T]]),
        !,
        sformat(Msg,'more than one lastID fact: ~w~n',[[H|[H2|T]]]),
        print(Msg),
        flush_output,
        throw(more_than_one_fact_exception(Msg)).

new_id(_New) :-
    lastID(_last),
    sum(_last, 1, _New),
    retract(lastID(_last)),
    assert(lastID(_New)).

/**
 * initLastID
 * adds the fact lastID(10000) if no lastID fact exists.
 *
 * Do not expose: internal predicate for new_id/1!
 */

:- dynamic lastID/1.

initLastID :-
  lastID(_),
  !.
initLastID :-
  assert(lastID(10000)).
  
:- initLastID.


/*************************************************************************************
 * Keep the default actions add/1, delete/1 and replace/2 predicates at the end of the file!
 * Otherwise the more specialized predicates will NOT be used and the operations
 * will typically try to change a static predicate.
 *************************************************************************************
 */

action(add(_elem)):-
        add(_elem).
         
action(delete(_elem)):-
        delete(_elem).
        
action(replace(_elem1,_elem2)):-
        replace(_elem1,_elem2).

action(replace(_elem)):-
    replace(_elem).

