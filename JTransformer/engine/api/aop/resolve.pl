% Author: Tobias
% Date: 14.02.2003


resolve_field(type(class,_enclClass,0),_name,_Field):-
    resolve_field(_enclClass,_name,_Field).

resolve_field(_enclClass,_name,_Field):-
    fieldDefT(_Field,_enclClass,_,_name,_),
    !.

resolve_field(_enclClass,_name,_Field):-
    extendsT(_enclClass,_super),
    resolve_field(_super,_name,_Field),
    !.
    
resolve_field(_enclClass,_name,_Field):-
    implementsT(_enclClass,_interf),
    resolve_field(_interf,_name,_Field),
    !.

resolve_field(_enclClass,_name,_Field):-
    classDefT(_enclClass,_,_className,_),
    sformat(_msg,'field ''~a'' not found in class ''~a''',[_name,_className]),
    error_handling(fail, _msg).

abstraction(resolve_class(_name,_Class)).
resolve_class(_name,_Class):-
    fullQualifiedName(_Class, _name),
    !.
    
resolve_class(_name,_):-
    sformat(_msg,'class not found ~a',_name),
    error_handling(fail, _msg).

% so geht's auch:
%    atom_concat(_pckgname, _dotclassname, _name),
%    atom_concat('.', _classname, _dotclassname),
%    packageT(_pckg, _pckgname),
%    classDefT(_Class,_pckg,_classname,_).

resolve_class(_enclClass,_name,_Class):-
    getPackage(_enclClass,_pckg),
    lookupClassInPackage(_pckg,_name,_Class),
    !.
resolve_class(_enclClass,_name,_Class):-
    getToplevel(_classId, _tl),
    toplevelT(_tl, _pckg, _, _),
    importT(_import, _tl, _import),
    lookupClassInImport(_import,_name,_Class),
    !.
resolve_class(_enclClass,_name,_Class):-
    packageT(_pckg, 'java.lang'),
    lookupClassInPackage(_pckg,_name,_Class),
    !.
%TODO - check accessable
resolve_class(_,_name,_):-
    sformat(_msg,'class not found ~a',_name),
    error_handling(fail, _msg).

lookupClassInPackage(_pckg,_name,_Class):-
    classDefT(_Class,_,_name,_),
    getPackage(_Class,_pckg).

lookupClassInImport(_import,_name,_Class):-
    importT(_import, _, _Class),
    classDefT(_Class,_,_name,_).

lookupClassInImport(_import,_name,_Class):-
    importT(_import, _, _packagename),
    packageT(_pckg, _packagename),
    lookupClassInPackage(_pckg,_name,_Class).



resolve_method(type(class,_enclClass,0),_name,_args,_Meth):-
    resolve_method(_enclClass,_name,_args,_Meth).

resolve_method(_enclClass,_name,_args,_Meth) :-
    assert(best_method(null)),
    (
        resolve_candidate(_enclClass,_enclClass,_name,_args)
        ;
        true
    ),
    %TODO: Interface Methods ?!
    check_for_ambiguty(_enclClass,_enclClass,_name,_args),
    check_best_method(_enclClass,_name,_args,_Meth),
    !.
%    format('method found: ~a~n',_name).

check_best_method(_enclClass,_name,_args,_):-
    best_method(null),
    printToTmp,
    class(_enclClass,_package,_className),
    packageT(_package,_packageName),
    printf('method (<init> for constructor) not found: '),
    printf('~a.~a.~a(',[_packageName,_className,_name]),
    gen_komma_list(_args),
    printf(')'),
    closeTmp(_msg),
    error_handling(fail, _msg).

check_best_method(_enclClass,_,_,_Meth):-
    best_method(_Meth).

find_method(_enclClass, _class,_name,_args):-
    methodDefT(_meth,_class,_name,_params,_,_,_),
%    % compare resolve.java in javac
    method_invocation_conversion(_params, _args),
    best_method(_best),
    not(as_good_as(_enclClass,_best,_meth)),
    retractall(best_method(_)),
    assert(best_method(_meth)),
    fail.



resolve_candidate(_enclClass,_class,_name,_args):-
    (
        find_method(_enclClass, _class,_name,_args);
        true
    ),
    _name \= '<init>',
    extendsT(_class,_super),
    _class \= _super,
    !,
    resolve_candidate(_enclClass,_super,_name,_args).
    
check_for_ambiguty(_enclClass,_class,_name,_args):-
    best_method(_best),
    method(_best,_,_,_,_,_,_),
    method(_meth,_class,_name,_params,_,_,_),
    % compare
    method_invocation_conversion(_params, _args),
    not(as_good_as(_enclClass,_best,_meth)),
    sformat(_msg,'ambiguty error: method ~a',_name),
    error_handling(fail, _msg).

check_for_ambiguty(_,_,_,_).


as_good_as(_,null,_meth):-
    !,
    fail.


as_good_as(_enclClass, _best,_meth):-
    method(_meth,_class,_,_params,_,_,_),
    method(_best,_bestClass,_,_bestParams,_,_,_),
    !,(
        (
            not(is_accessible(_enclClass,_best)),
            is_accessible(_enclClass,_meth),
            modifierT(_meth, 'static'),
            modifierT(_best, 'static')

        );
        interfaceT(_bestClass);(
            subtype(_bestclass,_class),
            !,
            method_invocation_conversion(_params, _bestParams)
        )
   ).
   
%private
is_accessible(_class,_meth):-
    modifierT(_meth, 'private'),
    !,
    method(_meth,_class,_,_,_,_,_).

%public, protected
is_accessible(_class,_meth):-
    method(_meth,_enclClass,_,_,_,_,_),
    (modifierT(_meth, 'public') ; modifierT(_meth, 'protected')).
%package
is_accessible(_class,_meth):-
    method(_meth,_enclClass,_name,_params,_,_,_),
    getPackage(_class,_pckg),
    getPackage(_enclClass,_pckg).

method_invocation_conversion([], []).
method_invocation_conversion([_param|_paramRest], [_arg| _argRest]) :-
    getType(_param, type(_kink,_paramCompType,_arraySize)),
    getType(_arg, type(_kink,_argCompType,_arraySize)),
    subtype(_argCompType,_paramCompType),
    !,
    method_invocation_conversion(_paramRest, _argRest).
    


assignment_check(_lhs,_rhs) :-
    getType(_lhs,_ltype),
    getType(_rhs,_rtype),
    assignable(_ltype,_rtype),
    !.
assignment_check(_lhs,_rhs) :-
    getType(_lhs,_ltype),
    getType(_rhs,_rtype),
    write('cannot assign '),
    gen_type_name(_rtype),
    write(' to type '),
    gen_type_name(_ltype),
    format('~n'),
    error_handling(fail, 1).

assignable(type(basic,_t,_dim),type(basic,_t,_dim)) :- !.
assignable(type(class,_,_),type(basic,null,_)) :- !.
assignable(type(class,_lc,_dim),type(class,_rc,_dim)) :-
    subtype(_lc, _rc),
    !.



