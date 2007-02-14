:- style_check(-atom).
% Author:
% Date: 01.08.2002

%


java_code_generator_memory_stream_key('java_code_generator_memory_stream_key').

/**
 * general helper
 */

:- dynamic sourcePath/1.
:- dynamic sourceFolder/1.
:- dynamic align/1.
:- dynamic gen_classfile_names/1.
:- dynamic add_synthetic_methods/0.
:- multifile gen_tree_before/1.
:- multifile gen_class_after/1.

/** 
 * gen_tree/1 can be extended by a 
 * generator for a Java extension (e.g. LogicAJ).
 */
:- multifile gen_tree/1.

/**
 * Public API
 * generate_synthetic_methods(+Bool)
 * 
 * (De-)/Activate the generation of synthetic methods.
 * The argument 'Bool' can be true or fail.
 */
generate_synthetic_methods(Generate) :-
    Generate -> 
   (
      not(add_synthetic_methods) ->
      assert(add_synthetic_methods);
      true
   );
   retractall(add_synthetic_methods).


/***********************************************************
 * gen_tree for every possible kind of tree
 ***********************************************************/

/**
 * gen_all_toplevels
 *
 * Write all toplevels to the current output project (outdir).
 * Uses gen_toplevel/1.
 */
 gen_all_toplevels :-
    close_all_printf_to_memory,
  	retractall(sourcePath(_)),
    forall(toplevelT(_id,_,_,_), gen_toplevel(_id)).
/**
 * gen_toplevels
 *
 * Write all modified toplevels to the current output project (outdir)
 * and delete all deleted files.
 * Uses gen_toplevel/1.
 */
gen_toplevels :-
    close_all_printf_to_memory,
  	retractall(sourcePath(_)),
    forall(sorted_modified_toplevels(Toplevel), gen_toplevel(Toplevel)),
    delete_deleted_files,
    retract_api_meta_data.

delete_deleted_files :-
    forall(deleted_file(File),
       catch((format('deleting file: ~w~n',[File]),delete_file(File))
          ,Exception,write(Exception))).
    
%    close(_fileStream),
%    retract(gen_classfile_names(_fileStream)).

/**
 * gen_toplevel(+ToplevelId)
 *
 * Write the toplevel ToplevelId to the current ouput project (outdir/1).
 * The source folders will be preserved.
 */

gen_toplevel(Toplevel) :-
    toplevelT(Toplevel, _packg, _filename, _defs),
    !,
    file_on_disk(Toplevel,_dirfile),
    retractall(align(_)),
    assert(align(0)),
	(
	   sourceFolder(Toplevel,SourcePath) ->
	    assert1T(sourcePath(SourcePath));true),
%    (add_to_gen_classfile_names(_dirfile);true),
    createDirsIfNeeded(_dirfile),
    write_if_fileoutput(_dirfile),
    open(_dirfile, write, _fileStream),
    retractall(file_output(_)),
    assert(file_output(_fileStream)),
    retractall(align(_)),
    assert1(align(0)),
    gen_tree(_packg),
    gen_stats(_defs),
    retract(file_output(_fileStream)),
    close(_fileStream).


/*****************************************
 *********  local predicates *************
 *****************************************/



align(0).

throwMsg(Msg,Element):-
    term_to_atom(Element,Atom),
    sformat(S,Msg,Atom),
    throw(S).

indent :-
    align(_align),
    plus(_align, 3, _NextAlign),
    retractall(align(_)),
    assert1(align(_NextAlign)).

undent :-
    align(_align),
    plus(_align, -3, _NextAlign),
    retractall(align(_)),
    assert1(align(_NextAlign)).

printAlign_if_not_a_block(_body) :-
    blockT(_body,_,_,_),
    !.

printAlign_if_not_a_block(_body) :-
    printf('~n'),
    printAlign,
    printAlignRec(3).
    

printAlign :-
    align(_n),
    printAlignRec(_n).
printAlignRec(0).
printAlignRec(_n) :-
    _n > 0,
    printf(' '),
    succ(_nminus1, _n),
    printAlignRec(_nminus1).

printfa(_str, _args) :-
    printAlign,
    printf(_str, _args).
printfa(_str) :-
    printAlign,
    printf(_str, []).

gen_komma_list_inits([]).
gen_komma_list_inits([_head|_tail]) :-
    gen_non_exec(_head),
   %% gen_comma_if_not_empty(_tail),
    gen_komma_list_inits_rec(_tail).
 
	


gen_comma_if_not_empty([]).
gen_comma_if_not_empty([_|_]):-
    printf(', ').    
    
gen_komma_list_inits_rec([]).
gen_komma_list_inits_rec([_H | _T]) :-
    localDefT(_H, _, _, _, _name, _init),
    printf(', '),
    printf('~w',[_name]),
    gen_init(_init),
    gen_komma_list_inits_rec(_T).

gen_komma_list_inits_rec([_H | _T]) :-
    fieldDefT(_H,_, _, _name, _init),
    printf(', '),
    printf(' ~w',[_name]),
    gen_init(_init),
    gen_komma_list_inits_rec(_T).


gen_komma_list([]).
gen_komma_list([_H | _T]) :-
    gen_non_exec(_H),
    gen_komma_list_rec(_T).
gen_komma_list_rec([]).
gen_komma_list_rec([_H | _T]) :-
    printf(', '),
    gen_non_exec(_H),
    gen_komma_list_rec(_T).
gen_non_exec(_H) :-
    execT(_H, _,_, _expr),
    gen_tree(_expr).
gen_non_exec(_H) :-
    not(execT(_H, _, _,_)),
    gen_tree(_H).

gen_list([]).
gen_list([_H | _T]) :-
    gen_tree(_H),
    gen_list_rec(_T).
gen_list_rec([]).
gen_list_rec([_H | _T]) :-
    printf(' '),
    gen_tree(_H),
    gen_list_rec(_T).


gen_stats([]).
gen_stats([_H | _T]) :-
    printAlign,
    (
      (gen_tree(_H),!);
      gen_tree_error_location(_H)
    ),
    println,
    gen_stats(_T).

gen_type_list(_,[]).
gen_type_list(Where,[_type]) :-
    gen_type(Where,type(class,_type,0)),!.
gen_type_list(Where,[_head|_tail]) :-
    gen_type(Where,type(class,_head,0)),
    !,
    printf(', '),
    gen_type_list(Where,_tail).

gen_type_list(_,L) :-
    throwMsg('gen_type_list failed for List: ~w',L).

gen_type(Where,Type):-	
    type_name(Where,Type,Name),
    printf(Name).
    
gen_optional_default(null):-
    !.
gen_optional_default(Value):-
    printf(' default '),
    gen_tree(Value).
    
/*
gen_type_name(type(class, _typeID, _dim)) :-
    !,
    gen_type(_typeID).
gen_type_name(type(basic, _type, _dim)) :-
    !,
    printf(_type).

% local classes:
gen_type_name(_typeID) :-
    classDefT(_typeID,_parent,_name,_),
    execT(_parent,_,_,_),
    !,
    printf(_name).

gen_type_name(_typeID) :-
    fullQualifiedName(_typeID, _fqn),
    printf(_fqn).


gen_type(type(class, _typeID, _dim)) :-
    gen_type(_typeID),
    print_square_brackets(_dim).

gen_type(type(basic, _type, _dim)) :-
    printf(_type),
    print_square_brackets(_dim).
    
%    printf(' ').
gen_type(_typeID) :-
    classDefT(_typeID, _, _name, _),
    !,
    fullQualifiedName(_typeID, _fqn),
    !,
    printf(_fqn).
%    printf(' ').

gen_type(TypeID) :-
    throwMsg('can not find Type for id: ~w.',TypeID).
   */ 
gen_trees_in_square_brackets([]).
gen_trees_in_square_brackets([_H | _T]) :-
    printf('['),
    gen_tree(_H),
    printf(']'),
    gen_trees_in_square_brackets(_T).

gen_komma_list_in_curly_brackets('null').
gen_komma_list_in_curly_brackets([]) :-
    printf('{}').
gen_komma_list_in_curly_brackets([H | T]) :-
    printf('{'),
    gen_komma_list([H | T]),
    printf('}').


print_square_brackets(0) :-!.
print_square_brackets(_dim) :-
    printf('[]'),
    succ(_dimDec, _dim),
    print_square_brackets(_dimDec).

/**
 * gen_new_array_elemtype(+Scope,+Dims, +Elemtype, +Elems)
 */
gen_new_array_elemtype(_,_, 'null', _) :-!.

gen_new_array_elemtype(Scope,[], type(Kind,Ref,Dim), _elems) :-
    printf('new '),
    gen_type(Scope,type(Kind,Ref,Dim)),
	%%print_square_brackets(Dim), ld: covered in gen_type
	gen_komma_list_in_curly_brackets(_elems),
    !.

gen_new_array_elemtype(Scope,_dims, type(Kind,Ref,TotalDim), _) :-
    printf('new '),
    gen_type(Scope,type(Kind,Ref,0)),
    gen_trees_in_square_brackets(_dims),
	%% ld: the number of dimension expressions might be less than 
	%% the total number of dimensions.  
	%%We need to pad with empty brackets.
	%% @see roundtrip test 0011.
	%% @see MultiDimensionalArraysTest
	length(_dims,DimCount),
	Diff is TotalDim - DimCount,
	print_square_brackets(Diff).
	

gen_array_elems('null'):-!.

gen_array_elems(_elem) :-
    printf('[]').
/**
 * Helper
 */

gen_class(_id, _name) :-
	annotationTypeT(_id),
    printf('@interface ~w ',[_name]),
	gen_class_body(_id).

gen_class(_id, _name) :-
    not(interfaceT(_id)),
    !,
    gen_modifier(_id),
    printf('class ~w ',[_name]),
    gen_extends(_id),
    gen_implements(_id),
	gen_class_body(_id).


gen_class(_id, _name) :-
    interfaceT(_id),
    !,
    gen_modifier(_id),
    printf('interface ~w ',[_name]),
    gen_extends_interface(_id),
    classDefT(_id, _, _, _defs),

    %AOPHook instead of gen_block(_defs)
    printf(' {~n'),
    indent,
    gen_stats(_defs),
    (gen_class_after(_id);true),
    undent,
    printfa('}').


gen_class_body(_id):-
    classDefT(_id, _, _, _defs),
    
    %AOPHook instead of gen_block(_defs)
    printf(' {~n'),
    indent,
    gen_stats(_defs),
    (gen_class_after(_id);true),
    undent,
    printfa('}').

gen_method_body('null') :-
    !,
    printf(';~n').

gen_method_body(_body) :-
    gen_tree(_body).

gen_block(_list) :-
    printf(' {~n'),
    indent,
    gen_stats(_list),
    undent,
    printfa('}').

gen_modifier(_id) :-
    findall(_modifier, modifierT(_id, _modifier), []),!.
gen_modifier(_id) :-
    findall(_modifier, modifierT(_id, _modifier), _modifiers),
    filter_modifieres(_modifiers,_filtered),
    concat_atom(_filtered, ' ', _str),
    printf('~w ',[_str]).


filter_modifieres([],[]).

filter_modifieres(['synthetic' | _t],_T):-
    !,
    filter_modifieres(_t, _T).

filter_modifieres(['aspect' | _t],_T):-
    !,
    filter_modifieres(_t, _T).

filter_modifieres([_h | _t],[_h | _T]):-
    filter_modifieres(_t, _T).


gen_extends(_id) :-
    not(extendsT(_id, _)),
    !.
    
%FIXME: For Trans WS June 05
gen_extends(_id) :-
    findall(_extName, (extendsT(_id, _ext), fullQualifiedName(_ext, _extName), not(_extName='java.lang.Object')), List),%[_head|_tail]),
    List\=[]->
    (List=[_head|_tail],concat_atom([_head|_tail], ', ', _str),
    printf('extends ~w ',[_str]));true.

gen_implements(_id) :-
    not(implementsT(_id, _)),
    !.

gen_implements(_id) :-
    findall(_implName, (implementsT(_id, _impl), fullQualifiedName(_impl, _implName)), [_head|_tail]),
    concat_atom([_head|_tail], ', ', _str),
    printf('implements ~w ',[_str]).

gen_extends_interface(_id) :-
    findall(_impl, (implementsT(_id, _impl)), []).
gen_extends_interface(_id) :-
    findall(_implName, (implementsT(_id, _impl), fullQualifiedName(_impl, _implName)), [_head|_tail]),
    concat_atom([_head|_tail], ', ', _str),
    printf('extends ~w ',[_str]).


gen_exceptions(_,[]).
gen_exceptions(Method,[_head|_tail]) :-
    printf('throws '),
    methodDefT(Method,Class,_,_,_,_,_),
    gen_type_list(Class,[_head|_tail]).


gen_semicolon(_pid) :-
    classDefT(_pid, _, _,_),
    printf(';\n').
gen_semicolon(_pid) :-
    blockT(_pid, _,_,_),
    printf(';').
gen_semicolon(_pid) :-
    switchT(_pid, _,_,_,_),
    printf(';').

gen_semicolon( _).

gen_init('null').
gen_init(_init) :-
    printf(' = '),
    gen_tree(_init).



is_java_lang_string(_classID) :-
    fullQualifiedName(_classID, _fqn),
    equals(_fqn, 'java.lang.String').

is_java_lang_string(ClassID) :-
    globalIds('java.lang.String',ClassID).

gen_non_null(_, 'null') :- !.
gen_non_null(_str, _tree) :-
    printf(_str),
    gen_tree(_tree).

    /*
gen_method_name(_id) :-
    methodDefT(_id, _class, '<init>', _, _, _, _),
    !,
    class(_class, _, _name),
    printf(_name).

gen_method_name(_id) :-
    methodDefT(_id, _class, '<clinit>', _, _, _, _),
    !.
%    class(_class, _, _name),
%    printf(_name).

gen_method_name(_id) :-
    methodDefT(_id, _, _name, _, _, _, _),
%    not(equals(_name,'<init>')),
    printf(_name).
*/
gen_method_name_type(_id) :-
    methodDefT(_id, _class, '<init>', _, _, _, _),
    !,
%    write('constr'),
    class(_class, _, _name),
    printf(_name).

gen_method_name_type(_id) :-
    methodDefT(_id, _class, '<clinit>', _, _, _, _),
%    write('init'),
    !.
%    class(_class, _, _name),
%    printf(_name).

gen_method_name_type(_id) :-
    methodDefT(_id, Class, _name, _, _type, _, _),
%    write('method: '),
%    write(_name),

%    not(equals(_name,'<init>')),
    gen_type(Class,_type),
    printf(' '),
    printf(_name).

gen_finalizer('null') :-!.
gen_finalizer(_fin) :-
    printf(' finally'),
    gen_tree(_fin).

gen_label('null').
gen_label(_l) :-
    not(equals(_l, 'null')),
    printf(_l).


print_if_not_null(_test,_s) :-
    _test \= 'null' -> printf(_s) ; true.


/**
add_to_gen_classfile_names(_filename) :-
    gen_classfile_names(_fileStream),
    format(_fileStream, '~w~n',_filename).
*/


createDirsIfNeeded(_filename) :-
%    ground(_filename),
    file_directory_name(_filename, _dir),
    not(exists_directory(_dir)),
    concat_atom( _list, '/', _dir),
    working_directory(_current, _current),
    make_dirs_rec(_list),
    working_directory(_, _current).

createDirsIfNeeded(_filename) :-
%    ground(_filename),
    file_directory_name(_filename, _dir),
    exists_directory(_dir).

make_dirs_rec([]).
%FIX: for full unix outdir path names unix:
make_dirs_rec(['' | [_H|_T]]) :-
    atom_concat('/', _H,_rootH),
    make_dirs_rec([_rootH|_T]).
    
make_dirs_rec([_dirname | _T]) :-
    not(exists_directory(_dirname)),
    make_directory(_dirname),
    working_directory(_, _dirname),
    make_dirs_rec(_T).

make_dirs_rec([_dirname | _T]) :-
    exists_directory(_dirname),
    working_directory(_, _dirname),
    make_dirs_rec(_T).
% PREDEC - put operator before expr
gen_operation([_head | _], _,_name, -1) :-
    printf('~w',[_name]),
    gen_tree(_head).

% POSTDEC - put operator after expr
gen_operation([_head | _], _,_name, 1) :-
    gen_tree(_head),
    printf('~w',[_name]).

% else - put operator between exprs
gen_operation([_head | _tail], _parent, _name, 0) :-
%    printf_if_not_parent_assign(_parent, '('),
    gen_tree(_head),
%	ld: honour extended operands
%    gen_list(_tail).
	 gen_operation_rec(_tail, _parent,_name,0).
	 
gen_operation_rec([_head | _tail],_parent,_name,0):-
    printf(' ~w ',[_name]),
    gen_tree(_head),
    gen_operation_rec(_tail,_parent,_name,_0).
    
 gen_operation_rec([],_parent,_name,0).

%    printf_if_not_parent_assign(_parent, ')').

printf_if_not_parent_assign(_parent, _print) :-
    assignT(_parent, _,_,_,_),!.
printf_if_not_parent_assign(_parent, _print) :-
    printf(_print).


print_encl('null') :- !.
print_encl(_encl) :-
    gen_tree(_encl),
    printf('.').

print_anonymous_class('null') :- !.
print_anonymous_class(_def) :-
    gen_class_body(_def).

appendDir(OutDirNoSlash, Filename, Dirfile) :-
%    outdir(OutDirNoSlash),
    !,
    remove_leading_slash_if_needed(Filename,Clean_Filename),
    add_suffix_slash_if_needed(OutDirNoSlash,OutDir),
    stringAppend(OutDir, Clean_Filename, Dirfile).
appendDir(Filename, Filename).

remove_leading_slash_if_needed(Filename,Clean_Filename):-
    atom_concat('/',Clean_Filename,Filename).

remove_leading_slash_if_needed(Filename,Filename).

add_suffix_slash_if_needed(Filename,Filename):-
    atom_concat(_,'/',Filename).

add_suffix_slash_if_needed(Filename,Clean_Filename):-
    atom_concat(Filename,'/',Clean_Filename).

write_if_fileoutput(X) :-
    output_to_file,
    !,
    format('writing ~w~n',[X]).
write_if_fileoutput(_).


debug_this_line(_id) :-
    not(equals(_id, 128548)).

debug_this_line(128548) :-
    !,
    dummy_pred.

dummy_pred.

gen_left_bracket_if_parent_not_exec(_parent) :-
    execT(_parent, _,_,_), !.
gen_left_bracket_if_parent_not_exec(_parent) :-
    forLoopT(_parent, _,_,_, _, _, _), !.
gen_left_bracket_if_parent_not_exec(_parent) :-
    printf('(').

gen_right_bracket_if_parent_not_exec(_parent) :-
    execT(_parent, _,_,_), !.
gen_right_bracket_if_parent_not_exec(_parent) :-
    forLoopT(_parent, _,_,_, _, _, _), !.
gen_right_bracket_if_parent_not_exec(_parent) :-
    printf(')').

if_null_semicolon_else_gen_tree('null') :-
    !,
    printf(';~n').
if_null_semicolon_else_gen_tree(_body) :-
    gen_tree(_body).

:- dynamic lasttree/1.
lasttree(default).

gen_tree_list([], []).
gen_tree_list([Head|Tail], [Source|SourceTail]):-
	gen_tree(Head, Source),
	gen_tree_list(Tail, SourceTail).

gen_tree(_id, _source):-
   (
     java_code_generator_memory_stream_key(MEMORY_STREAM),
     open_printf_to_memory(MEMORY_STREAM),
     gen_tree(_id),
     !
   ; 
     true
   ),
   close_printf_to_memory(MEMORY_STREAM, _source).


gen_tree(ID):-
	gen_tree_before(ID),
	!.
	
gen_tree('null'):-
%    write('n'),
    !.

gen_tree(ID):-
    not(memberValueT(ID,_,_,_)),
	forall(annotationT(Annotation,ID,_,_,_),
	gen_tree(Annotation)),
	fail.

%gen_tree(_id):-
%    not(lasttree(_id)),
%    !,
%    retract(lasttree(_)),
%    assert(lasttree(_id)).


gen_tree(Toplevel) :-
    toplevelT(Toplevel, _packg, _filename, _defs),
    !,
    gen_tree(_packg),
    gen_stats(_defs).


gen_tree(_id) :-
    packageT(_id, _packName),
    !,
    printf('package ~w;~n~n', [_packName]).

gen_tree(_id) :-
    importT(_id, _pid, _class),
    classDefT(_class,_,_,_),
    fullQualifiedName(_class,_name),
    !,
    printf('import ~w;~n', [_name]).

gen_tree(_id) :-
    importT(_id, _pid, PackageId),
    packageT(PackageId,_name),
    !,
    printf('import ~w.*;~n', [_name]).

gen_tree(_id) :-
    classDefT(_id, _pid, _name, _),
%    write('DEBUG - class: '),
%    write(_name),
    !,
    gen_class(_id, _name).

gen_tree(_id) :-
    methodDefT(_id, _pid, '<clinit>', _args, _ret, _exc, _body),
    !,
    (
    	(
     	    not(add_synthetic_methods),
    		modifierT(_id,'synthetic'),
    		!
   	);(
    		gen_modifier(_id),
		    gen_method_body(_body),
		    println
		)
	).
		    

gen_tree(_id) :-
    methodDefT(_id, _pid, _, _args, _ret, _exc, _body),
%    debug_this_line(_id),
    !,
    (
%TODO: bad for aspects, advices on default constructors are ignored!
    	(
    	    not(add_synthetic_methods),
    		modifierT(_id,'synthetic'),
    		!
    	);(
	    	gen_modifier(_id),
		    gen_method_name_type(_id),
		    printf('('),
		    gen_komma_list(_args),
	    	printf(')'),
		    gen_exceptions(_id,_exc),
	    	gen_method_body(_body),
		    println
		)
	).

gen_tree(_id) :-
    paramDefT(_id, _pid, _type, _name),
    !,
    (	methodDefT(_pid,Scope,_,_,_,_,_)
    ;	catchT(_pid,_,Scope,_,_)
    ),
    gen_modifier(_id),
    gen_type(Scope,_type),
    printf(' ~w',[_name]),
    gen_semicolon(_pid).

gen_tree(_id) :-
    fieldDefT(_id, _pid, _type, _name, _init),
    !,
    (
    	(
    		modifierT(_id,'synthetic'),
    		!
    	);(
		    gen_modifier(_id),
			gen_type(_pid,_type),
		    printf(' ~w',[_name]),
		    gen_init(_init),
		    gen_semicolon(_pid)
		)
	).

gen_tree(_id) :-
    localDefT(_id, _pid, _, _type, _name, _init),
    !,
    gen_modifier(_id),
    gen_type(_id,_type),
    printf(' ~w',[_name]),
    gen_init(_init),
    gen_semicolon(_pid).

gen_tree(_ID) :-
    blockT(_ID, _,_ , _list),
    !,
    gen_block(_list).

gen_tree(_ID) :-
    assignT(_ID, _parent,_, _lhs, _rhs),
    !,
    gen_left_bracket_if_parent_not_exec(_parent),
    gen_tree(_lhs),
    printf(' = '),
    gen_tree(_rhs),
    gen_right_bracket_if_parent_not_exec(_parent).

gen_tree(_ID) :-
    nopT(_ID, _,_),
    !,
    printf(';').

gen_tree(_ID) :-
    assignopT(_ID, _parent,_, _lhs, _op, _rhs),
    !,
    gen_left_bracket_if_parent_not_exec(_parent),
    gen_tree(_lhs),
    printf(' '),
    printf(_op),
    printf('= '),
    gen_tree(_rhs),
    gen_right_bracket_if_parent_not_exec(_parent).

gen_tree(_ID) :-
    newArrayT(_ID, _p,Scope, _dims, _elems, _elemtype),
    !,
    gen_new_array_elemtype(Scope,_dims, _elemtype, _elems).
%    gen_komma_list_in_curly_brackets(_elems).

gen_tree(_ID) :-
    typeCastT(_ID, _parent,_, _type, _expr),
    !,
    printf_if_not_parent_assign(_parent, '('),
    printf('('),
    gen_type(_ID,_type),
    printf(')('),
    gen_tree(_expr),
    printf(')'),
    printf_if_not_parent_assign(_parent, ')').

gen_tree(_ID) :-
    typeTestT(_ID, _,_, _type, _expr),
    !,
    printf('('),
    gen_tree(_expr),
    printf(' instanceof '),
    gen_type(_ID,_type),
    printf(')').

gen_tree(_ID) :-
    indexedT(_ID, _,_, _index, _indexed),
    !,
    gen_tree(_indexed),
    printf('['),
    gen_tree(_index),
    printf(']').

gen_tree(_ID) :-
    selectT(_ID, _,_, _name, _selected, _),
    !,
    gen_tree(_selected),
    printf('.'),
    printf(_name).

gen_tree(_ID) :-
    getFieldT(_ID, _,_, _selected, _name, _),
    !,
    ( _selected = 'null' ->
	    true;
	    gen_tree(_selected),
	    printf('.')
	),
    printf(_name).

gen_tree(ID) :-
    identT(ID,_,_,Name,Symbol),
    !,
    gen_ident(Name,Symbol).

gen_tree(_ID) :-
    literalT(_ID,_,_,_type, _value),
    !,
    gen_literal(_type, _value).

gen_tree(_ID) :-
    execT(_ID, _,_, _expr),
    !,
    gen_tree(_expr),
    printf(';').

gen_tree(_ID) :-
    applyT(_ID, _,_, Recv,_name, _args,_),
    !,
    ( Recv = 'null' ->
	    true;
	    (gen_tree(Recv),
	    printf('.'))
	),
    printf(_name),
    printf('('),
    gen_komma_list(_args),
    printf(')').

gen_tree(_ID) :-
    ifT(_ID, _,_, _cond, _then, _else),
    !,
    printf('if('),
    gen_tree(_cond),
    printf(') '),
    gen_tree(_then),
    gen_non_null(' else ', _else).

gen_tree(_ID) :-
    conditionalT(_ID,_, _, _cond, _then, _else),
    !,
    printf('(('),
    gen_tree(_cond),
    printf(') ? '),
    gen_tree(_then),
    gen_non_null(' : ', _else),
    printf(')').

gen_tree(_ID) :-
    newClassT(_ID, _,_, _, _args, _clazzIdentSelect, _def, 'null'),
    !,
    printf('new '),
    getSymbol(_clazzIdentSelect,_clazz),
    gen_type(_ID,type(class,_clazz,0)),
    printf('('),
    gen_komma_list(_args),
    printf(')'),
    print_anonymous_class(_def).


gen_tree(_ID) :-
    newClassT(_ID, _,_, _, _args, _clazz, _def, _encl),
    !,
    print_encl(_encl),
    printf('new '),
    gen_tree(_clazz),
    printf('('),
    gen_komma_list(_args),
    printf(')'),
    print_anonymous_class(_def).

gen_tree( _ID) :-
    operationT(_ID, _parent,_, _args, _name, _pos),
    !,
    gen_operation(_args,_parent, _name, _pos).

gen_tree(_ID) :-
    doLoopT(_ID, _,_, _cond, _body),
    !,
    printf('do '),
    gen_tree(_body),
    printf(' while('),
    gen_tree(_cond),
    printf(');').

gen_tree(_ID) :-
    whileLoopT(_ID, _,_, _cond, _body),
    !,
    printf('while('),
    gen_tree(_cond),
    printf(')'),
    gen_tree(_body).

gen_tree(_ID) :-
    tryT(_ID, _,_, _body, _catcher, _finalizer),
    !,
    printf('try'),
    gen_tree(_body),
    gen_stats(_catcher),
    gen_finalizer(_finalizer).

gen_tree(_ID) :-
    catchT(_ID, _,_, _param, _body),
    !,
    printf('catch('),
    gen_tree(_param),
    printf(') '),
    gen_tree(_body).

gen_tree(_ID) :-
    assertT(_ID, _,_, _test, _msg),
    !,
    printf('assert '),
    gen_tree(_test),
    print_if_not_null(_msg,':'),
    gen_tree(_msg),
    printf(';').

gen_tree(_ID) :-
    forLoopT(_ID, _,_, _inits, _cond, _steps, _body),
    !,
    printf('for('),
    gen_komma_list_inits(_inits),
    printf(';'),
    gen_tree(_cond),
    printf(';'),
    gen_komma_list(_steps),
    printf(')'),
    printAlign_if_not_a_block(_body),
    if_null_semicolon_else_gen_tree(_body).

gen_tree(_ID) :-
    switchT(_ID, _,_, _selector, _cases),
    !,
    printf('switch('),
    gen_tree(_selector),
    printf(')'),
    gen_block(_cases).


gen_tree(_ID) :-
    caseT(_ID, _,_, _label),
    !,
    (
    	_label == 'null' ->
    	printf('default: ');
	    printf('case '),
	    gen_tree(_label),
	    printf(': ')
    ),
    println.
    
gen_tree(_ID) :-
    returnT(_ID, _,_, _exec),
    !,
    printf('return '),
    gen_tree(_exec),
    printf(';').

gen_tree(_ID) :-
    synchronizedT(_ID, _,_, _lock, _body),
    !,
    printf('synchronized('),
    gen_tree(_lock),
    printf(') '),
    gen_tree(_body).

gen_tree(_ID) :-
    breakT(_ID, _,_, _label, _),
    !,
    printf('break '),
    gen_label(_label),
    printf(';').

gen_tree(_ID) :-
    continueT(_ID, _,_, _label, _),
    !,
    printf('continue '),
    gen_label(_label),
    printf(';').

gen_tree(_ID) :-
    throwT(_ID, _,_, _exec),
    !,
    printf('throw '),
    gen_tree(_exec),
    printf(';').

gen_tree(_ID) :-
    labelT(_ID, _, _,_body, _name),
    !,
    printf('~w : ',[_name]),
    gen_tree(_body).

gen_tree(_ID) :-
	precedenceT(_ID,_,_,_expr),
	!,
	printf('('),
    gen_tree(_expr),
	printf(')').

gen_tree(ID):-
	annotationT(ID,_,_,Class,ValuePairs),
	printf('@'),
	fullQualifiedName(Class,Type),
	printf(Type),
	printf('('),
    gen_komma_list(ValuePairs),
	printf(')').

gen_tree(ID):-
	memberValueT(ID,_,Member,Value),
	annotationMemberT(Member,_,_,Name,_),
	!,
	printf(Name),
	printf(' = '),
    gen_tree(Value).

gen_tree(ID):-
	memberValueT(ID,_,null,Value),
    gen_tree(Value).

gen_tree(ID):-
    annotationMemberT(ID,AnnotationType,Type,Name,Default),
    !,
	gen_type(AnnotationType,Type),
	printf(' '),
	printf(Name),
	printf('() '),
	gen_optional_default(Default),
	printf(';').


gen_tree(_ID) :-
    not(tree(_ID,_,_)),
    !,
    prolog_current_frame(F),
    stack_for_frame(F,Infos),
    format('~nstack trace: ~n~w~n',[Infos]),
%    gen_tree_error_location(_ID),
%	prolog_choice_attribute(parent,Out),
%	prolog_choice_attribute(type,Out),
    throwMsg('~nERROR: cannot find tree with id ~w~n', _ID).

gen_tree_error_location(ID) :-
    prolog_current_frame(F0),
    prolog_frame_attribute(F0,parent,F), 
    stack_for_frame(F,Infos),
    sformat(S,'stack trace: ~w~n',[Infos]),
    throwMsg(S,ID).

%    prolog_frame_attribute(F,clause,Clause),
%    prolog_frame_attribute(F,goal,Goal),    %
%	clause(Head,Body,Clause),%
%	clause_property(Clause,file(File)),%
%	clause_property(Clause,line_count(Line)),
%	(
%	  prolog_frame_attribute(F,argument(1),Arg) ->
%	  true;Arg='no arguments'
%	),
%	format('error when executing goal: ~w~n with (first) argument: ~w~nat pos ~w:~w~nin clause: ~w :- ~w~n~n',[Goal,Arg,File,Line,Head,Body]).

gen_tree_error_location(_ID):-
	format('error occured while generating error report!~n',[]).



/**
 gen_literal(+typeTerm, +value)
 
 If typeTerm is a basic type the value is printed.
 If typeTerm is of type String type(class, <id of class java.lang.String>, 0)
 all occurances of '~' are replaced by '~~'. The preprocessed value
 is printed.
 If value is a type term (type/3) a class literal is printed.
 */
 

gen_literal(type(basic, char, _), _value) :-
    printf('\'~w\'',[_value]),
    !.

gen_literal(type(basic, _, _), Value) :-
    printf('~w', [Value]).

%% ld: just in case i wonder again: the second arg implies that the first one be class
gen_literal(_, type(class,ID,Dim)) :-
    fullQualifiedName(ID,Name),
    printf(Name),
    print_square_brackets(Dim),
    printf('.class').

gen_literal(_, type(basic,ValTypeName,Dim)) :-
    printf(ValTypeName),
    print_square_brackets(Dim),
    printf('.class').


gen_literal(type(class, _classID, 0), _value) :-
    is_java_lang_string(_classID),
    concat_atom(_list6, '~', _value),
    concat_atom(_list6, '~~', _value7),
    printf('\"~w\"',[_value7]).


/**
 * gen_ident(+Identifier, +Symbol)
 *
 * If the symbol is a class and Identifier is not super or this
 * the full qualified name is printed. Otherwise the identifier
 * is printed.
 */
gen_ident(this,_Symbol):-
    !,
    printf(this).

gen_ident(super,_Symbol):-
    !,
    printf(super).

gen_ident(_Name,Class) :-
    classDefT(Class,_,_,_),
    !,
    fullQualifiedName(Class,FQN),
    write(FQN),
    printf(FQN).

gen_ident(Name,_Symbol):-
    !,
    printf(Name).
