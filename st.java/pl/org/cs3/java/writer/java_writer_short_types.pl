% Author:
% Date: 01.08.2002

%

/**
 * general helper
 */
:- dynamic align/1.

align(0).

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
    gen_tree(_H),
    println,
    gen_stats(_T).

gen_type_list(_pos, []).
gen_type_list(_pos, [_head|_tail]) :-
    gen_type(_pos, _head),
    gen_type_list(_pos, _tail).

gen_type(_pos, type(class, _typeID, _dim)) :-
    gen_type(_pos, _typeID),
    print_square_brackets(_dim).

gen_type(_, type(basic, _type, _dim)) :-
    printf(_type),
    print_square_brackets(_dim).
%    printf(' ').

gen_type(_pos, _typeID) :-
    (
    	importT(_, _top,_typeID);
	    (importT(_, _top,PID),classDefT(_typeID,PID,_,_))
	),
	!,
	class(_typeID,_,Name),
	printf(Name).

gen_type(_pos, _typeID) :-
%    classDefT(_typeID, _, _name, _),
    fullQualifiedName(_typeID, _fqn),
    enclToplevel(_pos, _top),
    printf(_fqn).
%    printf(' ').


enclToplevel(_tree, _tree) :-
    package(_tree, _),
    !,
    fail.
enclToplevel(_tree, _tree) :-
    toplevelT(_tree, _, _, _),
    !.
enclToplevel(_tree, _top) :-
    classDefT(_tree, _pack, _, _),
    packageT(_pack, _),
    !,
    toplevelT(_top, _, _, _members),
    member(_tree, _members).
enclToplevel(_tree, _top) :-
    enclClass(_tree, _class),
    _class \= null,
    !,
    enclToplevel(_class, _top).
enclToplevel(_tree, _top) :-
    tree(_tree, _parent, _),
    enclToplevel(_parent, _top).


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


print_square_brackets(0).
print_square_brackets(_dim) :-
    not(equals(_dim, 0)),
    printf('[]'),
    succ(_dimDec, _dim),
    print_square_brackets(_dimDec).

gen_new_array_elemtype(_, _, 'null', _, _).
gen_new_array_elemtype(_pos, _dims, _elemtype, _depth, _elems) :-
    not(equals(_elemtype, 'null')),
    printf('new '),
    gen_type(_pos, _elemtype),
    gen_trees_in_square_brackets(_dims),
    print_square_brackets(_depth),
    gen_array_elems(_elems).

gen_array_elems('null').

gen_array_elems(_elems) :-
    not(equals(_elems, 'null')),
    printf('[]').
/**
 * Helper
 */

gen_class(_id, _name) :-
    not(interfaceT(_id)),
    gen_modifier(_id),
    printf('class ~a ',[_name]),
    gen_extends(_id),
    gen_implements(_id),
    classDefT(_id, _, _, _defs),
    gen_block(_defs).


gen_class(_id, _name) :-
    interfaceT(_id),
    gen_modifier(_id),
    printf('interface ~a ',[_name]),
    gen_extends_interface(_id),
    classDefT(_id, _, _, _defs),
    gen_block(_defs).


gen_method_body('null') :-
    printf(';~n').

gen_method_body(_body) :-
    not(equals(_body, 'null')),
    gen_tree(_body).

gen_block(_list) :-
    printf(' {~n'),
    indent,
    gen_stats(_list),
    undent,
    printfa('}').

gen_modifier(_id) :-
    findall(_modifier, modifierT(_id, _modifier), []).
gen_modifier(_id) :-
    findall(_modifier, modifierT(_id, _modifier), [_h|_t]),
    concat_atom([_h|_t], ' ', _str),
    printf('~a ',[_str]).

gen_extends(_id) :-
    findall(_ext, (extendsT(_id, _ext)), []).
gen_extends(_id) :-
    findall(_extName, (extendsT(_id, _ext), fullQualifiedName(_ext, _extName)), [_head|_tail]),
    concat_atom([_head|_tail], ', ', _str),
    printf('extends ~a ',[_str]).

gen_implements(_id) :-
    findall(_impl, (implementsT(_id, _impl)), []).
gen_implements(_id) :-
    findall(_implName, (implementsT(_id, _impl), fullQualifiedName(_impl, _implName)), [_head|_tail]),
    concat_atom([_head|_tail], ', ', _str),
    printf('implements ~a ',[_str]).

gen_extends_interface(_id) :-
    findall(_impl, (implementsT(_id, _impl)), []).
gen_extends_interface(_id) :-
    findall(_implName, (implementsT(_id, _impl), fullQualifiedName(_impl, _implName)), [_head|_tail]),
    concat_atom([_head|_tail], ', ', _str),
    printf('extends ~a ',[_str]).


gen_exceptions(_, []).
gen_exceptions(_pos, [_head|_tail]) :-
    printf('throws '),
    gen_type_list(_pos, [_head|_tail]).


gen_semicolon(_pid) :-
    classDefT(_pid, _, _,_),
    printf(';\n').
gen_semicolon(_pid) :-
    blockT(_pid, _,_,_),
    printf(';').
gen_semicolon(_pid) :-
    caseT(_pid, _,_,_),
    printf(';').
gen_semicolon( _).

gen_init('null').
gen_init(_init) :-
    printf(' = '),
    gen_tree(_init).



is_java_lang_string(_ID) :-
    fullQualifiedName(_classID, _fqn),
    equals(_fqn, 'java.lang.String').

gen_literal(type(basic, 'float', _), _value) :-
    printf('~aF',[_value]).

gen_literal(type(basic, 'long', _), _value) :-
    printf('~aL',[_value]).

% a bisserl teuer, nicht wahr?! besser einen Type zum literal hinzufügen ?
gen_literal(type(basic, 'char', _), '\\') :-
    printf('\'\\\\\'').
gen_literal(type(basic, 'char', _), '\'') :-
    printf('\'\\\'\'').
gen_literal(type(basic, 'char', _), '\b') :-
    printf('\'\\b\'').
gen_literal(type(basic, 'char', _), '\n') :-
    printf('\'\\n\'').
gen_literal(type(basic, 'char', _), '\t') :-
    printf('\'\\t\'').
gen_literal(type(basic, 'char', _), '\r') :-
    printf('\'\\r\'').
gen_literal(type(basic, 'char', _), '\f') :-
    printf('\'\\f\'').
gen_literal(type(basic, 'char', _), _value) :-
%    not(equals(_value, '\\')),
    printf('\'~a\'',[_value]).

gen_literal(type(basic, _type, _), _value) :-
    not(equals(_type, 'char')),
    printf(_value).

gen_literal(type(class, _classID, _), _value) :-
    not(is_java_lang_string(_classID)),
    printf(_value).

gen_literal(type(class, _classID, _), _value) :-
    is_java_lang_string(_classID),
    concat_atom(_list0, '\\', _value),
    concat_atom(_list0, '\\\\', _value0),
    concat_atom(_list1, '\n', _value0),
    concat_atom(_list1, '\\n', _value1),
    concat_atom(_list2, '\"', _value1),
    concat_atom(_list2, '\\"', _value2),
    concat_atom(_list3, '\t', _value2),
    concat_atom(_list3, '\\t', _value3),
    concat_atom(_list4, '\r', _value3),
    concat_atom(_list4, '\\r', _value4),
    concat_atom(_list5, '\f', _value4),
    concat_atom(_list5, '\\f', _value5),
    concat_atom(_list6, '\b', _value5),
    concat_atom(_list6, '\\b', _value6),
    printf('\"~a\"',[_value6]).

gen_non_null(_, 'null').
gen_non_null(_str, _tree) :-
    not(equals(_tree,'null')),
    printf(_str),
    gen_tree(_tree).

gen_method_name(_id) :-
    methodDefT(_id, _class, '<init>', _, _, _, _),
    class(_class, _, _name),
    printf(_name).

gen_method_name(_id) :-
    methodDefT(_id, _, _name, _, _, _, _),
    not(equals(_name,'<init>')),
    printf(_name).

gen_method_name_type(_id) :-
    methodDefT(_id, _class, '<init>', _, _, _, _),
    class(_class, _, _name),
    printf(_name).

gen_method_name_type(_id) :-
    methodDefT(_id, _, _name, _, _type, _, _),
    not(equals(_name,'<init>')),
    gen_type(_id, _type),
    printf(' '),
    printf(_name).

gen_finalizer('null').
gen_finalizer(_fin) :-
    not(equals(_fin, 'null')),
    printf(' finally '),
    gen_tree(_fin).

gen_label('null').
gen_label(_l) :-
    not(equals(_l, 'null')),
    printf(_l).


/***********************************************************
 * gen_tree for every possible kind of tree
 ***********************************************************/

gen_toplevels :-
    findall(_tl, toplevelT(_tl,_,_,_), _allTls),
    gen_toplevels(_allTls).

gen_toplevels([]).
gen_toplevels([_H | _T]) :-
    gen_toplevel(_H),
    gen_toplevels(_T).



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
gen_operation([_head | _], _name, -1) :-
    printf('~a',[_name]),
    gen_tree(_head).

% POSTDEC - put operator after expr
gen_operation([_head | _], _name, 1) :-
    gen_tree(_head),
    printf('~a',[_name]).

% else - put operator between exprs
gen_operation([_head | _tail], _name, 0) :-
    printf('('),
    gen_tree(_head),
    printf(' ~a ',[_name]),
    gen_list(_tail),
    printf(')').

print_encl('null').
print_encl(_encl) :-
    not(equals(_encl, 'null')),
    gen_tree(_encl),
    printf('.').

print_anonymous_class('null').
print_anonymous_class(_def) :-
    not(equals(_def, 'null')),
    classDefT(_def, _p, _, _defs),
    gen_block(_defs).


gen_toplevel(_toplevel) :-
    toplevelT(_toplevel, _packg, _filename, _defs),
    !,
    stringAppend('prologJavaOut/', _filename, _dirfile),
    createDirsIfNeeded(_dirfile),
    open(_dirfile, write, _fileStream),
    %for debugging
%    current_output(_fileStream),
    retractall(file_output(_)),
    assert(file_output(_fileStream)),
    retractall(align(_)),
    assert1(align(0)),
    gen_tree(_packg),
    gen_stats(_defs),
    retract(file_output(_fileStream)),
    close(_fileStream).

debug_this_line(_id) :-
    not(equals(_id, 128548)).

debug_this_line(128548) :-
    !,
    dummy_pred.

dummy_pred.




gen_tree('null').

gen_tree(_id) :-
    packageT(_id, _packName),
    !,
    printf('package ~a;~n~n', [_packName]).

gen_tree(_id) :-
    importT(_id, _pid, _name),
    !,
    printf('import ~a;~n', [_name]).


gen_tree(_id) :-
    classDefT(_id, _pid, _name, _),
    !,
    gen_class(_id, _name).


gen_tree(_id) :-
    methodDefT(_id, _pid, _, _args, _ret, _exc, _body),
%    debug_this_line(_id),
    !,
    gen_modifier(_id),
    gen_method_name_type(_id),
    printf('('),
    gen_komma_list(_args),
    printf(')'),
    gen_exceptions(_id, _exc),
    gen_method_body(_body),
    println.

gen_tree(_id) :-
    varDefT(_id, _pid, _, _type, _name, _init),
    !,
    gen_modifier(_id),
    gen_type(_id, _type),
    printf(' ~a',[_name]),
    gen_init(_init),
    gen_semicolon(_pid).

gen_tree(_ID) :-
    blockT(_ID, _,_ , _list),
    !,
    gen_block(_list).

gen_tree(_ID) :-
    assignT(_ID, _,_, _lhs, _rhs),
    !,
    gen_tree(_lhs),
    printf(' = '),
    gen_tree(_rhs).

gen_tree(_ID) :-
    assignopT(_ID, _,_, _lhs, _op, _rhs),
    !,
    gen_tree(_lhs),
    printf(' '),
    printf(_op),
    printf('= '),
    gen_tree(_rhs).

gen_tree(_ID) :-
    newArrayT(_ID, _p,_, _dims, _elems, _elemtype),
    !,
    gen_new_array_elemtype(_ID, _dims, _elemtype, _depth, _elems),
    gen_komma_list_in_curly_brackets(_elems).

gen_tree(_ID) :-
    typeCastT(_ID, _,_, _type, _expr),
    !,
    printf('('),
    printf('('),
    gen_type(_ID, _type),
    printf(')('),
    gen_tree(_expr),
    printf('))').

gen_tree(_ID) :-
    typeTestT(_ID, _,_, _type, _expr),
    !,
    printf('('),
    gen_tree(_expr),
    printf(' instanceof '),
    gen_type(_ID, _type),
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
    identT(_ID,_,_,_name,_),
    !,
    printf(_name).

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
    applyT(_ID, _,_, _,_ident, _args,_),
    !,
    gen_tree(_ident),
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
    newClassT(_ID, _,_, _, _args, _clazz, _def, _encl),
    !,
    print_encl(_encl),
    printf('new '),
    gen_tree(_clazz),
    printf('('),
    gen_komma_list(_args),
    printf(')'),
    print_anonymous_class(_def).


/*gen_tree(_ID) :-
    newClassT(_ID, _, _constructor, _args, _clazz, _def, _),
    treeT(_constructor, _newClass, methodDefT),
    !,
    fullQualifiedName(_constructor, _parent, methodDefT),
    printf('new '),
    gen_tree(_clazz),
    printf('('),
    gen_komma_list(_args),
    printf(')'),
    print_anonymous_class(_def).
*/

gen_tree( _ID) :-
    operationT(_ID, _,_, _args, _name, _pos),
    !,
    gen_operation(_args, _name, _pos).

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
    printf('try '),
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
    forLoopT(_ID, _,_, _inits, _cond, _steps, _body),
    !,
    printf('for('),
    gen_komma_list(_inits),
    printf(';'),
    gen_tree(_cond),
    printf(';'),
    gen_komma_list(_steps),
    printf(')'),
    gen_tree(_body).

gen_tree(_ID) :-
    switchT(_ID, _,_, _selector, _cases),
    !,
    printf('switch('),
    gen_tree(_selector),
    printf(')'),
    gen_block(_cases).

gen_tree(_ID) :-
    caseT(_ID, _, _,_label),
    !,
    printf('case '),
    gen_tree(_label),
    printf(' : '),
    println,
    gen_stats(_body).

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
    labelledT(_ID, _, _,_body, _name),
    !,
    printf('~a : ',[_name]),
    gen_tree(_body).


gen_tree(_ID) :-
    format('ERROR: ~w~n', [_ID]).

/**
 * Query
 */

