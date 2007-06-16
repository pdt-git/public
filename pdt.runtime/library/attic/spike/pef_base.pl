:- module(pef_base,[pef_reserve_id/2, pef_type/2]).

:- use_module(library('org/cs3/pdt/util/pdt_util_context')).	
:- dynamic pef_pred/2.
:- dynamic pef_type/2.
:- dynamic pef_edge/5.
:- dynamic '$metapef_concrete'/1.
:- dynamic '$metapef_edge'/3.
:- dynamic '$metapef_is_a'/2.

define_assert(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_assert',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,assert(Cx)),Ref),
    assert(pef_pred(Name,Ref)),
    export(Head).

define_query(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_query',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,call(Cx)),Ref),
    assert(pef_pred(Name,Ref)),
    export(Head).

define_query2(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_query',HeadName),
    functor(Head,HeadName,2),
    arg(1,Head,List),
    arg(2,Head,Cx),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,call(Cx)),Ref),
    assert(pef_pred(Name,Ref)),
    export(Head).


define_retractall(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_retractall',HeadName),
    functor(Head,HeadName,1),
    arg(1,Head,List),
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,retractall(Cx)),Ref),
    assert(pef_pred(Name,Ref)),
    export(Head).

define_recorda(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_recorda',HeadName),
    functor(Head,HeadName,3),
    arg(1,Head,Key),
    arg(2,Head,List),
    arg(3,Head,Ref),    
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,recorda(Key,Cx,Ref)),PefRef),
    assert(pef_pred(Name,PefRef)),    
    export(Head).

define_recordz(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_recordz',HeadName),
    functor(Head,HeadName,3),
    arg(1,Head,Key),
    arg(2,Head,List),
    arg(3,Head,Ref),    
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,recordz(Key,Cx,Ref)),PefRef),
    assert(pef_pred(Name,PefRef)),
    export(Head).

define_recorded(Template):-
    functor(Template,Name,_),
    atom_concat(Name,'_recorded',HeadName),
    functor(Head,HeadName,3),
    arg(1,Head,Key),
    arg(2,Head,List),
    arg(3,Head,Ref),    
    atom_concat(Name,'_get',GetterName),
    functor(Getter,GetterName,2),
    arg(1,Getter,Cx),
    arg(2,Getter,List),
    atom_concat(Name,'_new',ConstructorName),
    functor(Constructor,ConstructorName,1),
    arg(1,Constructor,Cx),
    assert((Head:-Constructor,Getter,recorded(Key,Cx,Ref)),PefRef),
    assert(pef_pred(Name,PefRef)),
    export(Head).



%%
% define_pef(+Template).
%
% define a new PEF type.
% Suppose template is foo(bar,baz).
% Then this call will generate and export predicates foo_assert(+List), foo_retractall(+List)
% and foo_query(+List), where List is a list of key=Value pairs.
% E.g. you can use foo_query([bar=bang,baz=V]), for retreiveing the baz of all foos with 
% a bar of bang.
% @param Template should be a ground compound term like in pdt_define_context/1.

define_pef(TypedTemplate):-
    strip_types(TypedTemplate,Template),
    functor(Template,Name,Arity),
    undefine_pef(Name),
    assert('$metapef_concrete'(Name),Ref),
    assert(pef_pred(Name,Ref)),    
    assert('$metapef_template'(Name,Template),Ref2),
    assert(pef_pred(Name,Ref2)),        
    process_types(TypedTemplate,_),    
    dynamic(Name/Arity),
	pdt_define_context(Template),	
	pdt_export_context(Name),
    define_assert(Template),
    define_retractall(Template),
    define_query(Template),
    define_query2(Template),
    define_recorded(Template),
    define_recorda(Template),
    define_recordz(Template).

% remove type identifier from a template
strip_types(T1:_,T2):-
    !,
    strip_types(T1,T2).
strip_types(T1,T2):-
    T1=..[F|Args1],
    strip_types_args(Args1,Args2),
    T2=..[F|Args2].

strip_types_args([],[]).
strip_types_args([Arg:_|Args1],[Arg|Args2]):-
    !,
    strip_types_args(Args1,Args2).
strip_types_args([Arg|Args1],[Arg|Args2]):-
    strip_types_args(Args1,Args2).


metapef_is_a(A,A).
metapef_is_a(A,B):-
    (	nonvar(A)
    ->	'$metapef_is_a'(A,Tmp),
    	metapef_is_a(Tmp,B)
    ;	'$metapef_is_a'(Tmp,B),
    	metapef_is_a(A,Tmp)
    ).
metapef_is_a(A,any):-
    '$metapef_concrete'(A).


%hack: make things apear in the graph that are not explicitly represented as pefs.
'$metapef_concrete'(toplevel).
'$metapef_concrete'(program).
process_types(Tmpl:T,Stripped):-
    !,
    functor(Tmpl,Name,_),
    assert('$metapef_is_a'(Name,T),Ref),
    assert(pef_pred(Name,Ref)),
    process_types(Tmpl,Stripped).
process_types(Tmpl,Stripped):-
	functor(Tmpl,Name,Arity),
	functor(Stripped,Name,Arity),	
	process_types_args(1,Name,Arity,Tmpl,Stripped).


process_types_args(I,_Name,Arity,_Tmpl,_Stripped):-
    I>Arity,
    !.
process_types_args(I,Name,Arity,Tmpl,Stripped):-
	process_types_arg(I,Name,Arity,Tmpl,Stripped),
	J is I + 1,
	process_types_args(J,Name,Arity,Tmpl,Stripped).

process_types_arg(I,Name,_Arity,Tmpl,Stripped):-
    arg(I,Tmpl,Arg:ArgT),
    !,
    arg(I,Stripped,Arg),
	assert('$metapef_edge'(Name,I,ArgT),Ref),
	assert(pef_pred(Name,Ref)).
	
process_types_arg(I,_Name,_Arity,Tmpl,Stripped):-
    arg(I,Tmpl,Arg),
    arg(I,Stripped,Arg).

process_meta_edges:-
    forall('$metapef_edge'(FromT,ArgNum,ToT),process_meta_edge(FromT,ArgNum,ToT)).


pef_edge(From,FromT,ArgName,To,ToT):-
    '$pef_edge'(From,FromT,ArgName,To,ToT),
    valid_target(ToT,To).

valid_target(ToT,To):-
    pef_type(To,ToT).
process_meta_edge(FromT,ArgNum,ToT):-
    '$metapef_template'(FromT,FromTemplate),
    functor(FromTemplate,_,Arity),
    functor(FromHead,FromT,Arity),
    arg(ArgNum,FromTemplate,ArgName),
    arg(ArgNum,FromHead,To),
    forall(
    	(	metapef_is_a(SubT,ToT),
    		'$metapef_concrete'(SubT)
    	),
		(	(	find_id(FromTemplate,IdNum)  
		    ->	arg(IdNum,FromHead,From),
		    	Clause=
		    		(	'$pef_edge'(From,FromT,ArgName,To,SubT):-
		        			call(FromHead)
		        	)
			;   Clause =
					(	'$pef_edge'(From,FromT,ArgName,To,SubT):-
		        			clause(FromHead,_,FromRef),
		        			From=FromRef
		        	)
		    ),
		    assert(Clause,Ref),
		    assert(pef_pred(FromT,Ref))
		)
	).

find_id(Tmpl,Num):-
    arg(Num,Tmpl,id),
    !.

undefine_pef(Name):-
    forall(pef_pred(Name,Ref),erase(Ref)),
    retractall(pef_pred(Name,_)).

pef_reserve_id(Type,Id):-
    flag(pef_next_id,Id,Id + 1),
    assert(pef_type(Id,Type)). 

% A module definition. Also represents the defined module.
:- define_pef(pef_module_definition(id,name,file:pef_file,toplevel:pef_toplevel):module).

% An operator definition. 
:- define_pef(pef_op_definition(id,priority,type,name,file:pef_file,toplevel:pef_toplevel)).

% A file dependency definition. Also represents the defined dependency.
:- define_pef(pef_file_dependency(id,depending:pef_file,dependency:pef_file,toplevel:pef_toplevel)).

% A named property of any pef. Id is the PEFs Id. The property itself is a weak entity,
% it does not have an Id of its own.
:- define_pef(pef_property(pef:any,key,value)).



% A parsed toplevel term. 
:- define_pef(pef_toplevel(id,file:pef_file,term,expanded,positions,varnames,singletons)).

% An AST node representing a non-var program term.
:- define_pef(pef_term(id,name,arity):ast_node).

% An AST node representing a program variable occurence.
:- define_pef(pef_variable_occurance(id,variable:pef_variable):ast_node).

% A program variable.
:- define_pef(pef_variable(id,toplevel:pef_toplevel)).

% The relation between a compound term and its arguments.
:- define_pef(pef_arg(num,parent:pef_term,child:ast_node)).

% The relation between a toplevel record and the root of corresponding AST.
:- define_pef(pef_toplevel_root(root:ast_node,toplevel:pef_toplevel,file:pef_file)).


% The mapping of names to modules within a program
% there currently is no separate pef for programs.
% Instead, file references are used to identify the program that results from
% loading that file into a new runtime.
:- define_pef(pef_program_module(program:pef_program,name,module:module)).

% A predicate
% note that module is a module identifier, not a module name.
:- define_pef(pef_predicate(id,module:module,name,arity)).

% The mapping of names to predicates within a module
:- define_pef(pef_imported_predicate(module:module,name,arity,predicate:pef_predicate)).

% The relation between a predicate and its clauses
:- define_pef(pef_clause(predicate:pef_predicate,number,toplevel:pef_toplevel)).

% A special Module that results from extending an existing module definition,
% e.g. by adding clauses to multifile predicates.
:- define_pef(pef_module_extension(id,base:pef_module_definition,program:pef_program):module).

% A special Module that is defined "ad hoc", i.e. there is no file
% associated to it.
:- define_pef(pef_ad_hoc_module(id,name,program:pef_program):module).

% The relation between modules and the signatures they export
% signature may be either Name/Arity or op(Pr,Tp,Nm)
:- define_pef(pef_exports(module:module,signature)).


% The relation between programs and files
% force_reload is true if file was loaded using consult/1 rather than ensure_loaded/1 or use_module/1.
% otherwise it is false.
:- define_pef(pef_program_file(program:pef_program,file:pef_file,module_name,force_reload)).

% The relation between predicates and their property definitions.
% Don't confuse this with normal pef_properties:
% pef_properties can be attached to any pef. In particular, they have no direct relation to source code.
% predicate property definitions are more like clauses - they are attached to toplevel terms.
% When predicates are merged or copied, so are the property definitions.
:- define_pef(pef_predicate_property_definition(predicate:pef_predicate,toplevel:pef_toplevel,property)).

% The relation between a module and its import list
% The import list is a list of module NAMES.
% This fact only exists, if the module has an import list that differs from the default one:
% By default each module imports user and user imports system.
% Also, a module extension inherits the list of its base by default. 
% If it has a list of its own, the list of its base is ignored.
:- define_pef(pef_import_list(module:module,list)).

% various problem pefs produced by the interpreter
:- define_pef(pef_module_name_clash(id,program:pef_program,toplevel:pef_toplevel,first:module,second:module):interpreter_problem).
:- define_pef(pef_predicate_name_clash(id,program:pef_program,toplevel:pef_toplevel,module:module,first:pef_predicate,second:pef_predicate):interpreter_problem).
:- define_pef(pef_predicate_redefinition(id,program:pef_program,toplevel:pef_toplevel,first:pef_predicate,second:pef_predicate):interpreter_problem). %TODO
:- define_pef(pef_predicate_abolished(id,program:pef_program,toplevel:pef_toplevel,module:module,abolished:pef_predicate):interpreter_problem).
:- define_pef(pef_unresolved_export(id,program:pef_program,toplevel:pef_toplevel,module:module,name,arity,export:ast_node/*TODO*/):interpreter_problem).

% problem pefs generated by the singleton checker (TODO)
:- define_pef(pef_singleton(id,toplevel:pef_toplevel,variable:pef_variable):singleton_problem).
:- define_pef(pef_no_singleton(id,toplevel:pef_toplevel,variable:pef_variable):singleton_problem).

% problem pefs generated by the parser
:- define_pef(pef_syntax_error(file:pef_file,start,end,message):parser_problem).
:- define_pef(pef_file_not_found(toplevel:pef_toplevel,file_spec):parser_problem).

:- define_pef(pef_program(id,file:pef_file)).
:- define_pef(pef_file(id,path)).


:- process_meta_edges.