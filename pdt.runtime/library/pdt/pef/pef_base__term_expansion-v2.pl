:- use_module(library('org/cs3/pdt/util/pdt_util_context')).
:- use_module(library('org/cs3/pdt/util/pdt_util')).

:- op(600,xfy,@).


/*
Expansion of declarations - "the general idea" + rational:
  - Each pef type declaration is parsed into a type_decl datastructure, see below.
  - The type_decl is passed on to a predicate expand_declaration/2, which processes it into actual prolog declarations (clauses or directives).
    All successfull instances are collected and asserted into the database.
  - the expand_declaration/2 predicate might be interesting for later extensions. Maybe make it multifile.
  
  - currently there is an additional post processing stage, which runs after all declarations have been processed. 
    We want to get rid of this stage, as it makes it difficult add declarations in a decentralized manner.
  - related to the above: the expand_declaration/2 predicate "sees" the expanded result of all declartion 
    preceeding the one beeing currently processed. It cannot make any assumptions about the futer expansion  of 
    subsequent declarations.
  - At no place during this process we shall alter the database directly. If we restrict ourselfs to working 
    with term expansion, the prolog interpreter will take care of any cleanup work when the same source file is read more than once.
    In general, all program modifications during load-time should occur this way, since this helps 
    tools like the PDT to understand what is happening during load time. 
    All we have to do is give the expansion rules to the tool.   
*/

/*
name is type name of the declared type
attributes is a term arg(arg decl, arg decl,...., arg decl) where arg decl is an attribute declaration term, see below.
supertypes is a list of type names.
tags is a list of tags

this data structure is for internal use in this module only.
*/
:- pdt_define_context(type_decl(name,attributes,supertypes,tags)).

/*
name is the name of the declared attribute.
types is a list of type names
tags is a list of tags

this data structure is for internal use in this module only.
*/
:- pdt_define_context(attr_decl(num,name,types,tags)).





user:term_expansion((:- define_pef(TypedTemplate)),Decls):-
    pef_base__term_expansion(TypedTemplate,Decls).
    
% entry point for expansion of pef type declarations.
pef_base__term_expansion(TypedTemplate,Decls):-    
	% if the respective option is enabled, add implicit id attribute if the template does not already have one.
	( 	'$option'(implicit_ids)
    ->	ensure_has_id(TypedTemplate0,TypedTemplate)
    ;	TypedTemplate=TypedTemplate0
    ),
    % created a untyped and untagged version of the template and have
    % it processed by pdt_define_context/1. 
    % This way we can reuse all the field accessor predicates from there.        
    strip_types(TypedTemplate,Template),    
	expand_term((:-pdt_define_context(Template)),ContextDecls),	    
	findall(Decl,pdt_util_context:export_decl(Template,Decl),ContextExportDecls),    
    
	% parse the template into a type_decl Datastructure    
    parse_template(TypedTemplate,TypeDecl),
    
    % generate the actual declarations from it.
    findall(Decl,expand_type_declaration(TypeDecl,Decl),ExpandedDecls),
    
    % put everything together.
    flatten([ContextDecls,ContextExportDecls,ExpandedDecls],Decls).


% remove types and tags from a type template
strip_types(T1:_,T2):-
    !,
    strip_types(T1,T2).
strip_types(T1 @ _,T2):-
    !,
    strip_types(T1,T2).
strip_types(T1,T2):-
    T1=..[F|Args1],
    strip_types_args(Args1,Args2),
    T2=..[F|Args2].

strip_types_args([],[]).
strip_types_args([Arg:_|Args1],[StrippedArg|Args2]):-
    !,
    strip_types(Arg,StrippedArg),
    strip_types_args(Args1,Args2).
strip_types_args([Arg @ _|Args1],[Arg|Args2]):-
    !,
    strip_types_args(Args1,Args2).
strip_types_args([Arg|Args1],[Arg|Args2]):-
    strip_types_args(Args1,Args2).

%--------------------------------------
	
parse_type_declaration(Type @ TagAndMore,TypeDecl):-
    type_decl_new(TypeDecl),
    parse_tag_and_more(TagAndMore,Tags,SuperTypes),
    parse_attributes(Type,Attributes),
    type_decl_tags(TypeDecl,Tags),
    type_decl_supertypes(TypeDecl,SuperTypes),
    type_decl_attributes(TypeDecl,Attributes).
parse_type_declaration(Type : TypeAndMore,TypeDecl):-
    type_decl_new(TypeDecl),
    parse_type_and_more(TypeAndMore,Tags,SuperTypes),
    parse_attribute_declarations(Type,Attributes),
    type_decl_tags(TypeDecl,Tags),
    type_decl_supertypes(TypeDecl,SuperTypes),
    type_decl_attributes(TypeDecl,Attributes).
parse_type_declaration(Type,TypeDecl):-
    type_decl_new(TypeDecl),    
    parse_attribute_declarations(Type,Attributes),
    type_decl_tags(TypeDecl,[]),
    type_decl_supertypes(TypeDecl,[]),
    type_decl_attributes(TypeDecl,Attributes).

parse_tag_and_more(Tag @ TagAndMore,[Tag|Tags],Types):-
    !,
    parse_tag_and_more(TagAndMore,Tags,Types).
parse_tag_and_more(Tag : TypeAndMore,[Tag|Tags],Types):-
    !,
    parse_type_and_more(TypeAndMore,Tags,Types).
parse_tag_and_more(Tag,[Tag],[]).

parse_type_and_more(Type @ TagAndMore,Tags,[Type|Types]):-
    !,
    parse_tag_and_more(TagAndMore,Tags,Types).
parse_type_and_more(Type : TypeAndMore,Tags,[Type|Types]):-
    !,
    parse_type_and_more(TypeAndMore,Tags,Types).
parse_type_and_more(Type,[],[Type]).


parse_attribute_declarations(N,_Type,Attributes):-    
    N==0,!.
parse_attribute_declarations(N,Type,Attributes):-
    arg(N,Type,TypeArg),
    arg(N,Attributes,AttributesArg),
    parse_attribute_declaration(N,TypeArg,AttributesArg),
    M is N - 1,
    parse_attributes(M,Type,Attributes).
    
parse_attribute_declaration(N,Arg,Decl):-
	attr_decl_new(Decl),
	attr_decl_num(Decl,N),
	(	Arg = Name @ TagAndMore
	->	parse_tag_and_more(TagAndMore,Tags,Types),
		attr_decl_name(Decl,Name),
		attr_decl_tags(Decl,Tags),
		attr_decl_types(Decl,Types)
	;	Arg = Name : TypeAndMore
	->	parse_type_and_more(TypeAndMore,Tags,Types),
		attr_decl_name(Decl,Name),
		attr_decl_tags(Decl,Tags),
		attr_decl_types(Decl,Types)
	;	attr_decl_name(Decl,Arg),
		attr_decl_tags(Decl,[]),
		attr_decl_types(Decl,[])
	).
	
%--------------------------------------	
