/* $LICENSE_MSG$(ld) */

:-module(undefined_exports_annotator,[]).

:- use_module(library('/org/cs3/pdt/util/pdt_util')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).
:- use_module(library('/org/cs3/pdt/annotate/pdt_annotator')).
:- use_module(library('/org/cs3/pdt/util/pdt_util_aterm')).


:- pdt_annotator([term],[
	library('org/cs3/pdt/annotate/export_annotator'),
	library('org/cs3/pdt/annotate/member_annotator')
]).


term_annotation_hook(_,_,Annos,InTerm,OutTerm):-
    %% check if term is a module declaration
    pdt_aterm_strip_annotation(InTerm,':-'(module(_Module,_Exports)),_),    
    pdt_member(defines(StatDefs),Annos),
    pdt_member(defines_dynamic(DynDefs),Annos),
    append(StatDefs,DynDefs,Defs),
    pdt_member(defines_module(Module),Annos),
	%% check the exports list 
	check_exports(InTerm,Module,Defs,OutTerm).

    
check_exports(In,Module,Defs,Out):-
	pdt_aterm_subterm(In,[1,2],Exports),
	%% find ill-formed elements in the exports list
	findall(undefined(Path,AExport),
		(	pdt_aterm_member(Exports,Path,AExport),
			pdt_aterm_strip_annotation(AExport,Export,_),
			well_formed_export(Export),
			\+ memberchk(Module:Export,Defs)
		),
		UndefinedExports
	),
	add_undefined_annos(In,UndefinedExports,Out).

add_undefined_annos(In,[],In).
add_undefined_annos(In,[undefined(Path,InExport)|IFEs],Out):-
    pdt_aterm_term_annotation(InExport,Term,Annotation),
    pdt_aterm_term_annotation(OutExport,Term,[problem(error(undefined_export))|Annotation]),
    pdt_aterm_subst(In,[1,2|Path],OutExport,Next),
    add_undefined_annos(Next,IFEs,Out).

well_formed_export(Name/Arity):-
    atom(Name),
    integer(Arity).
    

