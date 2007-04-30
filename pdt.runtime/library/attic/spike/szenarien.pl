/* scenario I variant A*/

/* This is an example from the export_annotator
  InTerm, TmpTerm and OutTerm are entity states.
 */
 /*
term_annotation_hook(_,_,_,InTerm,OutTerm):-
    %% check if term is a module declaration
    %% if not, fail. 
    %% the framework will catch us and unify InTerm=OutTerm.
    source_term_expand(InTerm,Stripped),    
    Stripped==':-'(module(_Module,_Exports)),

	%% check if this is the first term. Otherwise add problem marker.
	check_position(InTerm,TmpTerm),
	
	%% check the exports list, adding problem markers where necessary
	check_exports(TmpTerm,OutTerm).
	*/
	
/*
This scenario represents a trivial case:
* source_term_expand/2 is deterministic
* the comparison may fail, but we have not yet altered any entity.
* check_position/2 should be deterministic and should never fail
* check_exports/2 should be deterministic and should never fail

If ID holds an entity reference, we want to rewrite this to yield
*/	

/*
term_annotation_hook(_,_,_,ID):-
	
    source_term_expand(ID,Stripped),    
    Stripped==':-'(module(_Module,_Exports)),

	check_position(ID),
	
	check_exports(ID).
*/


v:-assert(w),w.

/*
source_term_expand/2 is a forwarding predicate, it delegates to the 


respective data structure implementation. 

check_position/2 and check_exports/2

*/

check_exports(In,Out):-
	source_term_subterm(In,[1,2],Exports),
	%% find ill-formed elements in the exports list
	findall(
		ill_formed(Path,AExport),
		(	source_term_member(Exports,Path,AExport),
			source_term_expand(AExport,Export),
			\+ well_formed_export(Export)
		),
		IllFormedExports
	),
	add_ill_formed_annos(In,IllFormedExports,Out).

add_ill_formed_annos(In,[],In).
add_ill_formed_annos(In,[ill_formed(Path,InExport)|IFEs],Out):-
    pdt_aterm_term_annotation(InExport,Term,Annotation),
    pdt_aterm_term_annotation(OutExport,Term,[problem(error(malformed_export))|Annotation]),
    pdt_aterm_subst(In,[1,2|Path],OutExport,Next),
    add_ill_formed_annos(Next,IFEs,Out).
    
well_formed_export(Name/Arity):-
    atom(Name),
    integer(Arity).

mark_subterm1(In,[],Out):-
	mark1(In,Out).
mark_subterm1(In,[K|Ks],Out):-
    source_term_arg(K,In,Arg),
    source_term_setarg(K,In,NewArg,Out), %O(numArgs(In)) 
    mark_subterm1(Arg,Ks,NewArg).
	

mark_subterm2(Ref,[]):-
	mark2(Ref).
mark_subterm2(Ref,[K|Ks]):-
    source_term_arg(K,Ref,ArgRef),
    mark_subterm2(ArgRef,Ks).

mark_subterm3(In,[],Out):-
	mark3(In,Tmp),%O(n)
	merge(In,Tmp,Out). %O(1)
	
mark_subterm3_x(In,[],Out):-
	mark3(In,Out). %O(1)
mark_subterm3_x(In,[K|Ks],Out):-    
    source_term_arg(K,In,Arg),%O(1)
    mark_subterm3_x(Arg,Ks,Out). %O(n-1)
	