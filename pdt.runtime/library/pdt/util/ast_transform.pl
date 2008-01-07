%here I gather some "manual" ast transformations I use for testing purposes.
%Will probably eventually evolve them into reusable CT-Sequences
%But for now its just plain ol' prolog. 

:- module(ast_transform,
	[	astseq_remove/1,
		astseq_remove_keep/1,
		ast_replace/2,
		ast_replace_keep/2,
		ast_unlink/1,
		ast_delete/1,
		fix_variables/2,		
		delete_unused_variables/1,
		mark_toplevel_modified/1,
		mark_file_modified/1
	]
).

:- ensure_loaded(library('pef/pef_base')).
:- ensure_loaded(library('pef/pef_api')).


/*
predicates starting with astseq are ment to operate on elements of sequences.

Seqences are right-recursive chains of binary nodes of the same functor type, 
terminated by an arbitrary other node that does not fit into this schema.
Typical examples would lists or conjunctions. The type of the chain operator is determined
by looking at the arguments parent node.
*/


/*
variables and consistency

After deleting or moving subtrees, the variable set of the affected toplevels may be inconsistent:
1) The toplevel may contain occurances of variables that orginate from another toplevel.
  The predicate fix_variables/2 takes care of those. 
  You MUST run this predicate on all modified toplevels, or else your program may be inconsistent.
2) A toplevel may contain variables that are not used any more. 
  The preducate delete_unused_variables/2 takes care of those.
  Note that occurances in unlinked subtrees, or occurances that were moved into
  other toplevels on which fix_variables/2 has not yet been run will keep variables 
  from beeing deleted.
  
  To avoid confusion:
  - keep track of your garbage. Don't leave unlinked subtrees in the database.
  - always run the FIRST step on all affected toplevels. THEN run the second on all
    affected toplevels.
*/

delete_unused_variables(Toplevel):-
    pef_ast_query([toplevel=Toplevel,id=Ast]),
    forall(
    	pef_variable_query([ast=Ast,id=Var]),
    	(	pef_variable_occurance_query([variable=Var])
    	->	true
    	;	pef_delete(Var)
    	)
    ).

fix_variables(Toplevel,Mode):- %mode is "merge" or "rename"
    pef_ast_query([toplevel=Toplevel,root=Root,id=Ast]),
	(	setof(
	    	occ(Name,Occ),
	    	fix_vars__nonlocal_occ(Ast,Root,_,Name,Occ),
	    	Occs
	    )
	->	true
	;	Occs=[]
	),	   
    (	setof(
	    	Name,
	    	memberchk(occ(Name,_),Occs),
	    	Names
	    )
	->	true
	;	Names=[]
	),    
    fix_vars__generate_local(Names,LocalVars,Ast,Mode),
    fix_vars__relink(Occs,LocalVars).

fix_vars__nonlocal_occ(Ast,Root,Var,Name,Occ):-
    subtree(Root,Occ),
    pef_variable_occurance_query([id=Occ,variable=Var]),
    (	pef_variable_query([id=Var,name=Name,ast=Ast2])
    ->	Ast \== Ast2
    ;	atom_concat('DeletedVariable_',Var,Name)
    ).
    


fix_vars__generate_local([],[],_,_).
fix_vars__generate_local([Name|Names],[local(Name,Var)|LocalVars],Ast,Mode):-
    (	pef_variable_query([ast=Ast,name=Name,id=ExistingVar])
    ->	(	Mode==rename
    	->	fix_vars__unused_name(Ast,Name,NewName),
    		pef_reserve_id(pef_variable,Var),
    		pef_variable_assert([id=Var,ast=Ast,name=NewName])
    	;	Var=ExistingVar
    	)
    ;	pef_reserve_id(pef_variable,Var),
    	pef_variable_assert([id=Var,ast=Ast,name=Name])
    ),
    fix_vars__generate_local(Names,LocalVars,Ast,Mode).

fix_vars__relink([],[]):-
	!.    
fix_vars__relink([occ(Name,Occ)|Occs],[local(Name,Var)|LocalVars]):-
    !,
	pef_variable_occurance_retractall([id=Occ]),
	pef_variable_occurance_assert([id=Occ,var=Var]),    
	fix_vars__relink(Occs,[local(Name,Var)|LocalVars]).    
fix_vars__relink(Occs,[_|LocalVars]):-    	
	fix_vars__relink(Occs,LocalVars).    

fix_vars__unused_name(Ast,Base,Name):-
    pef_variable_query([ast=Ast,name=Base]),
    !,
    atom_concat(Base,'_X',NewBase),
    fix_vars__unused_name(Ast,NewBase,Name).
fix_vars__unused_name(_Ast,Name,Name).




subtree(A,B):-
    (	nonvar(B)
    ->	subtree_up(A,B)
    ;	subtree_down(A,B)
    ).
subtree_down(Ast,Ast).
subtree_down(Ast,Subtree):-
	pef_arg_query([parent=Ast,child=Child]),
	subtree_down(Child,Subtree).    
subtree_up(Ast,Ast).
subtree_up(Ast,Subtree):-
	pef_arg_query([parent=Parent,child=Subtree]),
	subtree_up(Ast,Parent).    


my_arg(Num,Parent,Child):-
    pef_arg_query([child=Child,parent=Parent,num=Num]).

my_functor(Node,Name,Arity):-
	pef_term_query([id=Node,name=Name,arity=Arity]).    

my_var(Node):-
	pef_type(Node,pef_variable_occurance).    

/*
remove an element from a sequence. 
unlinks the element, but does not delete it.
*/
astseq_remove_keep(Elm):-
	my_arg(Num,Par,Elm),
	my_functor(Par,_Name,2),
	(	Num==1
	->	my_arg(2,Par,Next),
		ast_replace_keep(Par,Next)
	;	my_arg(1,Par,Prev),
		ast_replace_keep(Par,Prev)
	),
	ast_unlink(Elm),
	ast_delete(Par).

astseq_remove(Elm):-
	astseq_remove_keep(Elm),
	ast_delete(Elm).	
/*
Replace one node with another.
First, the second argument is unlinked.
Then unlink the first argument and replace it with the second.
The first argument is unlinked but not deleted.
*/	
ast_replace_keep(Old,New):-
	ast_unlink(New),
	my_arg(Num,Par,Old),
	pef_arg_retractall([child=Old,parent=Par]),
	pef_arg_assert([child=New,num=Num,parent=Par]).

/*
Replace one node with another.
Like replace_keep, but recursively deletes the first argument.
*/	
ast_replace(Old,New):-
	ast_replace_keep(Old,New),
	ast_delete(Old).

/*
unlink a subtree from its parent.
modify the parent node so it is consistent
The previously containing toplevel may have unused variables. run repair_variables/2 on the containing toplevel.
to fix this. 
The unlinked subtree is NOT consistent after this. In particular it may contain
occurances of variables that belong to the previously surrounding toplevel. When inserting an unlinked tree
in a new context, repair_variables must be run on the containing toplevel to fix this.  
*/
ast_unlink(Node):-
	my_arg(N,Par,Node),
	pef_arg_retractall([child=Node,parent=Par,num=N]),
	pef_term_query([id=Par,name=Name,arity=Arity]),
	pef_term_retractall([id=Par]),
	NewArity is Arity - 1,
	pef_term_assert([id=Par,name=Name,arity=NewArity]),
	Start is N + 1,
	shift_left(Start,Arity,Par).
	
shift_left(N,Arity,Par):-
	(	N > Arity
	->	true
	;	M is N -1,
		O is N +1,
		my_arg(N,Par,Child),
		pef_arg_retractall([child=Child,parent=Par,num=N]),
		pef_arg_assert([child=Child,parent=Par,num=M]),
		shift_left(O,Arity,Par)
	).
	
ast_delete(Node):-
	forget_subtree(Node).	
	
forget_subtree(Id):-
    pef_variable_occurance_query([id=Id,variable=Ref]),
    !,
    %pef_variable_occurance_retractall([id=Id]),
    pef_delete(Id),
    % if this was the last occurance, remove the variable.
	(	pef_variable_occurance_query([variable=Ref])
	->	true
	;	%pef_variable_retractall([id=Ref])
		pef_delete(Ref)
	),
	pef_arg_retractall([child=Id]).
    
forget_subtree(Id):-
	forall(
		pef_arg_query([parent=Id,child=ChildId]),
		forget_subtree(ChildId)
	),
	%pef_term_retractall([id=Id]),
	pef_delete(Id),
	pef_arg_retractall([child=Id]).  	
	
%% mark_toplevel_modified(+Toplevel)
% mark a toplevel fact as 'modified'. 
% Do this after you modified a toplevel.
mark_toplevel_modified(Toplevel):-
    (	pef_modified_toplevel_query([toplevel=Toplevel])
    ->	true
    ;	pef_modified_toplevel_assert([toplevel=Toplevel])
    ),
    (	pef_toplevel_query([id=Toplevel,file=File])
    ->	mark_file_modified(File)
    ;	true
    ).

mark_file_modified(File):-
    (	pef_modified_file_query([file=File])
    ->	true
    ;	pef_modified_file_assert([file=File])
    ).
	