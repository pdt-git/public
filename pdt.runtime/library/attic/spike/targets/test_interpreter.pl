:- module(test_interpreter,[pdt_test_interpreter/2]).
:- use_module(library('org/cs3/pdt/util/pdt_util')).
:- use_module(library('spike/pef_base')).
:- use_module(library('spike/pef_api')).
:- use_module(library('spike/builder')).
:- use_module(library('spike/targets/program_interpreter')).
:- nospyall.
:- guitracer.
%:- debug(parser(todo)).
:- debug(interpreter(_)).
%:- spy(program_interpreter:unload_file/2).
%:- spy(debugme).
%:- spy(create_program).
%:- spy(rebind_module_name).
%:- spy(my_build_hook).
%%
% pdt_test_interpreter(+Spec)
% PEFs-vs-Reality check.
%
% Reality subset of PEFs?
% All modules that are not ignored (see ignored_module/1) are collected.
% for each we check
% - module definition or extension PEFs exists, associated to the correct file, or real module
%   is not associated to a file and there is corresponding a ad-hoc module PEF
% All predicates that are not ignored (see ignored_predicate/3) are collected.
% for each we check
% - corresponding predicate PEF exists in the program.
% - run through the clauses, check that corresponding PEF clauses exist.
% - run through the properties. dito
%
% PEFs subset of Reality?
% basically the same thing in the other direction.
% 
% 
pdt_test_interpreter(Spec,Result):-
    user:consult(Spec),
    pdt_file_spec(Spec,Abs),    
    assert(test_file(Abs)),
    pdt_invalidate_target(interprete(Abs)),
    pdt_with_targets([interprete(Abs)],test_interpreter(Abs,Result)).

test_interpreter(Abs,Result):-
    reality_subset_pefs(Abs,Result).
test_interpreter(Abs,Result):-
    pefs_subset_reality(Abs,Result).
    
real_current_module(TestFile,Name,File):-
    current_module(Name),
    (	current_module(Name,File)
    ->	true
    ;	File=[]
    ),
    \+ ignored_module(Name,File,TestFile).
    


%ignored_module(user,[],_).
ignored_module(system,[],_).
ignored_module(prolog,[],_).
ignored_module(prolog_edit,[],_).
%only consider module files in or below the same directory as the test file.
ignored_module(_,File,TestFile):-
    exists_file(File),
	file_directory_name(TestFile,Dir),
	\+ atom_concat(Dir,_,File). 


real_current_predicate(TestFile,Context,Module,Name/Arity):-
	real_current_module(TestFile,Context,_),
	Context:current_predicate(Name/Arity),
	functor(Head,Name,Arity),
	(	Context:predicate_property(Head,imported_from(Module))
	->	real_current_module(TestFile,Module,_)
	;	Module=Context
	),
	\+ ignored_predicate(Module,Head,TestFile).

ignored_predicate(user,test_file(_),_).
ignored_predicate(user,meta_data_module(_,_,_),_).
ignored_predicate(user,meta_data_help(_,_,_),_).	
ignored_predicate(user,meta_data_help(_,_,_,_),_).	
ignored_predicate(user,tcp_debug(_),_).	
ignored_predicate(user,meta_data(_,_,_,_,_,_,_,_,_),_).	
ignored_predicate(user,Head,TestFile):-	
	predicate_property(Head,file(File)),
	file_directory_name(TestFile,Dir),
	\+ atom_concat(Dir,_,File). 		
	
ignored_predicate(Module,Head,_):-
	Module:predicate_property(Head,built_in).
	
relevant_property((dynamic),(dynamic)).
relevant_property((transparent),(module_transparent)).
relevant_property((multifile),(multifile)).
relevant_property((thread_local),(thread_local)).


reality_subset_pefs(TestFile,Result):-
    pdt_file_ref(TestFile,PID),
   	real_current_module(TestFile,Name,File),
    (	module_exists_in_program(Name,File,PID)
	->	Result=passed(module_exists_in_program(Name,File,PID))
	;	Result=failed(module_exists_in_program(Name,File,PID))
	).
reality_subset_pefs(TestFile,Result):-
    pdt_file_ref(TestFile,PID),
	real_current_predicate(TestFile,Context,Module,Name/Arity),
	(	predicate_exists_in_program(Context,Name/Arity,Module,PID)
	->	Result=passed(predicate_exists_in_program(Context,Name/Arity,Module,PID))
	;	Result=failed(predicate_exists_in_program(Context,Name/Arity,Module,PID))
	).

reality_subset_pefs(TestFile,Result):-
    pdt_file_ref(TestFile,PID),
	real_current_predicate(TestFile,_Context,Module,Name/Arity),
	clauses_exists_in_program(Module,Name/Arity,PID,Result).
     %TODO predicate properties.
    
module_exists_in_program(Name,[],PID):-
    !,
    pef_program_module_query([program=PID,name=Name,module=MID]),
    pef_ad_hoc_module_query([name=Name,id=MID]).
module_exists_in_program(Name,File,PID):-    
    pdt_file_ref(File,Ref),
    pef_program_module_query([program=PID,name=Name,module=MID]),
    pef_module_definition_query([file_ref=Ref,id=MID,name=Name]),
    !.
module_exists_in_program(Name,File,PID):-        
    pdt_file_ref(File,Ref),
    pef_program_module_query([program=PID,name=Name,module=MID]),
    pef_module_extension_query([base=Base,id=MID]),
    pef_module_definition_query([file_ref=Ref,id=Base,name=Name]).
    
predicate_exists_in_program(Context,Name/Arity,Module,PID):-
    resolve_module(PID,Context,CxMID),
   	resolve_predicate(PID,CxMID,Name,Arity,PredID),
    pef_predicate_query([id=PredID,module=DefMID]),
    module_name(DefMID,Module).
    
clauses_exists_in_program(Module,Name/Arity,PID,Result):-
    resolve_module(PID,Module,MID),
    resolve_predicate(PID,MID,Name,Arity,PredID),
    functor(Head,Name,Arity),

	Module:nth_clause(Head,N,ClauseRef),
	(	clause_exists_in_program(ClauseRef,N,PredID)
	->	Result=passed(clause_exists_in_program(ClauseRef,N,PredID))
	;	Result=failed(clause_exists_in_program(ClauseRef,N,PredID))
	).

    	
clause_exists_in_program(ClauseRef,N,PredID):-
    clause(Head,Body,ClauseRef),
	normalize_clause(Head:-Body,RealClause),
    pef_clause_query([predicate=PredID,number=N,toplevel_ref=TlRef]),
    pef_toplevel_recorded(_,[expanded=Exp],TlRef),
    normalize_clause(Exp,PefClause),
    RealClause =@= PefClause.


normalize_clause(Clause,Head:-Body):-
    strip_module(Clause,_,Striped),
    normalize_clause_X(Striped,Head:-Body).

normalize_clause_X(Head:-Body,StripedHead:-Body):-
    !,
    strip_module(Head,_,StripedHead).
normalize_clause_X(Fact,StripedFact:-true):-
    strip_module(Fact,_,StripedFact).

    

pefs_subset_reality(Abs,Result):- %modules
	pdt_file_ref(Abs,PID),
    pef_program_module_query([program=PID,name=MName,module=MID]),
    module_file(MID,FileRef),
    pdt_file_ref(ModFile,FileRef),
    (	real_current_module(Abs,MName,ModFile)
    ->	Result = passed(real_current_module(Abs,MName,ModFile))
    ;	Result = failed(real_current_module(Abs,MName,ModFile))
    ).
    
pefs_subset_reality(Abs,Result):- %visible predicates
   	pdt_file_ref(Abs,PID),
    pef_program_module_query([program=PID,name=DefMName,module=DefMID]),
    module_predicate(DefMID,PredID),
    pef_predicate_query([id=PredID,name=Name,arity=Arity]),
    pef_program_module_query([program=PID,module=ContextMID]),
	resolve_predicate(PID,ContextMID,Name,Arity,PredID),
	(	real_current_predicate(Abs,ContextMName,DefMName,Name/Arity)
	->	Result = passed(real_current_predicate(Abs,ContextMName,DefMName,Name/Arity))
	;	Result = failed(real_current_predicate(Abs,ContextMName,DefMName,Name/Arity))
	).
	
pefs_subset_reality(Abs,Result):- %clauses
   	pdt_file_ref(Abs,PID),
    pef_program_module_query([program=PID,name=DefMName,module=DefMID]),
    module_predicate(DefMID,PredID),
    pef_predicate_query([id=PredID,name=Name,arity=Arity]),
	pef_clause_query([predicate=PredID,number=Num,toplevel_ref=TlRef]),
	pef_toplevel_recorded(_,[expanded=PefTerm],TlRef),
	normalize_clause(PefTerm,NormPefTerm),
	functor(Head,Name,Arity),
	(	real_clause(DefMName,Head,Num,NormPefTerm)
	->	Result = passed(real_clause(DefMName,Head,Num,NormPefTerm))
	;	Result = failed(real_clause(DefMName,Head,Num,NormPefTerm))
	).

real_clause(DefMName,Head,N,PefTerm):-
    DefMName:nth_clause(Head,N,ClauseRef),
    clause(RealHead,RealBody,ClauseRef),
    normalize_clause((RealHead:-RealBody),RealTerm),
    RealTerm =@= PefTerm.

user:prolog_exception_hook(error(instantiation_error, context(system:'$concat_atom'/2, _)),
                               _, _, _) :-
            writeln(arsch),trace, fail.
