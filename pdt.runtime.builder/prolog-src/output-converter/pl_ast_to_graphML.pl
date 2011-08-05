:- module(pl_ast_to_graphML, [	write_project_graph_to_file/2,
								write_focus_to_graphML/2,
								pl_test_graph/0,
								pl_test_graph/2]).

:- use_module('../parse_util.pl').
:- use_module(graphML_api).
:- use_module('../analyzer/edge_counter').

write_project_graph_to_file(Project, OutputFile):-
	plparser_quick:generate_facts(Project),
	writeln('generating graphml-file'),
    time(write_facts_to_graphML(Project,OutputFile)).

/**
 * write_facts_to_graphML(+Project,+File)
 *   Arg1 has to be a full qualified file name. The file may not exist.
 *   The predicated collects the relevant informations about the following 
 *   facts and converts them into abba-sources for Bashaars Tool. This 
 *   sources are written into the file specified by arg1-
 *   The facts that are considered are:
 *   ###### to be completed #########
 **/
write_facts_to_graphML(Project, File):-
    prepare_for_writing(File,OutStream),
    member(FirstProject,Project),
    write_all_files(FirstProject,OutStream),
    flush_output(OutStream),
  	write_load_edges(OutStream),
  	flush_output(OutStream),
  	write_call_edges(OutStream),
  	flush_output(OutStream),
 	finish_writing(OutStream).


write_focus_to_graphML(FocusFile, File):-
    with_mutex(prolog_factbase,
    	with_mutex(meta_pred_finder,
    		setup_call_cleanup(
    			prepare_for_writing(File,OutStream),
				write_focus_facts_to_graphML(FocusFile, OutStream),
  	  			finish_writing(OutStream)
  	  		)
  	 	)
  	 ).  
  	 
write_focus_facts_to_graphML(FocusFile, OutStream):-
    fileT_ri(FocusFile,FocusId), !,
%	fileT(FocusId,FocusFile,Module),
%	write_file(OutStream,FocusFile,FocusId,FocusFile,Module),	
	
	count_call_edges_between_predicates,
	collect_ids_for_focus_file(FocusId,Files, CorrespondingPredicates,Calls),
	
   	write_files(FocusFile, Files, CorrespondingPredicates, OutStream),
    forall(
    	member((SourceId,TargetId),Calls),
    	write_call_edge(OutStream,SourceId,TargetId)
    ).
    
collect_ids_for_focus_file(FocusId,Files,CalledPredicates,Calls):-
    findall(
    	PredId,
    	predicateT(PredId,FocusId,_,_,_),
    	OwnPredicates
    ),
    collect_calls_to_predicates(OwnPredicates,[],IncomingCalls),
    collect_calling_predicates_and_files(IncomingCalls,OwnPredicates,CallingPreds,[FocusId],CallingFiles),
    collect_calls_from_predicates(OwnPredicates,[],OutgoingCalls),
    collect_called_predicates_and_files(OutgoingCalls,CallingPreds,AllPreds,CallingFiles,AllFiles),
    list_to_set(AllPreds, CalledPredicates),
    list_to_set(AllFiles, Files),
    append(IncomingCalls, OutgoingCalls, CallsList),
    list_to_set(CallsList, Calls).
    
collect_calls_to_predicates([],KnownCalls,KnownCalls).
collect_calls_to_predicates([Predicate|OtherPredicates],KnownCalls,AllCalls):-
    findall( (Source,Predicate),
    	call_edges_for_predicates(Source,Predicate,_Counter),
    	FoundCalls
    ),
    (	FoundCalls \= []
   	->	(	append(FoundCalls,KnownCalls,CallList), 
   			list_to_set(CallList,CallSet)
   		)
   	;	CallSet = KnownCalls
   	),
    collect_calls_to_predicates(OtherPredicates,CallSet,AllCalls).
    
collect_calls_from_predicates([],KnownCalls,KnownCalls).
collect_calls_from_predicates([Predicate|OtherPredicates],KnownCalls,AllCalls):-
    findall( (Predicate,Target),
    	call_edges_for_predicates(Predicate,Target,_Counter),
    	FoundCalls
    ),
	(	FoundCalls \= []
   	->	(	append(FoundCalls,KnownCalls,CallList), 
   			list_to_set(CallList,CallSet)
   		)
   	;	CallSet = KnownCalls
   	),
    collect_calls_from_predicates(OtherPredicates,CallSet,AllCalls).   
    
collect_calling_predicates_and_files([],Preds,Preds,Files,Files).    
collect_calling_predicates_and_files([(SourcePred,_CalledPred)|OtherCalls],KnownCalledPreds, CalledPreds,KnownCalledFiles,CalledFiles):-
    predicateT(SourcePred,SourceFile,_,_,_),
    collect_calling_predicates_and_files(OtherCalls,[SourcePred|KnownCalledPreds],CalledPreds,[SourceFile|KnownCalledFiles],CalledFiles). 
 
    
collect_called_predicates_and_files([],Preds,Preds,Files,Files).    
collect_called_predicates_and_files([(_Caller,TargetPred)|OtherCalls],KnownCalledPreds, CalledPreds,KnownCalledFiles,CalledFiles):-
    predicateT(TargetPred,TargetFile,_,_,_),
    collect_called_predicates_and_files(OtherCalls,[TargetPred|KnownCalledPreds],CalledPreds,[TargetFile|KnownCalledFiles],CalledFiles).
    
/**
 * write_files(+Stream)
 *    writes #### dito ####
 */
write_all_files(RelativePath,Stream):-
    forall(	fileT(Id,File,Module),
    		(	write_file(Stream,RelativePath,all_preds,Id,File,Module),
    			flush_output(Stream)
    		)
    	  ).
		

write_files(RelativePath, Files, PredicatesToWrite, Stream):-
	forall(	
		member(FileId,Files),
		(	fileT(FileId,FileName,Module),
			write_file(Stream,RelativePath,PredicatesToWrite,FileId,FileName,Module),
    		flush_output(Stream)
    	)
    ).	
    

write_load_edges(Stream):-
    forall(load_edge(LoadingFileId,FileId,_,_),
    	(	(	fileT(LoadingFileId,_,_),
    			fileT(FileId,_,_)
    		)
    	->	write_load_edge(Stream,LoadingFileId,FileId)
    		%format(Stream,'<edge source="~w" target="~w"/>~n', [LoadingFileId, FileId])
    	;	format('Problem with load-edge: ~w, ~w~n',[LoadingFileId, FileId])
	    )
	).


	

write_call_edges(Stream):-
    count_call_edges_between_predicates,
    forall(call_edges_for_predicates(SourceId,TargetId,_Counter),
    	(	write_call_edge(Stream,SourceId,TargetId)
    	)
    ).
    

pl_test_graph:-	
    pl_test_graph(['Z:/Git-Data/pdt.git/pdt.runtime.builder/prolog-src'],'Z:/Workspaces/WorkspaceFresh/test6.graphml'). 
pl_test_graph(Project, OutputFile):-
	write_project_graph_to_file(Project, OutputFile).
    