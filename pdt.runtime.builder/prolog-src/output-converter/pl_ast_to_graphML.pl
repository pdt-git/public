:- module(pl_ast_to_graphML, [	write_facts_to_graphML/2,
								write_focus_facts_to_graphML/2,
								pl_test_graph/0,
								pl_test_graph/2]).

:- use_module(graphML_api).

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
    write_files(FirstProject,OutStream),
    flush_output(OutStream),
  	write_load_edges(OutStream),
  	flush_output(OutStream),
  	write_call_edges(OutStream),
  	flush_output(OutStream),
 	finish_writing(OutStream).


write_focus_facts_to_graphML(FocusFile, File):-
    prepare_for_writing(File,OutStream),
	fileT(Id,FocusFile,Module),
	write_file(OutStream,File,Id,File,Module),	
	
	count_call_edges_between_predicates,
	collect_ids_for_focus_file(FocusFile,Files,CalledPredicates,Calls),
	
    write_files(FocusFile,OutStream),
    
    finish_writing(OutStream).  
    
    
collect_ids_for_focus_file(FocusFile,Files,CalledPredicates,Calls):-
    findall(
    	PredId,
    	predicateT(PredId,FocusFile,_,_,_),
    	OwnPredicates
    ),
    collect_calls_to_predicates(OwnPredicates,[],IncomingCalls),
    list_to_set(IncomingCalls,IncomingCallsSet),
    collect_calling_predicates_and_files(IncomingCallsSet,[],CallingPreds,[],CallingFiles),
    collect_calls_from_predicates(OwnPredicates,[],OutgoingCalls),
    list_to_set(OutgoingCalls, OutgoingCallsSet),
    collect_called_predicates_and_files(OutgoingCallsSet,CallingPreds,AllPreds,CallingFiles,AllFiles),
    list_to_set(AllPreds, CalledPredicates),
    list_to_set(AllFiles, Files),
    append(IncomingCallsSet, OutgoingCallsSet, CallsList),
    list_to_set(CallsList, Calls).
    
collect_calls_to_predicates([],KnownCalls,KnownCalls).
collect_calls_to_predicates([Predicate|OtherPredicates],KnownCalls,AllCalls):-
    findall( (Source,Predicate),
    	call_edges_for_predicates(Source,Predicate,_Counter),
    	FoundCalls
    ),
    collect_calls_to_predicats(OtherPredicates,[FoundCalls|KnownCalls],AllCalls).
    
collect_calls_from_predicates([],KnownCalls,KnownCalls).
collect_calls_from_predicates([Predicate|OtherPredicates],KnownCalls,AllCalls):-
    findall( (Predicate,Target),
    	call_edges_for_predicates(Predicate,Target,_Counter),
    	FoundCalls
    ),
    collect_calls_from_predicats(OtherPredicates,[FoundCalls|KnownCalls],AllCalls).   
    
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
write_files(Project,Stream):-
    forall(	fileT(Id,File,Module),
    		(	write_file(Stream,Project,Id,File,Module),
    			flush_output(Stream)
    		)
    	  ).
		
write_file(Stream,Project,Id,FileName,Module):-
	open_node(Stream,Id),
	write_data(Stream,'id',Id),
	catch(	(	atom_concat(Project,RelativeWithSlash,FileName),
				atom_concat('/',RelativeFileName,RelativeWithSlash)
			),
			_, 
			RelativeFileName=FileName
		),
	write_data(Stream,'fileName',RelativeFileName),
	write_data(Stream,'module',Module),	
	(	Module=user
	->	write_data(Stream,'kind','file')
	;	write_data(Stream,'kind','module')
	),
	start_graph_element(Stream),
	write_predicates(Stream,Id),
	close_graph_element(Stream),
	close_node(Stream).	
		
write_predicates(Stream,FileId):-
	forall(	predicateT(Id,FileId,Functor,Arity,Module),
			(	write_predicate(Stream,Id,Functor,Arity,Module),
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
pl_test_graph(Project,Output):-
	plparser_quick:generate_facts(Project),
	writeln('generating graphml-file'),
    time(write_facts_to_graphML(Project,Output)).
    