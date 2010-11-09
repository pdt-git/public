:- use_module('../prolog_file_reader_quick').
:- use_module('../analyzer/edge_counter').

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
    open(File,write,OutStream,[type(text)]),
    write_graphML_header(OutStream),
    write_graphML_keys(OutStream),
    start_graph_element(OutStream),
    flush_output(OutStream),
    Project=[FirstProject],
    write_files(FirstProject,OutStream),
    flush_output(OutStream),
  	write_load_edges(OutStream),
  	flush_output(OutStream),
  	write_call_edges(OutStream),
  	flush_output(OutStream),
 	close_graph_element(OutStream),
    write_graphML_footer(OutStream),
    close(OutStream).
    

write_graphML_header(OutStream):-
	write(OutStream,'<?xml version="1.0" encoding="UTF-8"?>'), nl(OutStream),
	write(OutStream,'<graphml xmlns="http://graphml.graphdrawing.org/xmlns"  
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">'), 
	nl(OutStream).
	
	
write_graphML_keys(OutStream):-
    write(OutStream, '<key id="id" for="node" attr.name="id" attr.type="string"/>'),
    nl(OutStream),
    write(OutStream, '<key id="kind" for="all" attr.name="kind" attr.type="string"/>'),
    nl(OutStream),
    write(OutStream, '<key id="fileName" for="node" attr.name="description" attr.type="string"/>'),
    nl(OutStream),
    write(OutStream, '<key id="module" for="node" attr.name="module" attr.type="string">'),
    nl(OutStream),
  	write(OutStream, '    <default>user</default>'),
  	nl(OutStream),
  	write(OutStream, '</key>'),
    nl(OutStream),
    write(OutStream, '<key id="functor" for="node" attr.name="functor" attr.type="string"/>'),
    nl(OutStream),
    write(OutStream, '<key id="arity" for="node" attr.name="arity" attr.type="int"/>'),
    nl(OutStream),
    write(OutStream, '<key id="moduleOfPredicate" for="node" attr.name="moduleOfPredicate" attr.type="string"/>'),
    nl(OutStream),
    write(OutStream, '<key id="isTransparent" for="node" attr.name="isTransparent" attr.type="boolean">'),
    nl(OutStream),
    write(OutStream, '    <default>false</default>'),
  	nl(OutStream),
  	write(OutStream, '</key>'),
    nl(OutStream),
    write(OutStream, '<key id="isDynamic" for="node" attr.name="isDynamic" attr.type="boolean">'),
    nl(OutStream),
    write(OutStream, '    <default>false</default>'),
  	nl(OutStream),
  	write(OutStream, '</key>'),
    nl(OutStream),   
    write(OutStream, '<key id="isMultifile" for="node" attr.name="isMultifile" attr.type="boolean">'),
    nl(OutStream),
    write(OutStream, '    <default>false</default>'),
  	nl(OutStream),
  	write(OutStream, '</key>'),
    nl(OutStream),
    write(OutStream, '<key id="frequency" for="edge" attr.name="frequency" attr.type="int">'),
    nl(OutStream),
    write(OutStream, '    <default>1</default>'),
  	nl(OutStream),
  	write(OutStream, '</key>'),
    nl(OutStream),
    nl(OutStream),
    nl(OutStream).
    

write_graphML_footer(OutStream):-
    write(OutStream,'</graphml>').
    
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
		
write_predicate(Stream,Id,Functor,Arity,Module):-
    open_node(Stream,Id),
    write_data(Stream,'kind','predicate'),
    write_data(Stream,'id',Id),
	write_data(Stream,'functor',Functor),
	write_data(Stream,'arity',Arity),	
	write_data(Stream,'moduleOfPredicate',Module),	
	(	dynamicT(_,Functor,Arity,Module,_)
	->	write_data(Stream,'isDynamic','true')
	;	true
	),
	(	transparentT(_,Functor,Arity,Module,_)
	->	write_data(Stream,'isTransparent','true')
	;	true
	),	
	(	multifileT(_,Functor,Arity,Module,_)
	->	write_data(Stream,'isMultifile','true')
	;	true
	),		
/*	start_graph_element(Stream),
	write_clauses(Stream,FileName),
	close_graph_element(Stream),
*/	close_node(Stream).	

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

write_load_edge(Stream,LoadingFileId,FileId):-
    open_edge(Stream,LoadingFileId,FileId),
    write_data(Stream,'kind','loading'),
	close_edge(Stream).
	

write_call_edges(Stream):-
    count_call_edges_between_predicates,
    forall(call_edges_for_predicates(SourceId,TargetId,_Counter),
    	(	write_call_edge(Stream,SourceId,TargetId)
    	)
    ).
    
	
write_call_edge(Stream,SourceId,TargetId):-
    open_edge(Stream,SourceId,TargetId),
    write_data(Stream,'kind','call'),
    call_edges_for_predicates(SourceId,TargetId,Frequency),
    write_data(Stream,'frequency',Frequency),
	close_edge(Stream).
		
	
/*write_onloades(Stream):-
	forall(	onloadT(Id,_,Module),
			(	write_node(Stream,Id,prolog_onload,Module),
				slT(Id,Begin,Length),
				write_position(Stream,Id,Begin,Length)
			)
		).
    
   	  
write_clauses(Stream):-
	forall(	headT(HeadId,Id,_,_,_,_),     
			(	termT(HeadId,Term),
				write_node(Stream,Id,prolog_clause,Term),
				slT(Id,Begin,Length),
				write_position(Stream,Id,Begin,Length)
			)
    	  ).      	  

write_directives(Stream):-
	forall(	directiveT(Id,_,_),
			(	termT(Id,Term),
				write_node(Stream,Id,prolog_directive,Term),
				slT(Id,Begin,Length),
				write_position(Stream,Id,Begin,Length)
			)
    	  ).  
 
write_hierarchy(Stream):-
    forall( onloadT(PredId,FileId,_),
    		write_within(Stream,FileId,PredId,'parent-child')
    	),
    forall(	predicateT(PredId,FileId,_,_,_),
 			write_within(Stream,FileId,PredId,'parent-child')
    	),
    forall( onload_edge(DirectId,OnloadId),
    		write_within(Stream,OnloadId,DirectId,'parent-child')
    	),
    forall(	pred_edge(ClauseId,Id),
    		write_within(Stream,Id,ClauseId,'parent-child')
    	).*/
 
    	
/*write_edges(Stream):-
    call_edge(LId,CalleeId),
    literalT(LId,_,CallerId,_,_,_),
    termT(LId,Term),
    	write_edge(Stream,LId,CalleeId,CallerId,Term),
    fail.
write_edges(_).*/		

    
start_graph_element(OutStream):-
    write(OutStream,'<graph edgedefault="directed">'), 
    nl(OutStream).

close_graph_element(OutStream):-
    write(OutStream,'</graph>'), 
    nl(OutStream).
    
open_node(Stream,Id):-
    format(Stream, '<node id="~w">~n', [Id]).

close_node(Stream):-
    write(Stream, '</node>'),
    nl(Stream).
   
open_edge(Stream,Source,Target):-
    format(Stream, '<edge source="~w" target="~w">~n', [Source, Target]). 
	
close_edge(Stream):-
    write(Stream, '</edge>'),
    nl(Stream).

write_data(Stream,Key,Value):-
	format(Stream, '   <data key="~w">~w</data>~n', [Key,Value]).	
	


pl_test:-
    pl_test(['Z:/pdt.git/pdt.runtime.ui/library'],'Z:/WorkspaceTeaching3/test6.graphml').           
   % pl_test(['Z:/pdt.git/pdt.runtime.builder/prolog-src'],'Z:/WorkspaceTeaching3/test5.graphml').
 
pl_test(Project,Output):-
	plparser_quick:generate_facts(Project),
	writeln('generating graphml-file'),
    time(write_facts_to_graphML(Project,Output)).
    