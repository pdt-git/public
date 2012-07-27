:- module(graphML_api,[ prepare_for_writing/2,
						finish_writing/1,
						write_file/6,
						write_call_edge/3,
						write_load_edge/3]).

:- ensure_loaded('../pdt_factbase.pl').
:- use_module('../analyzer/edge_counter').
:- use_module('../analyzer/dead_predicate_finder').
:- use_module('../modules_and_visibility').

prepare_for_writing(File,OutStream):-
    open(File,write,OutStream,[type(text)]),
    write_graphML_header(OutStream),
    write_graphML_ast_keys(OutStream),
    start_graph_element(OutStream),
    flush_output(OutStream).
  
finish_writing(OutStream):-
    close_graph_element(OutStream),
    write_graphML_footer(OutStream),
    close(OutStream).
    
    
write_file(Stream,RelativePath, Predicates, Id,FileName,Module):-
	open_node(Stream,Id),
	write_data(Stream,'id',Id),
	(	catch(	(	atom_concat(RelativePath,RelativeWithSlash,FileName),
					atom_concat('/',RelativeFileName,RelativeWithSlash), !
				),
				_, 
				fail
			)
	;	RelativeFileName=FileName
	),
	write_data(Stream,'fileName',RelativeFileName),
	write_data(Stream,'module',Module),	
	(	Module=user
	->	write_data(Stream,'kind','file')
	;	write_data(Stream,'kind','module')
	),
	start_graph_element(Stream),
	write_predicates(Stream, Id, Predicates),
	close_graph_element(Stream),
	close_node(Stream).	

		
write_predicates(Stream, FileId, all_preds):-
    !,
	forall(	predicateT(Id,FileId,Functor,Arity,Module),
			(	write_predicate(Stream,Id,Functor,Arity,Module),
				flush_output(Stream)
			)
	).	
write_predicates(Stream, FileId, PredicatesToWrite):-
	forall(	(	member(Id,PredicatesToWrite),
				predicateT(Id,FileId,Functor,Arity,Module)
			),
			(
				write_predicate(Stream,Id,Functor,Arity,Module),
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
    
    
    
write_predicate(Stream,Id,Functor,Arity,Module):-
    open_node(Stream,Id),
    write_data(Stream,'kind','predicate'),
    write_data(Stream,'id',Id),
	write_data(Stream,'functor',Functor),
	write_data(Stream,'arity',Arity),	
	write_data(Stream,'moduleOfPredicate',Module),	
	(	dynamicT(Id,_)
	->	write_data(Stream,'isDynamic','true')
	;	true
	),
	(	transparentT(Id,_)
	->	write_data(Stream,'isTransparent','true')
	;	true
	),	
	(	multifileT(Id,_)
	->	write_data(Stream,'isMultifile','true')
	;	true
	),		
	(	meta_predT(Id,_)	
	->	write_data(Stream,'isMetaPredicate','true')
	;	true
	),	
	(	exporting(_,Id,_)
	->	write_data(Stream,'isExported','true')
	;	true
	),	
	(	locally_dead_predicate(Id)
	->	write_data(Stream,'isUnusedLocal','true')
	;	true
	),	
	
/*	start_graph_element(Stream),
	write_clauses(Stream,FileName),
	close_graph_element(Stream),
*/	close_node(Stream).	

    
write_load_edge(Stream,LoadingFileId,FileId):-
    open_edge(Stream,LoadingFileId,FileId),
    write_data(Stream,'kind','loading'),
	close_edge(Stream).
    
write_call_edge(Stream,SourceId,TargetId):-
    open_edge(Stream,SourceId,TargetId),
    write_data(Stream,'kind','call'),
    call_edges_for_predicates(SourceId,TargetId,Frequency),
    write_data(Stream,'frequency',Frequency),
	close_edge(Stream).
    
    

write_graphML_header(OutStream):-
	write(OutStream,'<?xml version="1.0" encoding="UTF-8"?>'), nl(OutStream),
	write(OutStream,'<graphml xmlns="http://graphml.graphdrawing.org/xmlns"  
      xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">'), 
	nl(OutStream).
	
write_graphML_ast_keys(OutStream):-
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
    write(OutStream, '<key id="isMetaPredicate" for="node" attr.name="isMetaPredicate" attr.type="boolean">'),
    nl(OutStream),
    write(OutStream, '    <default>false</default>'),
  	nl(OutStream),
  	write(OutStream, '</key>'),
    nl(OutStream),
    write(OutStream, '<key id="isUnusedLocal" for="node" attr.name="isUnusedLocal" attr.type="boolean">'),
    nl(OutStream),
    write(OutStream, '    <default>false</default>'),
  	nl(OutStream),  	
  	write(OutStream, '</key>'),
    nl(OutStream),
    write(OutStream, '<key id="isExported" for="node" attr.name="isExported" attr.type="boolean">'),
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
	


